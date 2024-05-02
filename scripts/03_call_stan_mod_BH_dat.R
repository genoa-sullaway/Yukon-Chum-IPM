library(rstan)
library(tidyverse)
library(here)
library(bayesplot)
library(rstanarm) 

library(tidync)
library(lubridate) 
library(readxl)

# Things to look into  =======================================================
# sigma for both lifestages in estiamtes so I can fix it instead of have model estimate it 


# Load data =======================================================
# see 01_make salmon data.R for salmon data tidying 
# see 02_make covariates for data tidying 
# setup inputs ==============================================================
warmups <- 2000
total_iterations <- 4000
max_treedepth <-  15
n_chains <- 1
n_cores <- 4
adapt_delta <- 0.95

year_min = 2002
year_max_cal = 2022
year_max_brood = 2017

# load salmon data ==============================================
## Fall age comp ================================================
yukon_fall_obs_agecomp <- read_csv("data/processed_data/yukon_fall_age_comp.csv") %>%
  filter(cal_year >= year_min, 
         cal_year <= year_max_cal) %>%
  dplyr::select(2:ncol(.)) %>%
  as.matrix()

## Spawners, Recruits, Harvest ==================================== 
yukon_fall_spawners <-read_csv("data/processed_data/yukon_fall_spawners.csv") %>%
  filter(cal_year >= year_min, 
         cal_year <= year_max_cal)%>%
  select(2) %>%
  as.vector()

yukon_fall_harvest<-read_csv("data/processed_data/yukon_fall_harvest.csv") %>%
  filter(cal_year >= year_min, 
         cal_year <= year_max_cal)%>%
  select(2) %>%
  as.vector()

yukon_fall_recruits<-read_csv("data/processed_data/yukon_fall_recruits.csv") %>%
  filter(cal_year >= year_min, 
         cal_year <= year_max_cal) %>%
  select(2) %>%
  as.vector()

## Fall Juveniles ================================================
fall_juv <- read_csv("data/processed_data/tidy_juv_fall_yukon.csv")  %>%
  select(2) %>%
  as.vector()

## Summer ================================================
# summer_age_comp<-read_csv("data/age_comps/processed_age_comps_summer_yukon.csv")  %>% 
#   filter(!cal_year < 2005 )
# summer_brood <- read_csv("output/yukon_summer_broodyear.csv")%>%
#   filter(!brood_year < 2002) # for now to simplify matching with juveniles
# yukon_summer <- read_excel("data/Yukon_Escapement_ADFG/S Chum RR 2023.xlsx", sheet = 2) %>%
#   dplyr::select(1,11:14) %>% 
#   janitor::row_to_names(row_number = 1) %>%
#   dplyr::rename(cal_year = "Year")  %>%
#   dplyr::mutate(age3=as.numeric(age3),
#                 age4=as.numeric(age4),
#                 age5=as.numeric(age5),
#                 age6=as.numeric(age6)) %>% 
#   filter(!cal_year < 2005)
# ## harvest below weir 
# harvest_escapement <- read_excel("data/Yukon_Escapement_ADFG/S Chum RR 2023.xlsx", sheet = 2) %>%
#   dplyr::select(1:2,4) %>%  
#   janitor::row_to_names(row_number = 1)  %>% 
#   dplyr::rename(cal_year = "Year") %>% 
#   dplyr::mutate(cal_year = as.numeric(cal_year), 
#          Harvest = as.numeric(Harvest), 
#          Escapement = as.numeric(Escapement)) %>% 
#   filter(!cal_year < 2005) %>% # from brood year 2002 (first year of juvenile data), the first year that fish could return is 2005 if its a 3yo, the last yera it coudl return is 2007 if its a 6yo. 
#   as.data.frame()  #%>%  
#  #as.matrix()

#  covariates =================  
stage_a_cov <- read_csv("data/processed_covariates/stage_a_all.csv") %>%
  filter(Year >= year_min, 
         Year <= year_max_cal) %>%
  dplyr::select(yukon_mean_discharge,SST_CDD_NBS) %>% 
  as.matrix()

stage_b_cov <- read_csv("data/processed_covariates/stage_b_all.csv") %>%
  filter(Year >= year_min, 
         Year <= year_max_cal) %>% 
  dplyr::select(SST_CDD_SEBS) %>% 
  as.matrix()

# Organize data call inputs ================================================
nByrs = length(unique(fall_juv$Year)) # Number of BROOD years                 #- 1978-2022 calendar years 
nRyrs = length(unique(yukon_fall_harvest$cal_year)) # Number of CAL/RETURN                    #years - 1978-2022 calendar years 
A = 4 # number of age classes, 3,4,5,6
K = 1 # number of stocks 
Ps = 0.5 # proportion of females - assumption, need to lit check
fs = as.matrix(c(1800, 2000, 2200, 2440)) # fecundity - Gilk-Baumer 2009 estimate for Kusko Chum is: 2440. I added extra numbers temporarily just so that younger fish reproduce less, but will have to look up data for this more...
t_start = 5 # to fill startgin values 

# fix sigma until RR data or until its running and I can try to estimate it better 
  sigma_y_j = 1 # rnorm(1,0.5)
  sigma_y_sp = 1 #rnorm(1,0.5)
 
# covariates ===========
# number covariates for each life stage 
ncovars1 = 1
ncovars2 = 1

# mean productivity rate =====
# I think this will actually need to be estimated... but will fix it for now. 
basal_p_1 = 0.2#,0.05,
              #0.05) # straight from simulation
basal_p_2 = 0.4#,
              # 0.15,
              # 0.15) # straight from simulation
# fix marine mortality =======
M_fill_stan = c(0.06, 0.06, 0.06) # will be cumulative 
# M = matrix(ncol = A, nrow = nRyrs, 
#            c(NA,0.06, 0.06, 0.06) , byrow = TRUE)

#ess age comp =======
ess_age_comp = rep(300, times = nByrs)

# STAN STARTING VALUES ==========
kappa_j_start =  runif(1, 0.2, 0.2)
kappa_marine_start = runif(1, 0.4, 0.4)

N_j_start =  as.vector(NA)
N_e_sum_start = as.vector(NA)

N_recruit_start = matrix(NA,nrow=t_start, ncol=A)
N_egg_start = matrix(NA,nrow=t_start, ncol=A)
N_ocean_start = matrix(NA,nrow=t_start, ncol=A)#vector() # ages # array(data = NA, dim = c(1, A))
N_sp_start = matrix(NA,nrow=t_start, ncol=A)#vector() # array(data = NA, dim = c(1, A,K))

N_j_start = exp(rnorm(1,20,2))
N_e_sum_start = exp(rnorm(1,30,2))

for(t in 1:t_start){
  N_recruit_start[t,] = exp(rnorm(1,14,2))*p
  N_ocean_start[t,] = exp(rnorm(1,19.5,2))*p
  N_sp_start[t,] = exp(rnorm(1,18,2))*p 
  N_egg_start[t,] = exp(rnorm(1,40,2))*p
}

# ASSIGN DATA ==========
 
data_list_stan <- list(nByrs=nByrs,
                                        nRyrs=nRyrs,
                                        A=A,
                                        t_start = t_start,
                                        Ps=Ps,
                                        fs=fs,
                                        data_stage_j = fall_juv, 
                                        data_stage_return = yukon_fall_recruits,
                                        data_stage_sp = yukon_fall_spawners,
                                        N_j_start =  N_j_start,
                                        N_recruit_start = N_recruit_start,
                                        N_ocean_start = N_ocean_start,
                                        N_e_sum_start = N_e_sum_start,
                                        N_egg_start = N_egg_start,
                                        N_sp_start = N_sp_start,
                                        sigma_y_j=process_error_j,
                                        sigma_y_r=process_error_r,
                                        sigma_y_sp=process_error_sp,
                                        kappa_marine_start = basal_p_2,
                                        kappa_j_start = basal_p_1,
                                        basal_p_1=basal_p_1,
                                        basal_p_2=basal_p_2,
                                        cov1=stage_a_cov,
                                        cov2=stage_b_cov,
                                        ncovars1=ncovars1,
                                        ncovars2=ncovars2,
                                        M = M_fill_stan,
                                        o_run_comp=o_run_comp,
                                        ess_age_comp=ess_age_comp)
                 

# call mod  ===========================

bh_fit <- stan(
  file = here::here("scripts", "stan_mod_BH_DATA.stan"),
  data = data_list,
  chains = n_chains,
  warmup = warmups,
  iter = total_iterations,
  cores = n_cores)

write_rds(bh_fit, "output/stan_fit_statespace.RDS")













# OLD ===========================


# 
# # weir passage data [escapement]
# data_w=as.numeric(harvest_escapement$Escapement) 
# # harvest below passage data
# data_h_b = as.numeric(harvest_escapement$Harvest)
# # harvest above passage data
# data_h_a = c(rep(0, times = Y ))

# below values copied from Fleishman data for now... need to look into that more ============ 
# Coefficient of variation for harvest below weir
#  cv_hb = c(0.89,0.76,0.65,0.74,0.89,0.79,0.70,0.70,0.69,0.69,
#            0.08,0.17,0.72,0.72,0.50,0.46,0.16,0.06,0.05,0.08,
#            0.10,0.09,0.19,0.12,0.15,0.07,0.16,0.09,0.07,0.25,
#            0.11,0.24,0.05,0.9,0.9,0.9,0.09,0.07,0.25,0.11,
#            0.24,0.05,0.9,0.9,0.9)
#  
#  # Coefficient of variation for weir passage
#  cv_w  = c(0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05, 
#            0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,
#            0.05,0.05,0.19,0.05,0.05,0.05,0.05,0.05,0.05,0.05,
#            0.05,0.05,0.05,0.05,0.05,0.05, 0.05,0.05,0.05,0.05,
#            0.05,0.05,0.05,0.05,0.05,0.05,0.05)
# 
#  # Coefficient of variation for harvest above weir
#  cv_ha= c(0.90,0.76,0.75,0.74,0.89,0.79,0.70,0.70, 
#           0.70,0.70, 0.71,0.70,0.73,0.72,0.50,0.46, 
#           0.26,0.22,0.20,0.25, 0.29,0.25,0.27,0.25, 
#           0.23,0.28,0.05,0.05,0.05,0.60,0.62,0.50, 
#           0.9,0.9,0.9,0.9,0.05,0.05,0.60, 0.62,0.50,
#           0.9,0.9,0.9,0.9)
#  
# 
# data_list <- list(Ps = Ps,
#                   fs=fs,
#                   N = N, 
#                   K = K,  
#                   data_stage_j = data_stage_j, 
#                   data_stage_sp = data_stage_sp,
#                   kappa_sp_start = kappa_sp_start,
#                   kappa_j_start = kappa_j_start,
#                   basal_p_1=basal_p_1,
#                   basal_p_2=basal_p_2,
#                   cov1 = cov1,
#                   cov2 = cov2,
#                   ncovars1 = 1,
#                   ncovars2 = 2
# ) 
# 
# bh_fit <- stan(
#   file = here::here("scripts", "stan_mod_BH_DATA.stan"),
#   data = data_list,
#   chains = n_chains,
#   warmup = warmups,
#   iter = total_iterations,
#   cores = n_cores)
# 
# write_rds(bh_fit, "output/stan_fit_statespace.RDS")
# 
# mcmc_trace(bh_fit, pars = c("c_1[1]","c_1[2]","c_1[3]"))
# mcmc_trace(bh_fit, pars = c("c_2[1]","c_2[2]","c_2[3]"))
# mcmc_trace(bh_fit, pars = c("theta1[1,1]","theta1[2,1]","theta1[3,1]"))
# mcmc_trace(bh_fit, pars = c("theta2[1,1]","theta2[2,1]","theta2[3,1]"))
# mcmc_trace(bh_fit, pars = c("sigma_y_j[1]","sigma_y_j[2]","sigma_y_j[3]"))
# mcmc_trace(bh_fit, pars = c("sigma_y_sp[1]","sigma_y_sp[2]","sigma_y_sp[3]"))
# 
# 
# 
# 
# # OLD ======== 
# K = 3 # number of stocks involved
# N <- c(nrow(sp)) #, nrow(sim_dat$N_sp), nrow(sim_dat$N_sp))  
# 
# data_stage_j <- as.matrix(juv_prop_ayk[,2:4]) #c(as.integer(sim_dat$N_j))#, 
# # as.integer(sim_yukon_fall_df$N_j),
# # as.integer(sim_kusko_df$N_j))
# 
# data_stage_sp <-as.matrix(sp[,2:4])#c(as.integer(sim_dat$N_sp))#, 
# # as.integer(sim_yukon_fall_df$N_sp),
# # as.integer(sim_kusko_df$N_sp))
#  
# 
# # I think this will actually need to be estimated... 
# basal_p_1 = c(0.05,0.05,
#               0.05) # straight from simulation
# basal_p_2 = c(0.15,
#               0.15,
#               0.15) # straight from simulation
# 
# cov1 = as.matrix(as.numeric(zoop$scale))
# cov2 =  as.matrix(cbind(m2_cov, chum_cov$scale))
# 
# data_list <- list(Ps = Ps,
#                   fs=fs,
#                   N = N, 
#                   K = K,  
#                   data_stage_j = data_stage_j, 
#                   data_stage_sp = data_stage_sp,
#                   kappa_sp_start = kappa_sp_start,
#                   kappa_j_start = kappa_j_start,
#                   basal_p_1=basal_p_1,
#                   basal_p_2=basal_p_2,
#                   cov1 = cov1,
#                   cov2 = cov2,
#                   ncovars1 = 1,
#                   ncovars2 = 2
# ) 
# 
# bh_fit <- stan(
#   file = here::here("scripts", "stan_mod_BH_DATA.stan"),
#   data = data_list,
#   chains = n_chains,
#   warmup = warmups,
#   iter = total_iterations,
#   cores = n_cores)
# 
# write_rds(bh_fit, "output/stan_fit.RDS")
# 
# mcmc_trace(bh_fit, pars = c("c_1[1]","c_1[2]","c_1[3]"))
# mcmc_trace(bh_fit, pars = c("c_2[1]","c_2[2]","c_2[3]"))
# mcmc_trace(bh_fit, pars = c("theta1[1,1]","theta1[2,1]","theta1[3,1]"))
# mcmc_trace(bh_fit, pars = c("theta2[1,1]","theta2[2,1]","theta2[3,1]"))
# mcmc_trace(bh_fit, pars = c("sigma_y_j[1]","sigma_y_j[2]","sigma_y_j[3]"))
# mcmc_trace(bh_fit, pars = c("sigma_y_sp[1]","sigma_y_sp[2]","sigma_y_sp[3]"))
# 
# bh_summary <- summary(bh_fit)$summary %>% 
#   as.data.frame() %>% 
#   mutate(variable = rownames(.)) %>% 
#   select(variable, everything()) %>% 
#   as_data_frame()
# 
# bh_summary %>% 
#   slice(1:27) %>%
#   ggplot() + 
#   geom_linerange(aes(variable, ymin = `2.5%`,ymax = `97.5%`)) + 
#   geom_crossbar(aes(variable, mean, ymin = `25%`, ymax = `75%`), fill= 'grey') + 
#   facet_wrap(~variable, scales = 'free') # +
# # geom_point(data = obs_df, aes(variable, mean), color = "red" ) #observed
# 
# 
# 
