library(rstan)
library(tidyverse)
library(here)
library(bayesplot)
library(rstanarm) 

library(RNetCDF)
library(tidync)
library(lubridate) 

# Load data =======================================================
# Load salmon data
# made in 01_make_salmon_data_for_model.R

# currently this file ^^ makes two DFs one juve one spawner, I think I will want to put them into an array format?? 



# load Covariates  ==========================================================
# covariates for stage 1 =======================



# 
# # theta 1 -- Zooplankton 
# # this was created in "scripts/Explore_Zoop.R" 
# # currently this is a themisto and calanus large copepod sum mean abundance for Fall, not a true index. 
# # I also have a gelatinous zoop abundnce i could add in
# zoop_temp <- read_csv("data/covariate_large_zooplankton.csv") %>% 
#   dplyr::select(YEAR, mean) 
# 
# # this is currently BS just to get enough years... need to ask for an expanded dataset 
# insert <- data.frame(YEAR = c(2001,2020,2021,2022), 
#                      mean = rep(rnorm(4, mean(zoop_temp$mean), sd(zoop_temp$mean))))
# 
# zoop <- zoop_temp %>%
#   rbind(insert) %>% 
#   filter(!YEAR<2001) %>%
#   mutate(scale = scale(mean))
# 
# # covariates for stage 2 =======================
# # hatchery pink =============
# pink_cov <- read_csv("output/hatchery_Pink_Covariate_AKandAsia.csv")%>%
#   filter(Year>2001) %>%
#   mutate(scale = as.numeric(scale(sum)))
# 
# # hatchery chum ============
# chum_cov <- read_csv("output/hatchery_Chum_Covariate_AKandAsia.csv") %>%
#   filter(Year>2001) %>%
#   mutate(scale = as.numeric(scale(sum)))
# 
# # theta 2 -- M2 ============
# m2_cov<-read_csv("data/M2_df.csv") %>% # this was created in "MAPP/scripts_02/process_M2_degreedays.R" and the M2 file was copied to this datafile
#   dplyr::mutate(DOY = lubridate::yday(dates),
#                 Year = lubridate::year(dates)) %>% 
#   filter(!DOY>300) %>% 
#   group_by(Year) %>%
#   dplyr::summarise(degree_days = sum(temperature)) %>%
#   filter(Year>2001) %>%
#   dplyr::select(degree_days) %>%
#   mutate(degree_days = as.numeric(scale(degree_days))) # mean scale 

# setup inputs ==============================================================
warmups <- 2000
total_iterations <- 4000
max_treedepth <-  15
n_chains <- 1
n_cores <- 4
adapt_delta <- 0.95

Ps <- 0.5 # proportion of females - assumption, need to lit check
fs <- 2440 # fecundity - Gilk and Baumer 2009 estimate for Kusko Chum

# load data ================================================
summer_age_comp<-read_csv("data/age_comps/processed_age_comps_summer_yukon.csv")  


yukon_summer <- read_excel("data/Yukon_Escapement_ADFG/S Chum RR 2023.xlsx", sheet = 2) %>%
  dplyr::select(1,11:14) %>% 
  janitor::row_to_names(row_number = 1) %>%
  dplyr::rename(cal_year = "Year")  %>%
  dplyr::mutate(age3=as.numeric(age3),
                age4=as.numeric(age4),
                age5=as.numeric(age5),
                age6=as.numeric(age6)) %>% 
  filter(!cal_year == 2023)

## harvest below weir 
harvest_escapement <- read_excel("data/Yukon_Escapement_ADFG/S Chum RR 2023.xlsx", sheet = 2) %>%
  dplyr::select(1:4) %>% 
  janitor::row_to_names(row_number = 1)  %>%
  filter(!Year == 2023)
  
# Organize data call inputs ================================================

# test with one stock, yukon summer 

A = 4  # number of age classes
Y = 45 # Number of years - 1978-2022 calendar years 
C = A + Y # cohorts
a_min = 3 # Minimum age
a_max = 6 # Max age 
x = as.matrix(nrow = Y, ncol = A, yukon_summer[,2:5]) # Age counts by cal year

# weir passage data [escapement]
data_w=as.numeric(harvest_escapement$Escapement) 
# harvest below passage data
data_h_b = as.numeric(harvest_escapement$Harvest)
# harvest above passage data
data_h_a = c(rep(0, times = Y ))

# below values copied from Fleishman data for now... need to look into that more ============ 
# Coefficient of variation for harvest below weir
 cv_hb = c(0.89,0.76,0.65,0.74,0.89,0.79,0.70,0.70,0.69,0.69,
           0.08,0.17,0.72,0.72,0.50,0.46,0.16,0.06,0.05,0.08,
           0.10,0.09,0.19,0.12,0.15,0.07,0.16,0.09,0.07,0.25,
           0.11,0.24,0.05,0.9,0.9,0.9,0.09,0.07,0.25,0.11,
           0.24,0.05,0.9,0.9,0.9)
 
 # Coefficient of variation for weir passage
 cv_w  = c(0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05, 
           0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,
           0.05,0.05,0.19,0.05,0.05,0.05,0.05,0.05,0.05,0.05,
           0.05,0.05,0.05,0.05,0.05,0.05, 0.05,0.05,0.05,0.05,
           0.05,0.05,0.05,0.05,0.05,0.05,0.05)

 # Coefficient of variation for harvest above weir
 cv_ha= c(0.90,0.76,0.75,0.74,0.89,0.79,0.70,0.70, 
          0.70,0.70, 0.71,0.70,0.73,0.72,0.50,0.46, 
          0.26,0.22,0.20,0.25, 0.29,0.25,0.27,0.25, 
          0.23,0.28,0.05,0.05,0.05,0.60,0.62,0.50, 
          0.9,0.9,0.9,0.9,0.05,0.05,0.60, 0.62,0.50,
          0.9,0.9,0.9,0.9)

 
first_brood_year = 1972 # First brood year


data_list <- list(Ps = Ps,
                  fs=fs,
                  N = N, 
                  K = K,  
                  data_stage_j = data_stage_j, 
                  data_stage_sp = data_stage_sp,
                  kappa_sp_start = kappa_sp_start,
                  kappa_j_start = kappa_j_start,
                  basal_p_1=basal_p_1,
                  basal_p_2=basal_p_2,
                  cov1 = cov1,
                  cov2 = cov2,
                  ncovars1 = 1,
                  ncovars2 = 2
) 

bh_fit <- stan(
  file = here::here("scripts", "stan_mod_BH_DATA.stan"),
  data = data_list,
  chains = n_chains,
  warmup = warmups,
  iter = total_iterations,
  cores = n_cores)

write_rds(bh_fit, "output/stan_fit_statespace.RDS")

mcmc_trace(bh_fit, pars = c("c_1[1]","c_1[2]","c_1[3]"))
mcmc_trace(bh_fit, pars = c("c_2[1]","c_2[2]","c_2[3]"))
mcmc_trace(bh_fit, pars = c("theta1[1,1]","theta1[2,1]","theta1[3,1]"))
mcmc_trace(bh_fit, pars = c("theta2[1,1]","theta2[2,1]","theta2[3,1]"))
mcmc_trace(bh_fit, pars = c("sigma_y_j[1]","sigma_y_j[2]","sigma_y_j[3]"))
mcmc_trace(bh_fit, pars = c("sigma_y_sp[1]","sigma_y_sp[2]","sigma_y_sp[3]"))




# OLD ======== 
K = 3 # number of stocks involved
N <- c(nrow(sp)) #, nrow(sim_dat$N_sp), nrow(sim_dat$N_sp))  

data_stage_j <- as.matrix(juv_prop_ayk[,2:4]) #c(as.integer(sim_dat$N_j))#, 
# as.integer(sim_yukon_fall_df$N_j),
# as.integer(sim_kusko_df$N_j))

data_stage_sp <-as.matrix(sp[,2:4])#c(as.integer(sim_dat$N_sp))#, 
# as.integer(sim_yukon_fall_df$N_sp),
# as.integer(sim_kusko_df$N_sp))

kappa_j_start =  c(runif(1, 0.03, 0.07),
                   runif(1, 0.03, 0.07),
                   runif(1, 0.03, 0.07))
kappa_sp_start =  c(runif(1, 0.12, 0.2),
                    runif(1, 0.12, 0.2),
                    runif(1, 0.12, 0.2))

# I think this will actually need to be estimated... 
basal_p_1 = c(0.05,0.05,
              0.05) # straight from simulation
basal_p_2 = c(0.15,
              0.15,
              0.15) # straight from simulation

cov1 = as.matrix(as.numeric(zoop$scale))
cov2 =  as.matrix(cbind(m2_cov, chum_cov$scale))

data_list <- list(Ps = Ps,
                  fs=fs,
                  N = N, 
                  K = K,  
                  data_stage_j = data_stage_j, 
                  data_stage_sp = data_stage_sp,
                  kappa_sp_start = kappa_sp_start,
                  kappa_j_start = kappa_j_start,
                  basal_p_1=basal_p_1,
                  basal_p_2=basal_p_2,
                  cov1 = cov1,
                  cov2 = cov2,
                  ncovars1 = 1,
                  ncovars2 = 2
) 

bh_fit <- stan(
  file = here::here("scripts", "stan_mod_BH_DATA.stan"),
  data = data_list,
  chains = n_chains,
  warmup = warmups,
  iter = total_iterations,
  cores = n_cores)

write_rds(bh_fit, "output/stan_fit.RDS")

mcmc_trace(bh_fit, pars = c("c_1[1]","c_1[2]","c_1[3]"))
mcmc_trace(bh_fit, pars = c("c_2[1]","c_2[2]","c_2[3]"))
mcmc_trace(bh_fit, pars = c("theta1[1,1]","theta1[2,1]","theta1[3,1]"))
mcmc_trace(bh_fit, pars = c("theta2[1,1]","theta2[2,1]","theta2[3,1]"))
mcmc_trace(bh_fit, pars = c("sigma_y_j[1]","sigma_y_j[2]","sigma_y_j[3]"))
mcmc_trace(bh_fit, pars = c("sigma_y_sp[1]","sigma_y_sp[2]","sigma_y_sp[3]"))

bh_summary <- summary(bh_fit)$summary %>% 
  as.data.frame() %>% 
  mutate(variable = rownames(.)) %>% 
  select(variable, everything()) %>% 
  as_data_frame()

bh_summary %>% 
  slice(1:27) %>%
  ggplot() + 
  geom_linerange(aes(variable, ymin = `2.5%`,ymax = `97.5%`)) + 
  geom_crossbar(aes(variable, mean, ymin = `25%`, ymax = `75%`), fill= 'grey') + 
  facet_wrap(~variable, scales = 'free') # +
# geom_point(data = obs_df, aes(variable, mean), color = "red" ) #observed



