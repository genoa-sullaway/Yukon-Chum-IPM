library(rstan)
library(tidyverse)
library(here)
library(bayesplot)
library(rstanarm) 

library(tidync)
library(lubridate) 
library(readxl)

# Load data =======================================================
# see 01_make salmon data.R for salmon data tidying 
# see 02_make covariates for cov data tidying 
add_byrs <- c(0,0,0)

# load covariate data:
stage_a_cov <- read_csv("data/processed_covariates/stage_a_all.csv")
stage_b_cov <- read_csv("data/processed_covariates/stage_b_all.csv")

# setup inputs ==============================================================
warmups <- 2000
total_iterations <- 4000
max_treedepth <-  15
n_chains <- 1
n_cores <- 4
adapt_delta <- 0.95

# load salmon data ================================================
summer_age_comp<-read_csv("data/age_comps/processed_age_comps_summer_yukon.csv")  %>% 
  filter(!cal_year < 2005, 
         !cal_year == 2023)

summer_brood <- read_csv("output/yukon_summer_broodyear.csv")%>%
  filter(!brood_year < 2002) # for now to simplify matching with juveniles

yukon_summer <- read_excel("data/Yukon_Escapement_ADFG/S Chum RR 2023.xlsx", sheet = 2) %>%
  dplyr::select(1,11:14) %>% 
  janitor::row_to_names(row_number = 1) %>%
  dplyr::rename(cal_year = "Year")  %>%
  dplyr::mutate(age3=as.numeric(age3),
                age4=as.numeric(age4),
                age5=as.numeric(age5),
                age6=as.numeric(age6)) %>% 
  filter(!cal_year == 2023, !cal_year < 2005)

## harvest below weir 
harvest_escapement <- read_excel("data/Yukon_Escapement_ADFG/S Chum RR 2023.xlsx", sheet = 2) %>%
  dplyr::select(1:2,4) %>%  
  janitor::row_to_names(row_number = 1)  %>% 
  dplyr::rename(cal_year = "Year") %>% 
  dplyr::mutate(cal_year = as.numeric(cal_year), 
         Harvest = as.numeric(Harvest), 
         Escapement = as.numeric(Escapement)) %>% 
  filter(!cal_year == 2023, !cal_year < 2005) %>% 
  as.data.frame()  #%>%  
 #as.matrix()

# basis data ============
juv<- read_csv("data/tidy_BASIS_AYK_model.csv") %>%
  dplyr::select(1,2) # yukon summer is column labeled 1, yukon fall is 2, kusko is 3
  
# Organize data call inputs ================================================

nByrs = length(unique(juv$brood_year)) # Number of BROOD years                 #- 1978-2022 calendar years 
nRyrs = length(unique(yukon_summer$cal_year)) # Number of CAL/RETURN                    #years - 1978-2022 calendar years 
A = 4 # number of age classes, 3,4,5,6
K = 1 # number of stocks 
Ps = 0.5 # proportion of females - assumption, need to lit check
fs = c(1800, 2000, 2200, 2440) # fecundity - Gilk-Baumer 2009 estimate for Kusko Chum is: 2440. I added extra numbers temporarily just so that younger fish reproduce less, but will have to look up data for this more...

data_stage_j  <- as.matrix(juv$`1`, nrow = nByrs, ncol = K) # juveniles from basis data, by brood year
data_stage_harvest  <-  matrix(harvest_escapement$Harvest, nrow = nRyrs, ncol = K) # harvest_escapement[,3] #as.numeric(c(harvest_escapement$Harvest)))
data_stage_sp <- matrix(harvest_escapement[,2], nrow = nRyrs, ncol = K) #as.matrix(as.numeric(harvest_escapement$Escapement)) #Escapement and this addition equals the same thing:  yukon_summer[,2]+yukon_summer[,3]+yukon_summer[,4]+yukon_summer[,5] # this is spawners by recruitment/calendar year, summed across age classes 
#colnames(data_stage_sp) <- "abundance"

# fix sigma until RR data or until its running and I can try to estimate it better 
set.seed(123)
sigma_y_j <- matrix(ncol = 1, nrow =K, NA)
sigma_y_sp <- matrix(ncol = 1, nrow =K, NA)

for(k in 1:K) {  
  sigma_y_j[1,k] = rnorm(1,10)
  sigma_y_sp[1,k] = rnorm(1,10)
}

# starting values for survival rate 
kappa_j_start =  as.matrix(data.frame(kappa=c(rep(runif(1, 0.03, 0.07), times =K)))) # ,
                # as.numeric(rep(runif(1, 0.03, 0.07), times = K)) #,
                # as.matrix(data.frame(kappa=c(rep(runif(1, 0.03, 0.07), times =K)))) # ,
                   # runif(1, 0.03, 0.07),
                   # runif(1, 0.03, 0.07))
kappa_marine_start = as.matrix(data.frame(kappa=c(rep(runif(1, 0.12, 0.2), times =K)))) # ,
  # as.numeric(rep(runif(1, 0.12, 0.2), times = K)) #,
                    # runif(1, 0.12, 0.2),
                    # runif(1, 0.12, 0.2))

# covariates ===========
# number covariates for each life stage 
ncovars1 = 1
ncovars2 = 1
# arrange actual covariate data 
cov1 = as.matrix(stage_a_cov %>%
                    filter(Year > (min(juv$brood_year)-1),
                           !Year == 2023) %>%
                    dplyr::select(SST_CDD_NBS))

cov2 =  as.matrix(stage_b_cov %>%
                   filter(Year > (min(juv$brood_year)-1),
                           !Year == 2023) %>% 
                   dplyr::select(yukon_mean_discharge_summer))

# mean productivity rate =====
# I think this will actually need to be estimated... but will fix it for now. 
basal_p_1 = 0.05#,0.05,
              #0.05) # straight from simulation
basal_p_2 = 0.15#,
              # 0.15,
              # 0.15) # straight from simulation

# age comp info =============== 
set.seed(123)
prob <- c(rbeta(1,1,1),
          rbeta(1,1,1),
          rbeta(1,1,1),
          rbeta(1,1,1)) # probability of maturation for each age class...curry has it pulling from beta as a RE in script. 
 
o_run <- array(data = as.matrix(yukon_summer[,2:5]), dim = c(nRyrs, K,A))
#o_run <- as.matrix(yukon_summer[,2:5]) # observed run size by age class 
o_run_comp <- array(data = as.matrix(summer_age_comp[,2:5]), dim = c(nRyrs, K,A))
o_run_comp_mat <- as.matrix(summer_age_comp[,2:5]) # proportional age comp by year
ess_age_comp <- rep(200, times = nRyrs)

# H_b-  age comp harvest =========== I am not sure if this is the right way to do this but doing it this way for now....
H_b <- cbind(data_stage_harvest,o_run_comp_mat) %>%
        as.data.frame() %>% 
        dplyr::mutate(a3 = V1*age3,
                      a4 = V1*age4,
                      a5 = V1*age5,
                      a6 = V1*age6) %>%
        dplyr::select(6:9) %>%
  as.matrix()

# assign data list ==========
data_list <- list(nByrs=nByrs,
                  nRyrs=nRyrs,
                  A=A,
                  K=K, 
                  Ps=Ps,
                  fs=fs,
                  data_stage_j = data_stage_j,
                  data_stage_harvest= data_stage_harvest,
                  data_stage_sp = data_stage_sp,
                  sigma_y_j=sigma_y_j,
                  sigma_y_sp=sigma_y_sp,
                  kappa_marine_start = kappa_marine_start,
                  kappa_j_start = kappa_j_start,
                  cov1 = cov1,
                  cov2 = cov2,
                  ncovars1 = 1,
                  ncovars2 = 1,
                  basal_p_1=basal_p_1,
                  basal_p_2=basal_p_2,
                  H_b=H_b,
                  prob=prob,
                  o_run=o_run,
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



