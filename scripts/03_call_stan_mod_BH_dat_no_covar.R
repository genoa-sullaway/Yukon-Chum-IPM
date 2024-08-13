library(rstan)
library(tidyverse)
library(here)
library(bayesplot)
library(rstanarm) 

library(tidync)
library(lubridate) 
library(readxl)

# Things to look into  =======================================================
# sigma for both life stages in estimates so I can fix it instead of have model estimate it 

# Load data ==================================================================
# see 01_make salmon data.R for salmon data tidying 
# see 02_make covariates for data tidying 

# setup inputs ===============================================================
warmups <- 2000
total_iterations <- 4000
max_treedepth <-  12
n_chains <- 4
n_cores <- 4
adapt_delta <- 0.95 # step size 

A = 4 # number of age classes, 3,4,5,6
K = 1 # number of stocks 
Ps = 0.5 # proportion of females - assumption, need to lit check
fs = as.vector(c(1800, 2000, 2200, 2440)) # as.vector(c(1800, 2000, 2200, 2440)) #as.vector(c(2000, 2000, 2000, 2000)) # fecundity - Gilk-Baumer 2009 estimate for Kusko Chum is: 2440. I added extra numbers temporarily just so that younger fish reproduce less, but will have to look up data for this more...
t_start = A +2  # to fill starting values 

year_min = 2001
year_max_cal = 2020
year_max_brood = 2017

# load salmon data ==============================================
## Fall age comp ================================================
yukon_fall_obs_agecomp <- read_csv("data/processed_data/yukon_fall_age_comp.csv") %>%
  filter(cal_year >= year_min, 
         cal_year <= year_max_cal
  ) %>%
  dplyr::select(2:ncol(.)) %>%
  as.matrix()

## Spawners, Recruits, Harvest ==================================== 
yukon_fall_spawners <-read_csv("data/processed_data/yukon_fall_spawners.csv") %>%
  filter(cal_year >= year_min, 
         cal_year <= year_max_cal) %>% 
  dplyr::select(2) %>%
  as.vector()

yukon_fall_harvest<-read_csv("data/processed_data/yukon_fall_harvest.csv") %>%
  filter(cal_year >= year_min, 
         cal_year <= year_max_cal) %>% 
  dplyr::select(2) %>%
  as.vector()

yukon_fall_recruits<-read_csv("data/processed_data/yukon_fall_recruits.csv") %>%
  filter(cal_year >= year_min#,
         #cal_year <= year_max_cal
  ) %>%
  dplyr::select(2) %>%
  as.vector()

yukon_fall_return_brood_year<- read_csv("data/processed_data/yukon_fall_yukon_fall_return_brood_year.csv") %>%
  filter(Brood_Year >= year_min,
         Brood_Year <= year_max_brood) %>% 
  dplyr::select(2) %>% 
  as.vector()

## Fall Juveniles ================================================
fall_juv <- read_csv("data/processed_data/tidy_juv_fall_yukon.csv")  %>%
  dplyr::mutate(Year = Year-1) %>% 
  filter(Year <= year_max_brood) %>% 
  dplyr::select(2) %>% 
  as.vector()

# specific year inputs ========
nByrs = nrow(fall_juv) # Number of BROOD years                
nRyrs = nrow(yukon_fall_harvest) # Number of CAL/RETURN  
nRyrs_T = nByrs + 4 + 2

# starting values to fix  =============
N_recruit_start = matrix(NA,nrow=t_start, ncol=A)
N_catch_start = matrix(NA,nrow=t_start, ncol=A)
N_egg_start = matrix(0,nrow=t_start, ncol=A)
N_sp_start = matrix(NA,nrow=t_start, ncol=A)

N_j_start = exp(rnorm(1,17,1)) 
N_brood_year_return_start = exp(rnorm(1,16.5,1))

for (a in 1:A) {
  for(t in 1:t_start){
    N_recruit_start[t,a] = yukon_fall_recruits$total_run[t]*yukon_fall_obs_agecomp[t,a] #exp(rnorm(1,yukon_fall_recruits$mean,1))*p[t,]
    N_sp_start[t,a] = yukon_fall_spawners$Spawners[t] *yukon_fall_obs_agecomp[t,a]   #exp(rnorm(1,yukon_fall_spawners$mean,1))*p[t,]
    N_catch_start[t,a] = yukon_fall_harvest$harvest[t]*yukon_fall_obs_agecomp[t,a]   #exp(rnorm(1,yukon_fall_harvest$mean,1))*p[t,]
    N_egg_start[t,a] = exp(17.5)*yukon_fall_obs_agecomp[t,a]  
  }
}

# log for model ======
N_j_start_log = log(N_j_start)
N_brood_year_return_start_log =  log(N_brood_year_return_start)

N_recruit_start_log = log(N_recruit_start+ 0.001)
N_sp_start_log = log(N_sp_start+ 0.001) 
N_catch_start_log = log(N_catch_start+ 0.001) 
N_egg_start_log  = log(N_egg_start+ 0.001)
 
# fix marine mortality =======
# generally low mortality in ocean for older life stages 
M_fill_stan = c(0.06, 0.06, 0.06,0.06) # will be cumulative 

#ess age comp =======
ess_age_comp =300# as.vector(rep(300, times = nByrs))

# STAN STARTING VALUES ==========
# kappa_j_start =  basal_p_1
# kappa_marine_start = c(basal_p_2,basal_p_2)
# kappa_marine_mort_start = c(-log(basal_p_2), -log(basal_p_2))

# use average age comp to distribute starting values
# p <- colMeans(yukon_fall_obs_agecomp[1:21,]) 

# ASSIGN DATA ==========
data_list_stan <- list(nByrs=nByrs,
                       nRyrs=nRyrs,
                       nRyrs_T = nRyrs_T, 
                       A=A,
                       t_start = t_start,
                       
                       Ps=Ps,
                       fs=fs,
                       M = M_fill_stan,
                       
                       data_stage_j = as.vector(fall_juv$fall_abundance), 
                       data_stage_return = as.vector(yukon_fall_return_brood_year$Brood_Year_Return),
                       data_stage_sp = as.vector(yukon_fall_spawners$Spawners),
                       data_stage_harvest = as.vector(yukon_fall_harvest$harvest), 
                       
                       o_run_comp=yukon_fall_obs_agecomp,
                       ess_age_comp=ess_age_comp,
                       
                       # N_j_start_log =N_j_start_log,
                       # N_brood_year_return_start_log =  N_brood_year_return_start_log,
                       # N_recruit_start_log = N_recruit_start_log,
                       # N_sp_start_log =N_sp_start_log,
                       # N_catch_start_log = N_catch_start_log,
                       # N_egg_start_log=N_egg_start_log,
                       
                       basal_p_1 =basal_p_1,
                       basal_p_2 = basal_p_2)#,
                       
                       #pi=pi)

# call mod  ===========================
bh_fit <- stan(
  file = here::here("scripts", "stan_mod_BH_dat_no_covar.stan"),
  data = data_list_stan,
  chains = 1,  
  warmup = warmups,
  iter = total_iterations,
  cores = n_cores,
  verbose=FALSE)

write_rds(bh_fit, "output/stan_fit_DATA.RDS")
