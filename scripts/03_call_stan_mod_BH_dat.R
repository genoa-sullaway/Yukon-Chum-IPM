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
max_treedepth <-  15
n_chains <- 4
n_cores <- 4
adapt_delta <- 0.95

year_min = 2002
year_max_cal = 2022
year_max_brood = 2022

# load salmon data ==============================================
## Fall age comp ================================================
yukon_fall_obs_agecomp <- read_csv("data/processed_data/yukon_fall_age_comp.csv") %>%
  filter(cal_year >= year_min, 
         cal_year <= 2022
  ) %>%
  dplyr::select(2:ncol(.)) %>%
  as.matrix()

## Spawners, Recruits, Harvest ==================================== 
yukon_fall_spawners <-read_csv("data/processed_data/yukon_fall_spawners.csv") %>%
  filter(cal_year >= year_min #, 
         #cal_year <= year_max_cal
  ) %>%
  select(2) %>%
  as.vector()

yukon_fall_harvest<-read_csv("data/processed_data/yukon_fall_harvest.csv") %>%
  filter(cal_year >= year_min#, 
         #cal_year <= year_max_cal
  )%>%
  select(2) %>%
  as.vector()

yukon_fall_recruits<-read_csv("data/processed_data/yukon_fall_recruits.csv") %>%
  filter(cal_year >= year_min#, 
         #cal_year <= year_max_cal
  ) %>%
  select(2) %>%
  as.vector()

## Fall Juveniles ================================================
fall_juv <- read_csv("data/processed_data/tidy_juv_fall_yukon.csv")  %>%
  filter(Year <= year_max_brood) %>% 
  select(2) %>% 
  as.vector()

#plot(fall_juv$fall_abundance, type ="l")
# CV ========================================
spawner_cv <- read_xlsx("data/chum_cv.xlsx") %>% 
  filter(year >= year_min, 
         year <= 2022)  

#  covariates =================  
stage_a_cov <- read_csv("data/processed_covariates/stage_a_all.csv") %>%
  filter(Year >= year_min, 
         Year <= year_max_brood+1) %>%
  dplyr::mutate(yukon_mean_discharge = as.numeric(scale(yukon_mean_discharge)),
                SST_CDD_NBS = as.numeric(scale(SST_CDD_NBS))) %>%
  dplyr::select(SST_CDD_NBS,# yukon_mean_discharge,
                Cnideria,
                Large_zoop,
                #,yukon_mean_discharge 
                ) %>% #,yukon_mean_discharge) %>% #, Cnideria, Large_zoop) %>%
  as.matrix()
 
temp_b_cov <- read_csv("data/processed_covariates/stage_b_all.csv") %>%
  filter(brood_year >= year_min-1, 
         brood_year <= year_max_brood+1) %>% 
#         brood_year <= year_max_brood+2) %>% 
  dplyr::mutate(SST_CDD_GOA = as.numeric(scale(SST_CDD_GOA)),
                Chum_hatchery= as.numeric(scale(Chum_hatchery)),
                Pink_hatchery= as.numeric(scale(Pink_hatchery))#,
                #yukon_mean_discharge_summer= as.numeric(scale(yukon_mean_discharge_summer))
  ) %>% 
  dplyr::select(SST_CDD_GOA,
                # Chum_hatchery
                #,
                Pink_hatchery
                )  


#bind <- temp_b_cov %>% slice(22)

stage_b_cov <- temp_b_cov %>%
                  #  rbind(bind) %>% 
                    as.matrix() # add another row because t+a+1 is 2024, so this is basically a dummy row for the last year of fish...


# number covariates for each life stage 
ncovars1 = 3
ncovars2 = 2

# Organize data call inputs ================================================
nByrs = nrow(fall_juv) # Number of BROOD years                
nRyrs = nrow(yukon_fall_harvest) # Number of CAL/RETURN  
nRyrs_T = nByrs + 4 + 2
A = 4 # number of age classes, 3,4,5,6
K = 1 # number of stocks 
Ps = 0.5 # proportion of females - assumption, need to lit check
fs = as.vector(c(1800, 2000, 2200, 2440)) #as.vector(c(2000, 2000, 2000, 2000)) # fecundity - Gilk-Baumer 2009 estimate for Kusko Chum is: 2440. I added extra numbers temporarily just so that younger fish reproduce less, but will have to look up data for this more...
t_start = A + 2 # to fill starting values 

# mean productivity rate =====
# estimating this now
basal_p_1 = 0.1  # these are values it estimates at when allowed to

basal_p_2 = 0.4
# fix marine mortality =======
# generally low mortality in ocean for older life stages 
M_fill_stan = c(0.06, 0.06, 0.06,0.06) # will be cumulative 

#ess age comp =======
ess_age_comp = as.vector(rep(300, times = nByrs))

# STAN STARTING VALUES ==========
kappa_j_start =  basal_p_1
kappa_marine_start = basal_p_2

# N_j_start =  as.vector(NA)
# N_e_sum_start = as.vector(NA)
# 
# N_recruit_start = matrix(NA,nrow=t_start, ncol=A)
# N_catch_start = matrix(NA,nrow=t_start, ncol=A)
#  N_egg_start = matrix(NA,nrow=t_start, ncol=A)
# N_ocean_start = matrix(NA,nrow=t_start, ncol=A)#vector() # ages # array(data = NA, dim = c(1, A))
# N_sp_start = matrix(NA,nrow=t_start, ncol=A)#vector() # array(data = NA, dim = c(1, A,K))
# 
# N_j_start = exp(rnorm(1,15,2))
# N_e_sum_start = exp(rnorm(1,20,2)) #exp(rnorm(1,30,2))

# use average age comp to distribute starting values
p <- colMeans(yukon_fall_obs_agecomp[1:21,]) 
# 
# for(t in 1:t_start){
#   N_recruit_start[t,] = exp(rnorm(1,12.9,2))*p
#   N_ocean_start[t,] = exp(rnorm(1,13,2))*p
#   N_sp_start[t,] = exp(rnorm(1,12.2,2))*p #exp(rnorm(1,log(398700),2))*p 
#   N_catch_start[t,] = exp(rnorm(1,10.6,2))*p #exp(rnorm(1,log(27769),2))*p 
#   N_egg_start[t,] = exp(rnorm(1,20,2))*p
# }

# ASSIGN DATA ==========
data_list_stan <- list(nByrs=nByrs,
                       nRyrs=nRyrs,
                       nRyrs_T = nRyrs_T, 
                       A=A,
                       t_start = t_start,
                       
                       Ps=Ps,
                       fs=fs,
                       M = M_fill_stan,
                       # basal_p_1=basal_p_1,
                       # basal_p_2=basal_p_2, estimating these now 
                       data_sp_cv = spawner_cv$fall_spawner_cv, 
                       data_recruit_cv = spawner_cv$summer_recruit_cv, 
                       #  data_j_cv = fall_juv_cv$CV,
                       
                       data_stage_j = as.vector(fall_juv$fall_abundance), 
                       data_stage_return = as.vector(yukon_fall_recruits$total_run),
                       data_stage_sp = as.vector(yukon_fall_spawners$Spawners),
                       data_stage_harvest = as.vector(yukon_fall_harvest$harvest), 
                      
                       kappa_marine_mort_start = -log(kappa_marine_start),
                       kappa_marine_start = kappa_marine_start,
                       kappa_j_start = kappa_j_start,
                       
                       ncovars1=ncovars1,
                       ncovars2=ncovars2,
                       
                       cov1=stage_a_cov,
                       cov2=stage_b_cov,
                       
                       o_run_comp=yukon_fall_obs_agecomp,
                       ess_age_comp=ess_age_comp,
                       p_obs = p,
                        # c_1 = exp(16.1), # works using 18,16 (in PPT notes) with no covar, close with covar but flattens in the middle of the timeseries...
                        # c_2 = exp(14),
                       basal_p_1 =basal_p_1,
                       basal_p_2 = basal_p_2)

# call mod  ===========================
bh_fit <- stan(
  file = here::here("scripts", "stan_mod_BH_dat.stan"),
  data = data_list_stan,
  chains = 1,  
  warmup = warmups,
  iter = total_iterations,
  cores = n_cores)

write_rds(bh_fit, "output/stan_fit_DATA.RDS")

