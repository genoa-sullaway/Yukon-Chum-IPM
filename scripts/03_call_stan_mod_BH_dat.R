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
year_max_brood = 2021

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
         )%>%
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
  filter(Year <= 2021) %>% # 1 year less than all spawners 
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
         Year <= year_max_brood
         ) %>%
  dplyr::select(yukon_mean_discharge,SST_CDD_NBS) %>% 
  dplyr::mutate(yukon_mean_discharge = as.numeric(scale(yukon_mean_discharge)),
         SST_CDD_NBS = as.numeric(scale(SST_CDD_NBS))) %>% 
  as.matrix()

stage_b_cov <- read_csv("data/processed_covariates/stage_b_all.csv") %>%
  filter(Year >= year_min, 
         Year <= year_max_brood
         ) %>% 
  dplyr::select(SST_CDD_SEBS) %>% 
  dplyr::mutate(SST_CDD_SEBS = as.numeric(scale(SST_CDD_SEBS))) %>% 
  as.matrix()

# Organize data call inputs ================================================
nByrs = nrow(fall_juv) # Number of BROOD years                
nRyrs = nrow(yukon_fall_harvest)  # Number of CAL/RETURN  
nRyrs_T = nRyrs + 4
A = 4 # number of age classes, 3,4,5,6
K = 1 # number of stocks 
Ps = 0.5 # proportion of females - assumption, need to lit check
fs = as.vector(c(1800, 2000, 2200, 2440)) # fecundity - Gilk-Baumer 2009 estimate for Kusko Chum is: 2440. I added extra numbers temporarily just so that younger fish reproduce less, but will have to look up data for this more...
t_start = 5 # to fill starting values 
 
# covariates ===========
# number covariates for each life stage 
ncovars1 = 2
ncovars2 = 1

# mean productivity rate =====
 # estimating this now
basal_p_1 = 0.5#,0.05,
              #0.05) # straight from simulation
basal_p_2 = 0.5#,
              # 0.15,
              # 0.15) # straight from simulation
# fix marine mortality =======
# generally low mortality in ocean for older life stages 
M_fill_stan = c(0.06, 0.06, 0.06) # will be cumulative 

#ess age comp =======
ess_age_comp = as.vector(rep(300, times = nByrs))

# Process error  ===================
# error is now estimated
# process_error_j = 10 # matrix(nrow=K,ncol=1,rep(1, times =K))  #matrix(nrow=nByrs,ncol=1,rep(1, times =nByrs )) #rnorm(nByrs*1,1,0.2))
# process_error_sp = 10 # matrix(nrow=K,ncol=1,rep(1, times =K)) #matrix(nrow=nRyrs,ncol=1,rep(2, times =nRyrs )) #rnorm(nByrs*1,5, 1))
# process_error_r = 10 # matrix(nrow=K,ncol=1,rep(1, times =K))  #matrix(nrow=nRyrs,ncol=1,rep(3, times =nRyrs )) #rnorm(nByrs*1,5, 1))
# process_error_h = 10 # matrix(nrow=K,ncol=1,rep(1, times =K))  #matrix(nrow=nRyrs,ncol=1,rep(3, times =nRyrs )) #rnorm(nByrs*1,5, 1))

# STAN STARTING VALUES ==========
kappa_j_start =  basal_p_1 # runif(1, 0.2, 0.2)
kappa_marine_start = basal_p_2 #runif(1, 0.4, 0.4)

N_j_start =  as.vector(NA)
N_e_sum_start = as.vector(NA)

N_recruit_start = matrix(NA,nrow=t_start, ncol=A)
N_catch_start = matrix(NA,nrow=t_start, ncol=A)
 N_egg_start = matrix(NA,nrow=t_start, ncol=A)
N_ocean_start = matrix(NA,nrow=t_start, ncol=A)#vector() # ages # array(data = NA, dim = c(1, A))
N_sp_start = matrix(NA,nrow=t_start, ncol=A)#vector() # array(data = NA, dim = c(1, A,K))

N_j_start = exp(rnorm(1,9.2,2))
N_e_sum_start = exp(rnorm(1,20,2)) #exp(rnorm(1,30,2))

# use average age comp to distribute starting values
p <- colMeans(yukon_fall_obs_agecomp[1:21,]) 

for(t in 1:t_start){
  N_recruit_start[t,] = exp(rnorm(1,12.9,2))*p
  N_ocean_start[t,] = exp(rnorm(1,13,2))*p
  N_sp_start[t,] = exp(rnorm(1,12.2,2))*p #exp(rnorm(1,log(398700),2))*p 
  N_catch_start[t,] = exp(rnorm(1,10.6,2))*p #exp(rnorm(1,log(27769),2))*p 
  N_egg_start[t,] = exp(rnorm(1,20,2))*p
}

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
                       
                       data_stage_j = as.vector(fall_juv$fall_abundance), 
                       data_stage_return = as.vector(yukon_fall_recruits$total_run),
                       data_stage_sp = as.vector(yukon_fall_spawners$Spawners),
                       data_stage_harvest = as.vector(yukon_fall_harvest$harvest), 
                  
                      # N_sp_start = N_sp_start,
                       #N_catch_start = N_catch_start,
                       N_ocean_start = N_ocean_start,
                       N_egg_start = N_egg_start,
                       N_j_start =  N_j_start,
                      # N_recruit_start = N_recruit_start,
                       N_e_sum_start = N_e_sum_start,
                                        
                       kappa_marine_start = basal_p_2,
                       kappa_j_start = basal_p_1,
                                       
                       ncovars1=ncovars1,
                       ncovars2=ncovars2,

                       cov1=stage_a_cov,
                       cov2=stage_b_cov,
                                        
                       o_run_comp=yukon_fall_obs_agecomp,
                       ess_age_comp=ess_age_comp,
                       p_obs = p)
                 
# call mod  ===========================
bh_fit <- stan(
  file = here::here("scripts", "stan_mod_BH_dat.stan"),
  data = data_list_stan,
  chains = 1,#n_chains,
  warmup = warmups,
  iter = total_iterations,
  cores = n_cores)

write_rds(bh_fit, "output/stan_fit_DATA.RDS")
 

