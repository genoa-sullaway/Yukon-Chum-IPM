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
n_chains <- 1
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
  # zoop are already mean scaled
  dplyr::mutate(yukon_mean_discharge = as.numeric(scale(yukon_mean_discharge)),
                SST_CDD_NBS = as.numeric(scale(SST_CDD_NBS))) %>%
  dplyr::select(SST_CDD_NBS,# yukon_mean_discharge,
                Large_zoop,
                Cnideria
                #,yukon_mean_discharge 
                ) %>% #,yukon_mean_discharge) %>% #, Cnideria, Large_zoop) %>%
  as.matrix()

# the temp in 2001 is gonna effect fish from brood year 1999
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
kappa_marine_start = c(basal_p_2,basal_p_2)
kappa_marine_mort_start = c(-log(basal_p_2),-log(basal_p_2))


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
# # Initial values ========
# init_fn <- function(chain_id=1) {
#   list( 
#        theta1=list(rnorm(1,-0.1,0.05),
#                    rnorm(1,0.1,0.05),
#                    rnorm(1,-0.1,0.05)), # list because there are 3 thetas 
# 
#        theta2=list(rnorm(1,-0.01,0.05),
#                    rnorm(1,-0.01,0.05)), # list becaues two thetas 
# 
#        prob = list(rbeta(1,0.25,1),
#                    rbeta(1,0.75,1),
#                    rbeta(1,0.2,1)), 
#        
#        D_scale = runif(1,0.1,0.8),  
#        
#        g = list(runif(1,0.001,0.5),
#                 runif(1,0.5,1),
#                 runif(1,0.3,1),
#                 runif(1,0.001,0.5)), 
#        
#        log_catch_q = rnorm(1,-5,0.01),
#        
#        log_F_dev_y = rnorm(nRyrs_T,0,2),
#        log_F_mean = runif(1,0.2,2),
#           
#        basal_p_1 = runif(1,0.1,1), 
#        basal_p_2 = runif(1,0.1,1), 
#        
#        sigma_y_j = runif(1,0.5,2)
#        )
#      }
# 
# init_ll <- lapply(1:n_chains, function(id) init_fn(chain_id = id))
# 

# ASSIGN DATA ==========
data_list_stan <- list(nByrs=nByrs,
                       nRyrs=nRyrs,
                       nRyrs_T = nRyrs_T, 
                       A=A,
                       t_start = t_start,
                       
                       Ps=Ps,
                       fs=fs,
                       M = M_fill_stan,
                      
                       data_sp_cv = spawner_cv$fall_spawner_cv, 
                       data_recruit_cv = spawner_cv$summer_recruit_cv, 
                       
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
                      
                       basal_p_1 =basal_p_1,
                       basal_p_2 = basal_p_2)

# call mod  ===========================
bh_fit <- stan(
  file = here::here("scripts", "stan_mod_BH_dat.stan"),
  data = data_list_stan,
  chains = n_chains,  
  warmup = warmups,
  iter = total_iterations,
  cores = n_cores
  )

write_rds(bh_fit, "output/stan_fit_DATA.RDS")

