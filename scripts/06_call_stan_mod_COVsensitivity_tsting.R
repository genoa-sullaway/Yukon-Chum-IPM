library(rstan)
library(tidyverse)
library(here)
library(bayesplot)
# library(rstanarm) 

library(tidync)
library(lubridate) 
library(readxl)
# remove.packages(c("StanHeaders", "rstan"))
# 
#  install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Load data ==================================================================
# see 01_make salmon data.R for salmon data tidying 
# see 02_make covariates for data tidying 

# setup inputs ===============================================================
warmups <- 2000
total_iterations <- 6000
max_treedepth <-  15
n_chains <- 4
n_cores <- 4
# adapt_delta <- 0.95 # step size 

A = 4 # number of age classes, 3,4,5,6
K = 1 # number of stocks 
Ps = 0.5 # proportion of females - assumption, need to lit check
# fs = as.vector(c(1800, 2000, 2200, 2440)) # as.vector(c(1800, 2000, 2200, 2440)) #as.vector(c(2000, 2000, 2000, 2000)) # fecundity - Gilk-Baumer 2009 estimate for Kusko Chum is: 2440. I added extra numbers temporarily just so that younger fish reproduce less, but will have to look up data for this more...
fs = as.vector(c(1800,2351, 2902,3453))
t_start = A +2  # to fill starting values 

year_min = 2002
year_max_cal = 2022
year_max_brood = 2021 # fall juvenile data go to 2022, but as a brood year it is 2021

# load salmon data ==============================================
## Fall age comp ================================================
yukon_fall_obs_agecomp <- read_csv("data/processed_data/yukon_fall_age_comp.csv") %>%
  filter(cal_year >= year_min, 
         cal_year <= year_max_cal) %>%
  dplyr::select(2:ncol(.)) %>%
  as.matrix()

## Fall brood year age comp ================================================
yukon_fall_broodyear_obs_agecomp <- read_csv("data/processed_data/yukon_fall_age_comp.csv") %>%
  dplyr::select(2:ncol(.)) %>%
  filter(!is.na(abund_0.6)) %>% 
  as.matrix()

### mean age comp brood year ========  
pi <- colMeans(yukon_fall_broodyear_obs_agecomp)

# add CV =========
return_CVs <- read_xlsx("data/chum_cv.xlsx") %>%
  filter(year >= year_min, 
         year <= year_max_cal) 

# add juvenile index CV ========
fall_juv_CV <- read_csv("data/Juvenile_Index_CC/Index for Sabrina.csv") %>%
  filter(Stratum == "Stratum_1")  

mean_2020 <- mean(fall_juv_CV$CV, na.rm = TRUE)

fall_juv_CV_all <- fall_juv_CV %>%
  dplyr::select(Time,CV) %>%
  dplyr::mutate(CV = case_when(Time == 2020 ~ mean_2020,
                               TRUE ~ CV)) %>%
  dplyr::mutate(brood_year = Time-1) %>%
  filter(!brood_year>year_max_brood) %>%
  dplyr::select(brood_year,CV) %>% 
  filter(!brood_year ==2001)


## Spawners, Recruits, Harvest ==================================== 
yukon_fall_spawners <-read_csv("data/processed_data/yukon_fall_spawners.csv") %>%
  filter(cal_year >= year_min, 
         cal_year <= year_max_cal) 

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
         !is.na(Brood_Year_Return))


## Fall Juveniles ================================================
fall_juv <- read_csv("data/processed_data/tidy_juv_fall_yukon.csv")  %>%
  dplyr::mutate(brood_year = Year-1) %>%
  filter(!brood_year>year_max_brood) %>%
  dplyr::select(brood_year,fall_abund)

# specific year inputs ========
nByrs = nrow(fall_juv) # Number of BROOD years                
nRyrs = nrow(yukon_fall_harvest) # Number of CAL/RETURN  
n_age_comp_yrs = nrow(yukon_fall_obs_agecomp)
nByrs_return_dat = nrow(yukon_fall_return_brood_year)

nRyrs_T = nByrs + 4 + 2

## starting values to fix  =============
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

N_recruit_start_log = log(N_recruit_start+ 1.001)
N_sp_start_log = log(N_sp_start+ 1.001)
N_catch_start_log = log(N_catch_start+ 1.001)
N_egg_start_log  = log(N_egg_start+ 1.001)

## CV ========================================
spawner_cv <- read_xlsx("data/chum_cv.xlsx") %>%
  filter(year >= year_min,
         year <= year_max_cal)

#  covariates =================  
stage_a_cov <- read_csv("data/processed_covariates/stage_a_all.csv") %>%
  filter(brood_year >= year_min, 
         brood_year <= year_max_brood) %>%
  dplyr::mutate(SST_CDD_NBS = as.numeric(scale(SST_CDD_NBS)), 
                yukon_mean_discharge = as.numeric(scale(yukon_mean_discharge)),
                fall_snow_cummulative = as.numeric(scale(fall_snow_cummulative)), 
                pollock_recruit_scale = as.numeric(scale(Recruit_age_1_millions))) %>%
  dplyr::select(SST_CDD_NBS, 
                yukon_mean_discharge,
                pollock_recruit_scale,
                mean_size, # was already mean scaled because of the averaging across ages
                # fall_mintemp_CDD#,
                fall_snow_cummulative
  ) %>% 
  as.matrix() 

# the temp in 2001 is gonna effect fish from brood year 1999
stage_b_cov <- read_csv("data/processed_covariates/stage_b_all.csv") %>%
  dplyr::rename(full_index=full_index_scale) %>% 
  filter(brood_year >= year_min, 
         brood_year <= year_max_brood) %>% 
  dplyr::mutate( SST_CDD_Aleut = as.numeric(scale(SST_CDD_Aleut)),
                 Chum_hatchery= as.numeric(scale(Chum_hatchery)),
                 Pink_hatchery= as.numeric(scale(Pink_hatchery)),
                 # full_index = as.numeric(scale(full_index))
  ) %>%
  dplyr::select(SST_CDD_Aleut,
                Chum_hatchery,
                Pink_hatchery,
                full_index) %>%
  as.matrix() # add another row because t+a+1 is 2024, so this is basically a dummy row for the last year of fish...

# number covariates for each life stage 
ncovars1 = ncol(stage_a_cov)
ncovars2 = ncol(stage_b_cov)

# fix marine mortality =======
# generally low mortality in ocean for older life stages 
M_fill_stan = c(0.06, 0.06, 0.06,0.06) # will be cumulative 

#ess age comp =======
ess_age_comp = 300 #as.vector(rep(400, times = nRyrs))

# use all the above data consistently across models, its just the covariate that gets altered
set_up_covariate_data <- function(exclusion_stage,covariate_exclude,covariate_name) {
# exclude in covariate 1 category =============
if(exclusion_stage == "a"){  
  ncovars1 = 4
  ncovars2 = 4

          if(covariate_exclude == "SST_CDD_NBS"){
            stage_a_cov <- read_csv("data/processed_covariates/stage_a_all.csv") %>%
            filter(brood_year >= year_min, 
                   brood_year <= year_max_brood) %>%
              dplyr::mutate(SST_CDD_NBS = as.numeric(scale(SST_CDD_NBS)), 
                            yukon_mean_discharge = as.numeric(scale(yukon_mean_discharge)),
                            fall_snow_cummulative = as.numeric(scale(fall_snow_cummulative)), 
                            pollock_recruit_scale = as.numeric(scale(Recruit_age_1_millions))) %>%
              dplyr::select(#SST_CDD_NBS, 
                            yukon_mean_discharge,
                            pollock_recruit_scale,
                            mean_size, # was already mean scaled because of the averaging across ages
                            fall_snow_cummulative
              ) %>% 
              as.matrix() 
            
        }
         
  
  if(covariate_exclude == "yukon_mean_discharge"){
    stage_a_cov <- read_csv("data/processed_covariates/stage_a_all.csv") %>%
      filter(brood_year >= year_min, 
             brood_year <= year_max_brood) %>%
      dplyr::mutate(SST_CDD_NBS = as.numeric(scale(SST_CDD_NBS)), 
        yukon_mean_discharge = as.numeric(scale(yukon_mean_discharge)),
        fall_snow_cummulative = as.numeric(scale(fall_snow_cummulative)), 
        pollock_recruit_scale = as.numeric(scale(Recruit_age_1_millions))) %>%
      dplyr::select(SST_CDD_NBS, 
        #yukon_mean_discharge,
        pollock_recruit_scale,
        mean_size, # was already mean scaled because of the averaging across ages
        fall_snow_cummulative
      ) %>% 
      as.matrix()
  }
  
  if(covariate_exclude == "pollock_recruit_scale"){
    stage_a_cov <- read_csv("data/processed_covariates/stage_a_all.csv") %>%
      filter(brood_year >= year_min, 
             brood_year <= year_max_brood) %>%
      dplyr::mutate(SST_CDD_NBS = as.numeric(scale(SST_CDD_NBS)), 
        yukon_mean_discharge = as.numeric(scale(yukon_mean_discharge)),
        fall_snow_cummulative = as.numeric(scale(fall_snow_cummulative)), 
        pollock_recruit_scale = as.numeric(scale(Recruit_age_1_millions))) %>%
      dplyr::select(SST_CDD_NBS, 
        yukon_mean_discharge,
        #pollock_recruit_scale,
        mean_size, # was already mean scaled because of the averaging across ages
        fall_snow_cummulative
      ) %>% 
      as.matrix()
  }
  if(covariate_exclude == "mean_size"){
    stage_a_cov <- read_csv("data/processed_covariates/stage_a_all.csv") %>%
      filter(brood_year >= year_min, 
             brood_year <= year_max_brood) %>%
      dplyr::mutate(SST_CDD_NBS = as.numeric(scale(SST_CDD_NBS)), 
        yukon_mean_discharge = as.numeric(scale(yukon_mean_discharge)),
        fall_snow_cummulative = as.numeric(scale(fall_snow_cummulative)), 
        pollock_recruit_scale = as.numeric(scale(Recruit_age_1_millions))) %>%
      dplyr::select(SST_CDD_NBS, 
        yukon_mean_discharge,
        pollock_recruit_scale,
        #mean_size, # was already mean scaled because of the averaging across ages
        fall_snow_cummulative
      ) %>% 
      as.matrix()
  }
  if(covariate_exclude == "fall_snow_cummulative"){
    stage_a_cov <- read_csv("data/processed_covariates/stage_a_all.csv") %>%
      filter(brood_year >= year_min, 
             brood_year <= year_max_brood) %>%
      dplyr::mutate(SST_CDD_NBS = as.numeric(scale(SST_CDD_NBS)), 
        yukon_mean_discharge = as.numeric(scale(yukon_mean_discharge)),
        fall_snow_cummulative = as.numeric(scale(fall_snow_cummulative)), 
        pollock_recruit_scale = as.numeric(scale(Recruit_age_1_millions))) %>%
      dplyr::select(SST_CDD_NBS, 
        yukon_mean_discharge,
        pollock_recruit_scale,
        mean_size, # was already mean scaled because of the averaging across ages
        # fall_snow_cummulative
      ) %>% 
      as.matrix()
  }
  
  stage_b_cov <- read_csv("data/processed_covariates/stage_b_all.csv") %>%
    dplyr::rename(full_index=full_index_scale) %>% 
    filter(brood_year >= year_min, 
           brood_year <= year_max_brood) %>% 
    dplyr::mutate( SST_CDD_Aleut = as.numeric(scale(SST_CDD_Aleut)),
                   Chum_hatchery= as.numeric(scale(Chum_hatchery)),
                   Pink_hatchery= as.numeric(scale(Pink_hatchery)),
                   # full_index = as.numeric(scale(full_index))
    ) %>%
    dplyr::select(SST_CDD_Aleut,
                  Chum_hatchery,
                  Pink_hatchery,
                  full_index) %>%
    as.matrix() # add another row because t+a+1 is 2024, so this is basically a dummy row for the last year of fish...
  
}
  
# exclude in cov 2 category =============
  if(exclusion_stage == "b"){  
    ncovars1 = 5
    ncovars2 = 3
    
    stage_a_cov <- read_csv("data/processed_covariates/stage_a_all.csv") %>%
      filter(brood_year >= year_min, 
             brood_year <= year_max_brood) %>%
      dplyr::mutate(SST_CDD_NBS = as.numeric(scale(SST_CDD_NBS)), 
                    yukon_mean_discharge = as.numeric(scale(yukon_mean_discharge)),
                    fall_snow_cummulative = as.numeric(scale(fall_snow_cummulative)), 
                    pollock_recruit_scale = as.numeric(scale(Recruit_age_1_millions))) %>%
      dplyr::select(SST_CDD_NBS, 
        yukon_mean_discharge,
        pollock_recruit_scale,
        mean_size, # was already mean scaled because of the averaging across ages
        fall_snow_cummulative
      ) %>% 
      as.matrix() 
    
if(covariate_exclude == "SST_CDD_Aleut"){
  stage_b_cov <- read_csv("data/processed_covariates/stage_b_all.csv") %>%
    dplyr::rename(full_index=full_index_scale) %>% 
    filter(brood_year >= year_min, 
           brood_year <= year_max_brood) %>% 
    dplyr::mutate( SST_CDD_Aleut = as.numeric(scale(SST_CDD_Aleut)),
                   Chum_hatchery= as.numeric(scale(Chum_hatchery)),
                   Pink_hatchery= as.numeric(scale(Pink_hatchery)),
                   #full_index = as.numeric(scale(full_index))
    ) %>%
    dplyr::select(#SST_CDD_Aleut,
                  Chum_hatchery,
                  Pink_hatchery,
                  full_index) %>%
    as.matrix() # add another row because t+a+1 is 2024, so this is basically a dummy row for the last year of fish...
  
}
    
if(covariate_exclude == "Chum_hatchery"){
  stage_b_cov <- read_csv("data/processed_covariates/stage_b_all.csv") %>%
    dplyr::rename(full_index=full_index_scale) %>% 
    filter(brood_year >= year_min, 
           brood_year <= year_max_brood) %>% 
    dplyr::mutate( SST_CDD_Aleut = as.numeric(scale(SST_CDD_Aleut)),
                   Chum_hatchery= as.numeric(scale(Chum_hatchery)),
                   Pink_hatchery= as.numeric(scale(Pink_hatchery)),
                   # full_index = as.numeric(scale(full_index))
    ) %>%
    dplyr::select(SST_CDD_Aleut,
                  #Chum_hatchery,
                  Pink_hatchery,
                  full_index) %>%
    as.matrix() # add another row because t+a+1 is 2024, so this is basically a dummy row for the last year of fish...
  
}
    
    if(covariate_exclude == "Pink_hatchery"){
      stage_b_cov <- read_csv("data/processed_covariates/stage_b_all.csv") %>%
        dplyr::rename(full_index=full_index_scale) %>% 
        filter(brood_year >= year_min, 
               brood_year <= year_max_brood) %>% 
        dplyr::mutate( SST_CDD_Aleut = as.numeric(scale(SST_CDD_Aleut)),
                       Chum_hatchery= as.numeric(scale(Chum_hatchery)),
                       Pink_hatchery= as.numeric(scale(Pink_hatchery)),
                       # full_index = as.numeric(scale(full_index))
        ) %>%
        dplyr::select(SST_CDD_Aleut,
                      Chum_hatchery,
                      #Pink_hatchery,
                      full_index) %>%
        as.matrix() # add another row because t+a+1 is 2024, so this is basically a dummy row for the last year of fish...
      
    }
    
    if(covariate_exclude == "full_index"){
      stage_b_cov <- read_csv("data/processed_covariates/stage_b_all.csv") %>%
        dplyr::rename(full_index=full_index_scale) %>% 
        filter(brood_year >= year_min, 
               brood_year <= year_max_brood) %>% 
        dplyr::mutate( SST_CDD_Aleut = as.numeric(scale(SST_CDD_Aleut)),
                       Chum_hatchery= as.numeric(scale(Chum_hatchery)),
                       Pink_hatchery= as.numeric(scale(Pink_hatchery)),
                       # full_index = as.numeric(scale(full_index))
        ) %>%
        dplyr::select(SST_CDD_Aleut,
                      Chum_hatchery,
                      Pink_hatchery#,
                     # full_index
                     ) %>%
        as.matrix() # add another row because t+a+1 is 2024, so this is basically a dummy row for the last year of fish...
      
    }
  }
  
  # assign data ==========
  data_list_stan <- list(nByrs=nByrs,
                         nRyrs=nRyrs,
                         nRyrs_T = nRyrs_T, 
                         
                         A=A,
                         t_start = t_start,
                         nByrs_return_dat=nByrs_return_dat,
                         
                         Ps=Ps,
                         fs=fs,
                         M = M_fill_stan,
                         
                         lik_count = 4, # for sensitivity testing 
                         
                         data_stage_j = as.vector(fall_juv$fall_abund), 
                         data_stage_return = as.vector(yukon_fall_return_brood_year$Brood_Year_Return),
                         #data_stage_return=as.vector(yukon_fall_recruits$total_run), 
                         data_stage_sp = as.vector(yukon_fall_spawners$Spawners),
                         data_stage_harvest = as.vector(yukon_fall_harvest$harvest), 
                         
                         years_data_sp = yukon_fall_spawners$cal_year,
                         years_data_juv = fall_juv$brood_year,
                         years_data_return = yukon_fall_return_brood_year$Brood_Year,
                         
                         ncovars1=ncovars1,
                         ncovars2=ncovars2,
                         
                         cov1=stage_a_cov,
                         cov2=stage_b_cov,
                         
                         data_sp_cv = spawner_cv$fall_spawner_cv,
                         
                         o_run_comp=(yukon_fall_obs_agecomp),
                         ess_age_comp=ess_age_comp,
                         # basal_p_1 = 0.9,
                         # basal_p_2 = 0.9,
                         log_c_1 = 18, 
                         log_c_2 =25,
                         juv_CV= fall_juv_CV_all$CV,
                         return_CV = return_CVs$fall_spawner_cv
                         # ricker_beta = 0.0002,
                         # ricker_alpha = 1.6
                         #pi = pi
  )
  
  # call mod ========
  bh_fit <- stan(
    file = here::here("scripts", "stan_mod_sensitivity.stan"),
    data = data_list_stan,
    chains = 1,  
    warmup = warmups, 
    iter = total_iterations, 
    cores = n_cores, 
    verbose=FALSE, 
    control = list(adapt_delta = 0.99))
  
  write_rds(bh_fit, paste0("output/stan_sensitivity",exclusion_stage,"_",covariate_name,".RDS"))
  return(bh_fit)
}

 
cov_a_list<- c("SST_CDD_NBS", 
               "yukon_mean_discharge",
               "pollock_recruit_scale",
               "mean_size",
               "fall_snow_cummulative"
               )

cov_b_list<-  c("SST_CDD_Aleut",
                "Chum_hatchery",
                "Pink_hatchery",
                "full_index")

stage_a <- list()
stage_b <- list()

# call function stage a =======
for (i in 1:length(cov_a_list)) {
 cov <- cov_a_list[[i]]

 stage_a[[i]] <- set_up_covariate_data(exclusion_stage = "a", covariate_exclude = cov,covariate_name = cov)
}
write_rds(stage_a, "output/stan_sensitivity_stage_A_list.RDS")

# call function stage b =======
for (i in 1:length(cov_b_list)) {
  cov <- cov_b_list[[i]]
  
  stage_b[[i]] <- set_up_covariate_data(exclusion_stage = "b", covariate_exclude = cov,covariate_name = cov)
}
write_rds(stage_b, "output/stan_sensitivity_stage_B_list.RDS")

 


