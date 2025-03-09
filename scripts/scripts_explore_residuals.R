# load model
library(rstan)
library(tidyverse)
library(here)
library(bayesplot) 
library(tidync)
library(lubridate) 
library(readxl)

bh_fit<- read_rds("output/stan_fit_DATA.RDS")

# load data 
years <-read_csv("data/processed_data/yukon_fall_spawners.csv") %>%
  filter(cal_year >= year_min) %>%
  dplyr::select(cal_year) %>%
  dplyr::mutate(time = c(1:nrow(.)))

year_min = 2002
year_max_cal = 2022
year_max_brood = 2021 # fall juvenile data go to 2022, but as a brood year it is 2021

# load salmon data ==============================================
## Spawners, Recruits, Harvest ==================================== 
yukon_fall_spawners <-read_csv("data/processed_data/yukon_fall_spawners.csv") %>%
  filter(cal_year >= year_min, 
         cal_year <= year_max_cal) 

yukon_fall_harvest<-read_csv("data/processed_data/yukon_fall_harvest.csv") %>%
  filter(cal_year >= year_min, 
         cal_year <= year_max_cal) %>% 
  dplyr::select(2) %>%
  as.vector()

yukon_fall_return_brood_year<- read_csv("data/processed_data/yukon_fall_yukon_fall_return_brood_year.csv") %>%
  filter(Brood_Year >= year_min,
         !is.na(Brood_Year_Return)) %>%
  dplyr::rename(brood_year="Brood_Year")
 fall_juv <- read_csv("data/processed_data/tidy_juv_fall_yukon.csv")  %>%
  dplyr::mutate(brood_year = Year-1) %>%
  filter(!brood_year>year_max_brood) %>%
  dplyr::select(brood_year,fall_abund)
 
# load covariates ============= 
 #  covariates =================  
 stage_a_cov <- read_csv("data/processed_covariates/stage_a_all.csv") %>%
   filter(brood_year >= year_min, 
          brood_year <= year_max_brood) %>%
   dplyr::mutate(SST_CDD_NBS = as.numeric(scale(SST_CDD_NBS)), 
                 yukon_mean_discharge = as.numeric(scale(yukon_mean_discharge)),
                 fall_snow_cummulative = as.numeric(scale(fall_snow_cummulative)), 
                 pollock_recruit_scale = as.numeric(scale(Recruit_age_1_millions))) %>%
   dplyr::select(brood_year, SST_CDD_NBS, 
                 yukon_mean_discharge,
                 pollock_recruit_scale,
                 mean_size, # was already mean scaled because of the averaging across ages
                 fall_snow_cummulative
   ) %>%
   data.frame() 
 
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
   dplyr::select(brood_year,SST_CDD_Aleut,
                 Chum_hatchery,
                 Pink_hatchery,
                 full_index)  
 
 # obs data df ========
# obs_df_brood <- left_join(fall_juv,  yukon_fall_return_brood_year)
# 
# obs_df_cal <- left_join(yukon_fall_spawners,yukon_fall_harvest)

# Juv ================================
# Get fitted values from your stan fit =======
log_q <- exp(get_posterior_mean(bh_fit, "log_catch_q")[,1])  # y_pred would be your predicted values parameter

juv_pred <- log_q*get_posterior_mean(bh_fit, "N_j")[,1]  # y_pred would be your predicted values parameter

juv <- cbind(fall_juv, juv_pred) %>%
  data.frame() %>%
  dplyr::mutate(juv_resid = as.numeric(scale(fall_abund-juv_pred))) %>%
  left_join(stage_a_cov) %>%
  gather(5:ncol(.), key = "cov", value = "value")

# plot juv resid and covariates ============
ggplot(data = juv,aes(x=value, y = juv_resid, color = cov)) +
  geom_point( ) + 
  geom_smooth() + 
  facet_wrap(~cov, scale = "free")

# return ================================
# Get fitted values from your stan fit =======
return_pred <-  get_posterior_mean(bh_fit, "N_brood_year_return")[1:17,1]  # y_pred would be your predicted values parameter

return <- cbind(yukon_fall_return_brood_year, return_pred) %>%
  data.frame() %>%
  dplyr::mutate(return_resid = as.numeric(scale(Brood_Year_Return-return_pred))) %>%
  left_join(stage_b_cov) %>%
  gather(5:ncol(.), key = "cov", value = "value")

# plot juv resid and covariates ============
ggplot(data = return,aes(x=value, y = return_resid, color = cov)) +
  geom_point( ) + 
  geom_smooth() + 
  facet_wrap(~cov, scale = "free")




