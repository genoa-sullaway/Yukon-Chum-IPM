library(GGally)
library(here)
library(tidyverse)

 
year_min = 2002
year_max_cal = 2022
year_max_brood = 2021 # fall juvenile data go to 2022, but as a brood year it is 2021

# load more recent covariates ========================
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
                sockeye_juv_index, 
                fall_snow_cummulative
  ) 

X_a<-stage_a_cov[,1:6]
colinearity_a <-ggpairs(X_a)
pdf("output/cova_a_plot.pdf")
print(colinearity_a)
dev.off()
 
# stage b =========
stage_b_cov <- read_csv("data/processed_covariates/stage_b_all.csv") %>%
  dplyr::rename(cal_year = Year) %>% 
  dplyr::mutate(brood_year = cal_year-2) %>% 
  filter(brood_year >= year_min, 
         brood_year <= year_max_brood) %>% 
  dplyr::mutate( SST_CDD_Aleut = as.numeric(scale(SST_CDD_Aleut)),
                Chum_hatchery= as.numeric(scale(Chum_hatchery)), 
                 Pink_hatchery= as.numeric(scale(Pink_hatchery))
                # full_index = as.numeric(scale(full_index))
                ) %>% 
  dplyr::select(SST_CDD_Aleut,
                Chum_hatchery,
                Pink_hatchery,
                full_index_scale)  

X_b<-stage_b_cov[,1:4]
colinearity_b <-ggpairs(X_b)
 
pdf("output/cova_b_plot.pdf")
print(colinearity_b)
dev.off()




