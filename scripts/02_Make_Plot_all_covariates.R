# Bring in all covariates into one big dataset and plot all covariates in one plot 
library(tidyverse)
library(here)

# years in covariate timeseries to expand the TS that need it 
YEAR <-data.frame(YEAR = seq(from = 2002, to = 2022, by =1))

# Stage A - Load data ============= 
sst_a <- read_csv("data/processed_covariates/Stage_A_CDD.csv") %>%
  dplyr::rename(SST_CDD_NBS = "CDD",
                Year = "year")

# this is a GAM zoop index
zoop <- read_csv("data/processed_covariates/Stage_A_Zooplankton_Index.csv")  %>% 
  dplyr::rename(Year = "YEAR")  
 
river_discharge_a <- read_csv("data/processed_covariates/Stage_A_YK_Discharge.csv") %>%
  dplyr::select(Year, mean_discharge,id) %>%
  spread(id, mean_discharge) %>%
  dplyr::rename(#kusko_mean_discharge = "Kusko",
         yukon_mean_discharge = "Yukon") %>% 
  dplyr::mutate(Year = Year+1) %>% # already brood year, this will line up when i subtract 1 year from all covariate years 
 dplyr::select(Year, yukon_mean_discharge)
# 
# air_temp_a <- read_csv("data/processed_covariates/Stage_A_airtemp.csv") %>%
#   filter(type =="average", id == "spring") %>%
#   dplyr::rename(mean_air_temp = "value") %>%
#   dplyr::select(Year, mean_air_temp,site) %>%
#   spread(site,mean_air_temp) %>%
#   dplyr::rename(kusko_aniak_mean_airtemp = "aniak",
#          yukon_chena_mean_airtemp = "chena")
  
pollock <- read_csv("data/processed_covariates/Stage_A_Pollock_Recruitment.csv") %>%
  dplyr::select(Year, Recruit_age_1_millions)

size <- read_csv("data/processed_covariates/Stage_A_Size.csv") %>%
  dplyr::rename(#Year = "cal_year",
                mean_size = "trend")

  # Stage A - One DF for model ============= 
stage_a_cov<- left_join(river_discharge_a,sst_a)  %>%
              # left_join(air_temp_a) %>%
              left_join(zoop)  %>%
              left_join(pollock) %>%
              left_join(size) %>%
  data.frame()

# Stage A - Save DF ============= 
write_csv(stage_a_cov, "data/processed_covariates/stage_a_all.csv")
  
# Stage B - Load data ============= 
fullness_df<-readRDS("data/processed_covariates/fullness_cov.RDS") %>%
  dplyr::rename(Year = "brood_year",
                # full_index = "pred"
                ) %>%
  dplyr::select(Year, full_index_scale ) %>%
  dplyr::mutate(full_index_scale=as.numeric(full_index_scale))

hatchery_chum_df<-read_csv("data/hatchery_Chum_Covariate_AKandAsia.csv") 
hatchery_chum_b<-hatchery_chum_df %>%
  dplyr::rename(Chum_hatchery="sum") %>%
  dplyr::select(Year, Chum_hatchery) %>%
  rbind(data.frame(Year = c(2023),
                   Chum_hatchery = c(mean(hatchery_chum_df$sum) + 0.01)))

hatchery_pink_df <- read_csv("data/hatchery_Pink_Covariate_AKandAsia.csv") 
hatchery_pink_b <- hatchery_pink_df%>%
  dplyr::rename(Pink_hatchery="sum") %>%
  dplyr::select(Year, Pink_hatchery) %>% 
  rbind(data.frame(Year = c(2023),
                   Pink_hatchery = c(mean(hatchery_pink_df$sum) + 0.01)))

sst_b<-read_csv("data/processed_covariates/Stage_B_CDD.csv") %>%
  dplyr::rename(SST_CDD_Aleut = "CDD",
                Year = "year") %>%
  dplyr::select(Year, SST_CDD_Aleut)

stage_b_cov<- left_join(hatchery_pink_b,sst_b) %>%
              left_join(hatchery_chum_b) %>%
              left_join(fullness_df) 

# Stage B - Save DF ============= 
write_csv(stage_b_cov, "data/processed_covariates/stage_b_all.csv")
