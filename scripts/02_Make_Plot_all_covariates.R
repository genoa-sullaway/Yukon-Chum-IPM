# Bring in all covariates into one big dataset and plot all covariates in one plot 
library(tidyverse)
library(here)

# years in covariate timeseries to expand the TS that need it 
# YEAR <-data.frame(YEAR = seq(from = 2002, to = 2022, by =1))

# Stage A - Load data ============= 
## FW stage A ======================
### Fairbanks airport temp and snow ========
# Group by year and get cummulative degree and snow days?? 
# min and mean temp is super correlated (81%) so just focusing on mean and max snow, they are not correlated! -11%
# temperature_snow <- read.csv("data/Fairbanks_airport_covariates/combined_temp_fairbanks.csv")  %>% 
#   group_by(year) %>% 
#   dplyr::summarise(fall_mintemp_CDD = sum(min_temperature),
#                    fall_meantemp_CDD = sum(mean_temperature),
#                    fall_max_snow_depth = sum(max_snow_depth_in)) %>%
#   filter(year > 2000) %>% 
#   dplyr::mutate(fall_mintemp_CDD = as.numeric(scale(fall_mintemp_CDD)),
#                 # fall_meantemp_CDD = as.numeric(scale(fall_meantemp_CDD)),
#                 fall_max_snow_depth= as.numeric(scale(fall_max_snow_depth)))  %>%
#   dplyr::rename(brood_year = "year") %>%
#   dplyr::select(brood_year,fall_mintemp_CDD,fall_max_snow_depth)  

# circle snow ============
circle_snow <- read.csv("data/circle_snow_processed.csv") %>%
  dplyr::rename(brood_year = "year") %>% 
  group_by(brood_year) %>% 
  dplyr::summarise(fall_snow_cummulative = sum(Monthly.Mean.Snow.Depth..in.))  
 
                 #                    fall_meantemp_CDD = sum(mean_temperature),
                 #                    fall_max_snow_depth = sum(max_snow_depth_in)) %>%
                 #   filter(year > 2000) %>% 
                 #   dplyr::mutate(fall_mintemp_CDD = as.numeric(scale(fall_mintemp_CDD)),
                 #                 # fall_meantemp_CDD = as.numeric(scale(fall_meantemp_CDD)),
                 #                 fall_max_snow_depth= as.numeric(scale(fall_max_snow_depth)))  %>%
                 #   dplyr::rename(brood_year = "year") %>%
                 #   dplyr::select(brood_year,fall_mintemp_CDD,fall_max_snow_depth)  

# cor.test(temperature_snow$fall_mintemp_CDD,temperature_snow$fall_meantemp_CDD)
# cor.test(temperature_snow$fall_mintemp_CDD,temperature_snow$fall_max_snow_depth)
# cor.test(temperature_snow$fall_meantemp_CDD,temperature_snow$fall_max_snow_depth)

### River Discharge ====== 
river_discharge_a <- read_csv("data/processed_covariates/Stage_A_YK_Discharge.csv") %>%
  dplyr::select(Year, mean_discharge,id) %>%
  spread(id, mean_discharge) %>%
  dplyr::rename(brood_year = "Year",
                yukon_mean_discharge = "Yukon") %>% 
  dplyr::select(brood_year, yukon_mean_discharge)

### Size =========== 
size <- read_csv("data/processed_covariates/Stage_A_Size.csv") %>%
  dplyr::rename(brood_year = "Year",
                mean_size = "trend")

## early marine stage A ===============
### Pollock ========
pollock <- read_csv("data/processed_covariates/Stage_A_Pollock_Recruitment.csv") %>%
  dplyr::select(Year, Recruit_age_1_millions) %>%
  dplyr::mutate(brood_year = Year-1) 

### NBS SST ========== 
sst_a <- read_csv("data/processed_covariates/Stage_A_CDD.csv") %>%
  dplyr::rename(SST_CDD_NBS = "CDD",
                Year = "year") %>%
  dplyr::mutate(brood_year = Year-1)  %>%
  dplyr::select(brood_year,SST_CDD_NBS)

### Zoop  ========== 
# this is a GAM zoop index
# zoop <- read_csv("data/processed_covariates/Stage_A_Zooplankton_Index.csv")  %>% 
#   dplyr::rename(Year = "YEAR")  %>%
#   dplyr::mutate(brood_year = Year-1)  %>%
#   dplyr::select(brood_year, Large_zoop, Cnideria)
 
# sockeye =========
sockeye <- read_csv("data/processed_covariates/Cov_A_Sockeye_JuvIndex.csv")  
  
  
  
  # Stage A - One DF for model ============= 
stage_a_cov<- left_join(river_discharge_a,sst_a)  %>%
              # left_join(zoop)  %>%
              left_join(pollock) %>%
              left_join(size) %>%
              left_join(circle_snow) %>% 
              left_join(sockeye) %>% 
  data.frame()

# Stage A - Save DF ============= 
write_csv(stage_a_cov, "data/processed_covariates/stage_a_all.csv")
  
# Stage B - Load data ============= 
fullness_df<-readRDS("data/processed_covariates/fullness_cov.RDS") %>%
  dplyr::rename(Year = "SampleYear_add1") %>%
  dplyr::select(Year, full_index_scale )  

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
  dplyr::rename(SST_CDD_GOA = "CDD",
                Year = "year") %>%
  dplyr::select(Year, SST_CDD_GOA)

stage_b_cov<- left_join(hatchery_pink_b,sst_b) %>%
              left_join(hatchery_chum_b) %>%
              left_join(fullness_df) %>%
  dplyr::mutate(brood_year = Year-2)

# Stage B - Save DF ============= 
write_csv(stage_b_cov, "data/processed_covariates/stage_b_all.csv")
