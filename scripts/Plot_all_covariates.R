# Plot all covariates in one plot 
library(tidyverse)
library(here)

# Stage A - Load data ============= 

# sst_a <- read_csv("data/processed_covariates/Stage_A_CDD.csv")
# large_zoop_a <- read_csv("data/processed_covariates/covariate_large_zooplankton.csv")
# gelatinous_zoop_a <- read_csv("data/processed_covariates/covariate_gelatinous_zooplankton.csv")
# air_temp_a <- read_csv("data/processed_covariates/Stage_A_airtemp.csv")
#  
# # Stage B - Load data ============= 
# hatchery_chum_b<-read_csv("output/hatchery_Chum_Covariate_AKandAsia.csv")
# hatchery_pink_b <- read_csv("output/hatchery_Pink_Covariate_AKandAsia.csv")
# sst_b<-read_csv("data/processed_covariates/Stage_B_CDD.csv")

# 
stage_a_cov <- read_csv("data/processed_covariates/stage_a_all.csv") %>%
  filter(Year >= year_min, 
         Year <= year_max_brood+1) %>%
  dplyr::mutate(id = "A",
                brood_year = Year,
                yukon_mean_discharge = as.numeric(scale(yukon_mean_discharge)),
                SST_CDD_NBS = as.numeric(scale(SST_CDD_NBS))) %>%
  dplyr::select(brood_year,
                SST_CDD_NBS,# yukon_mean_discharge,Cnideria,
                Large_zoop,
                Cnideria
                #,yukon_mean_discharge 
  ) %>% 
  gather(2:4, id = "variable", value = "value" )

temp_b_cov <- read_csv("data/processed_covariates/stage_b_all.csv") %>%
  filter(brood_year >= year_min, 
         brood_year <= year_max_brood+2) %>% 
  dplyr::mutate(id = "B",
                SST_CDD_GOA = as.numeric(scale(SST_CDD_GOA)),
                Chum_hatchery= as.numeric(scale(Chum_hatchery)),
                Pink_hatchery= as.numeric(scale(Pink_hatchery))#,
                #yukon_mean_discharge_summer= as.numeric(scale(yukon_mean_discharge_summer))
  ) %>% 
  dplyr::select(brood_year,
                SST_CDD_GOA,
                Chum_hatchery, 
                Pink_hatchery)


bind <- temp_b_cov %>% slice(22)

all_cov <- temp_b_cov %>%
  rbind(bind) %>%  # add another row because t+a+1 is 2024, so this is basically a dummy row for the last year of fish...
  gather(2:4, id = "variable", value = "value") %>% 
  rbind(stage_a_cov)

# compare GOA SST and NBS SST - covariance =======

ggplot(all_cov %>% filter(variable %in% c("SST_CDD_GOA","SST_CDD_NBS"))) +
         geom_point(aes(x = brood_year, y = value, group = variable)) +
         geom_line(aes(x = brood_year, y = value, group = variable)) 


# # Stage A - Plots ============= 
# plot_CDD <- ggplot(data = sst_a) +
#   geom_point(aes(x=year, y = CDD)) +
#   geom_line(aes(x=year, y = CDD)) +
#   theme_classic() +
#   geom_hline(yintercept = mean(sst_a$CDD), linetype =2) +
#   ylab("NBS Cumulative Degree Days") +
#   xlab("Year") + 
#   ggtitle("Stage A - CDD NBS") +
#   labs(caption = "horizontal line is the mean for timeseries")
# 
# 
