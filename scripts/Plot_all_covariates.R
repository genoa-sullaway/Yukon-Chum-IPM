# Plot all covariates in one plot 
library(tidyverse)
library(here)

# Stage A - Load data ============= 

sst_a <- read_csv("data/processed_covariates/Stage_A_CDD.csv")
large_zoop_a <- read_csv("data/processed_covariates/covariate_large_zooplankton.csv")
gelatinous_zoop_a <- read_csv("data/processed_covariates/covariate_gelatinous_zooplankton.csv")
air_temp_a <- read_csv("data/processed_covariates/Stage_A_airtemp.csv")
 
# Stage B - Load data ============= 
hatchery_chum_b<-read_csv("output/hatchery_Chum_Covariate_AKandAsia.csv")
hatchery_pink_b <- read_csv("output/hatchery_Pink_Covariate_AKandAsia.csv")
sst_b<-read_csv("data/processed_covariates/Stage_B_CDD.csv")

# Stage A - Plots ============= 

plot_CDD <- ggplot(data = sst_a) +
  geom_point(aes(x=year, y = CDD)) +
  geom_line(aes(x=year, y = CDD)) +
  theme_classic() +
  geom_hline(yintercept = mean(sst_a$CDD), linetype =2) +
  ylab("NBS Cumulative Degree Days") +
  xlab("Year") + 
  ggtitle("Stage A - CDD NBS") +
  labs(caption = "horizontal line is the mean for timeseries")




