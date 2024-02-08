library(tidyverse)
library(here)
library()

# load data =========================== 
chena <- read_csv("data/Little_Chena_Ridge_AirTemp.csv")
aniak <-read_csv("data/Aniak_Air_Temp.csv")

# Chena process data =========================== 
# first need to fix column names 
col_name <-data.frame(names = colnames(chena)) %>%
  separate(names , sep = 3, into =c("month", "delete")) %>%
  select(-delete) %>%
  slice(3:38)

temp <- data.frame(temp = rep(c("min", "max", "average"), times = 12))

new_colnames<- cbind(col_name,temp) %>%
  unite(c(1:2),  col= "month_temp")  

top =data.frame(month_temp = c("Year", "Day"))

new_colnames_final <- rbind(top, new_colnames)

colnames(chena) <-  new_colnames_final$month_temp

# now tidy and summarize dataset
chena_df <- chena %>%
  slice(-1) %>%
  gather(3:38, key = "label", value = "value") %>%
  separate(label, into = c("month", "temperature_type"), sep = 3) %>%
  separate(temperature_type, into = c("delete", "type"), sep = 1) %>%
  dplyr::select(-delete) %>%
  dplyr::mutate(value = as.numeric(value))  

chena_air_summer<- chena_df %>% 
                    filter(month %in% c("Jun","Jul"),
                           case_when(month == "Jun" ~ !Day < 15,
                                     month == "Jul" ~ !Day > 15,
                                     TRUE ~ TRUE),
                           !is.na(value)) %>% 
                    group_by(Year, type) %>%
                    dplyr::summarise(value = mean(value)) %>%
                    dplyr::mutate(id = "summer",
                                  site = "chena")

chena_air_iceoff <- chena_df %>% 
                      filter(month %in% c("Apr","May"),
                             !is.na(value)) %>% 
                      group_by(Year, type) %>%
                      dplyr::summarise(value = mean(value)) %>%
                      dplyr::mutate(id = "spring",
                                    site = "chena")
                           
chena_tidy <- rbind(chena_air_iceoff,chena_air_summer)

# From CSV: Aniak =============== 
# first need to fix column names - can use same names as chena

colnames(aniak) <-  new_colnames_final$month_temp

aniak_df <- aniak %>%
  slice(-1) %>%
  gather(3:38, key = "label", value = "value") %>%
  separate(label, into = c("month", "temperature_type"), sep = 3) %>%
  separate(temperature_type, into = c("delete", "type"), sep = 1) %>%
  dplyr::select(-delete) %>%
  dplyr::mutate(value = as.numeric(value))  

aniak_air_summer<- aniak_df %>% 
  filter(month %in% c("Jun","Jul"),
         case_when(month == "Jun" ~ !Day < 15,
                   month == "Jul" ~ !Day > 15,
                   TRUE ~ TRUE),
         !is.na(value)) %>% 
  group_by(Year, type) %>%
  dplyr::summarise(value = mean(value)) %>%
  dplyr::mutate(id = "summer",
                site = "aniak")

aniak_air_iceoff <- aniak_df %>% 
  filter(month %in% c("Apr","May"),
         !is.na(value)) %>% 
  group_by(Year, type) %>%
  dplyr::summarise(value = mean(value)) %>%
  dplyr::mutate(id = "spring", 
                site = "aniak")

aniak_tidy <- rbind(aniak_air_iceoff,aniak_air_summer)

# combine aniak and chena and save =========== 
temp_tidy <- rbind(aniak_tidy,chena_tidy)

write_csv(temp_tidy,"data/processed_covariates/Stage_A_airtemp.csv")


#------------------------------------------------- WARNING --------------------------------------------																																					
# 																																					
# The data you have obtained from this automated Natural Resources Conservation Service 																																					
# database are subject to revision regardless of indicated Quality Assurance level. 																																					
# Data are released on condition that neither the NRCS nor the United States Government 																																					
# may be held liable for any damages resulting from its use. 																																					
# 																																					
# SNOTEL air temperature data contains a known bias. This bias is rooted in the sensor 																																					
# conversion equation and varies through the output range. Solutions are in development. 																																					
# For more information go to Air Temperature Bias Correction. 																																					
# 																																					
# Help and Tutorials: https://www.nrcs.usda.gov/sites/default/files/2023-03/Report%20Generator%20Help%20Guide.pdf																																					
# Air Temperature Bias Correction: https://www.nrcs.usda.gov/wps/portal/wcc/home/snowClimateMonitoring/temperature/temperatureBiasCorrection/																																					
# 																																					
# Support Contact: usdafpacbc@servicenowservices.com																																					
# 																																					
#------------------------------------------------------------------------------------------------------																																					
# 																																					
# Reporting Frequency: Daily																																					
# Date Range: Period of Record																																					
#																																					
# Data for the following site(s) are contained in this file:																																					
#																																					
#       SNOTEL 2065: Aniak	 AK																																				
# 																																					
# Data items provided in this file:																																					
#																																					
# Element Name             Value Type  Function Type  Function Duration  Base Data  Measurement Units   Sensor Depth  Element Code  Description                                              																																					
# Air Temperature Minimum  Value       None           Day                N/A        Degrees fahrenheit  N/A           TMIN          Minimum air temperature - sub-hourly sampling frequency  																																					
# Air Temperature Maximum  Value       None           Day                N/A        Degrees fahrenheit  N/A           TMAX          Maximum air temperature - sub-hourly sampling frequency  																																					
# Air Temperature Average  Value       None           Day                N/A        Degrees fahrenheit  N/A           TAVG          Average air temperature - sub-hourly sampling frequency  																																					
# 																																					
# Quality Control flags included:																																					
#																																					
# Flag    Name                Description																																					
#  V      Valid               Validated Data																																					
#  N      No Profile          No profile for automated validation																																					
#  E      Edit                Edit	 minor adjustment for sensor noise																																				
#  B      Back Estimate       Regression-based estimate for homogenizing collocated Snow Course and Snow Pillow data sets																																					
#  K      Estimate            Estimate																																					
#  X      External Estimate   External estimate																																					
#  S      Suspect             Suspect data																																					
# 																																					
# Quality Assurance flags included:																																					
#																																					
# Flag    Name                Description																																					
#  U      Unknown             Unknown																																					
#  R      Raw                 No Human Review																																					
#  P      Provisional         Preliminary Human Review																																					
#  A      Approved            Processing and Final Review Completed																																					
# 																																					
#------------------------------------------------------------------------------------------------------																																					
# 																																					
# Aniak (2065) 																																					
# Alaska  SNOTEL Site - 80 ft																																					
# Reporting Frequency: Daily; Date Range: Period of Record																																					
#																																					
# As of: Feb 7	2024	 1:17:55 PM GMT-08:00																																			
#																																					
# From CSV: Chena =============== 
#------------------------------------------------- WARNING --------------------------------------------																																					
# 																																					
# The data you have obtained from this automated Natural Resources Conservation Service 																																					
# database are subject to revision regardless of indicated Quality Assurance level. 																																					
# Data are released on condition that neither the NRCS nor the United States Government 																																					
# may be held liable for any damages resulting from its use. 																																					
# 																																					
# SNOTEL air temperature data contains a known bias. This bias is rooted in the sensor 																																					
# conversion equation and varies through the output range. Solutions are in development. 																																					
# For more information go to Air Temperature Bias Correction. 																																					
# 																																					
# Help and Tutorials: https://www.nrcs.usda.gov/sites/default/files/2023-03/Report%20Generator%20Help%20Guide.pdf																																					
# Air Temperature Bias Correction: https://www.nrcs.usda.gov/wps/portal/wcc/home/snowClimateMonitoring/temperature/temperatureBiasCorrection/																																					
# 																																					
# Support Contact: usdafpacbc@servicenowservices.com																																					
# 																																					
#------------------------------------------------------------------------------------------------------																																					
# 																																					
# Reporting Frequency: Daily																																					
# Date Range: Period of Record																																					
#																																					
# Data for the following site(s) are contained in this file:																																					
#																																					
#       SNOTEL 947: Little Chena Ridge	 AK																																				
# 																																					
# Data items provided in this file:																																					
#																																					
# Element Name             Value Type  Function Type  Function Duration  Base Data  Measurement Units   Sensor Depth  Element Code  Description                                              																																					
# Air Temperature Minimum  Value       None           Day                N/A        Degrees fahrenheit  N/A           TMIN          Minimum air temperature - sub-hourly sampling frequency  																																					
# Air Temperature Maximum  Value       None           Day                N/A        Degrees fahrenheit  N/A           TMAX          Maximum air temperature - sub-hourly sampling frequency  																																					
# Air Temperature Average  Value       None           Day                N/A        Degrees fahrenheit  N/A           TAVG          Average air temperature - sub-hourly sampling frequency  																																					
# 																																					
# Quality Control flags included:																																					
#																																					
# Flag    Name                Description																																					
#  V      Valid               Validated Data																																					
#  N      No Profile          No profile for automated validation																																					
#  E      Edit                Edit	 minor adjustment for sensor noise																																				
#  B      Back Estimate       Regression-based estimate for homogenizing collocated Snow Course and Snow Pillow data sets																																					
#  K      Estimate            Estimate																																					
#  X      External Estimate   External estimate																																					
#  S      Suspect             Suspect data																																					
# 																																					
# Quality Assurance flags included:																																					
#																																					
# Flag    Name                Description																																					
#  U      Unknown             Unknown																																					
#  R      Raw                 No Human Review																																					
#  P      Provisional         Preliminary Human Review																																					
#  A      Approved            Processing and Final Review Completed																																					
# 																																					
#------------------------------------------------------------------------------------------------------																																					
# 																																					
# Little Chena Ridge (947) 																																					
# Alaska  SNOTEL Site - 2000 ft																																					
# Reporting Frequency: Daily; Date Range: Period of Record																																					
#																																					
# As of: Feb 7	2024	 12:53:28 PM GMT-08:00																																			
#																																					