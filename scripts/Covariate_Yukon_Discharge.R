library(here)
library(tidyverse)
library(lubridate)

# source:  https://nwis.waterdata.usgs.gov/nwis/inventory/?site_no=15565447
# for juveniles, mid-june to mid july outflow 

# YUKON R AT PILOT STATION AK
# Latitude 61°56'04",   Longitude 162°52'50"   NAD27
# Wade Hampton Census Area County, Alaska, Hydrologic Unit 19090304
# Drainage area: 318,300 square miles
# Datum of gage: 20.00 feet above   NGVD29.

#currently data from the summer stops at 2018... 
yukon_discharge<- read_csv("data/yukon_discharge.csv")

yukon_a <- yukon_discharge %>%  
  dplyr::mutate( date = mdy(date),
                 year = year(date),
                month = month(date),
                day = day(date),
                discharge_cubic_ft_sec = as.numeric(discharge_cubic_ft_sec)) %>% 
        filter(month %in% c(6,7),
               case_when(month == 6 ~ !day < 15,
                         month == 7 ~ !day > 15,
                         TRUE ~ TRUE),
               !is.na(discharge_cubic_ft_sec))  %>%
  group_by(year) %>% 
  summarise(max_discharge = max(discharge_cubic_ft_sec),
            min_discharge = min(discharge_cubic_ft_sec),
            mean_discharge = mean(discharge_cubic_ft_sec))


# Covariate B for returning fish
yukon_b <- yukon_discharge %>%  
  dplyr::mutate( date = mdy(date),
                 year = year(date),
                 month = month(date),
                 day = day(date),
                 discharge_cubic_ft_sec = as.numeric(discharge_cubic_ft_sec)) %>% 
  filter(month %in% c(6,7,8),
         # case_when(month == 6 ~ !day < 15,
         #           month == 7 ~ !day > 15,
         #           TRUE ~ TRUE),
         !is.na(discharge_cubic_ft_sec))  %>%
  group_by(year) %>% 
  summarise(max_discharge = max(discharge_cubic_ft_sec),
            min_discharge = min(discharge_cubic_ft_sec),
            mean_discharge = mean(discharge_cubic_ft_sec))
   
 


write_csv(yukon_a,"data/processed_covariates/Stage_a_Yukon_drainage.csv")
write_csv(yukon_b,"data/processed_covariates/Stage_b_Yukon_drainage.csv")
# Notes from data ============
# ---------------------------------- WARNING ----------------------------------------
# Some of the data that you have obtained from this U.S. Geological Survey database
# may not have received Director's approval. Any such data values are qualified
# as provisional and are subject to revision. Provisional data are released on the
# condition that neither the USGS nor the United States Government may be held liable
# for any damages resulting from its use.
#
# Additional info: https://help.waterdata.usgs.gov/policies/provisional-data-statement
#
# File-format description:  https://help.waterdata.usgs.gov/faq/about-tab-delimited-output
# Automated-retrieval info: https://help.waterdata.usgs.gov/faq/automated-retrievals
#
# Contact:   gs-w_waterdata_support@usgs.gov
# retrieved: 2024-02-06 16:09:11 EST       (nadww01)
#
# Data for the following 1 site(s) are contained in this file
#    USGS 15565447 YUKON R AT PILOT STATION AK
# -----------------------------------------------------------------------------------
#
# Data provided for site 15565447
#            TS   parameter     statistic     Description
#          1152       00010     00001     Temperature, water, degrees Celsius (Maximum)
#          1153       00010     00002     Temperature, water, degrees Celsius (Minimum)
#          1154       00010     00003     Temperature, water, degrees Celsius (Mean)
#          1155       00060     00003     Discharge, cubic feet per second (Mean)
#
# Data-value qualification codes included in this output:
#        
#     A  Approved for publication -- Processing and review completed.
#     P  Provisional data subject to revision.
#     e  Value has been estimated.
#     4  Statistic computed from less than expected number of instantaneous values for the period
# 