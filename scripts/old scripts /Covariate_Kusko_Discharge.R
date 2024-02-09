library(here)
library(tidyverse)
library(lubridate)

# source:  https://nwis.waterdata.usgs.gov/nwis/inventory/?site_no=15565447
# for juveniles, mid-june to mid july outflow 

# Kuskokwim R at Crooked Creek AK - 15304000
 
kusko_discharge<- read_csv("data/kusko_discharge.csv") %>%
  filter(!is.na(discharge_cubic_ft_sec)) %>%
  group_by(date) %>%
  mutate(discharge_cubic_ft_sec = as.numeric(discharge_cubic_ft_sec)) %>%
  summarise(discharge_cubic_ft_sec = mean(discharge_cubic_ft_sec))

kusko_a <- kusko_discharge %>%  
  dplyr::mutate(date = mdy(date),
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
kusko_b <- kusko_discharge %>%  
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
   
 


write_csv(kusko_a,"data/processed_covariates/Stage_a_kusko_drainage.csv")
write_csv(kusko_b,"data/processed_covariates/Stage_b_kusko_drainage.csv")

# Notes from data ============
# ---------------------------------- WARNING ----------------------------------------
# Some of the data that you have obtained from this U.S. Geological Survey database may not 
# have received Director's approval.  Any such data values are qualified as provisional and 
# are subject to revision.  Provisional data are released on the condition that neither the 
# USGS nor the United States Government may be held liable for any damages resulting from its use.
#  Go to http://help.waterdata.usgs.gov/policies/provisional-data-statement for more information.
#
# File-format description:  http://help.waterdata.usgs.gov/faq/about-tab-delimited-output
# Automated-retrieval info: http://help.waterdata.usgs.gov/faq/automated-retrievals
#
# Contact:   gs-w_support_nwisweb@usgs.gov
# retrieved: 2024-02-06 17:23:15 -05:00	(nadww02)
#
# Data-value grade codes included in this output:
#    91    IV verification DV <= 1 percent diff
#    92    IV verification DV <= 5 percent diff
#    93    IV verification DV <= 10 percent diff
#
# Data for the following 1 site(s) are contained in this file
#    USGS 15304000 KUSKOKWIM R AT CROOKED CREEK AK
# -----------------------------------------------------------------------------------
#
# TS_ID - An internal number representing a time series.
#
# Data provided for site 15304000
#    TS_ID       Parameter Description
#    1681        00060     Discharge, cubic feet per second
#
# Data-value qualification codes included in this output:
#     A  Approved for publication -- Processing and review completed.
#     e  Value has been estimated.
#     P  Provisional data subject to revision.
#     Ice  Value is affected by ice at the measurement site.
#