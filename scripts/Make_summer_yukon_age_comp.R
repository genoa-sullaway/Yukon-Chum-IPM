library(tidyverse)
library(here)
 
yukon_summer <- read_excel("data/Yukon_Escapement_ADFG/S Chum RR 2023.xlsx", sheet = 2) %>%
  dplyr::select(1,6:9) %>% 
  janitor::row_to_names(row_number = 1) %>%
  dplyr::rename(cal_year = "Year") %>%  
  gather(2:5, key = "age", value = "abundance") %>% 
  dplyr::mutate(cal_year = as.numeric(cal_year),
                abundance = as.numeric(abundance)) %>%
  spread(age, abundance) 

#brood year age comps ----- 
# yukon_summer <- read_excel("data/Yukon_Escapement_ADFG/S Chum RR 2023.xlsx", sheet = 2) %>%
#   dplyr::select(1,11:14) %>% 
#   janitor::row_to_names(row_number = 1) %>%
#   dplyr::rename(cal_year = "Year") %>%  
#   gather(2:5, key = "age", value = "abundance") %>% 
#   dplyr::mutate(cal_year = as.numeric(cal_year),
#                 abundance = as.numeric(abundance), 
#                 brood_year = case_when(age == "age3" ~ cal_year -3,
#                                  age == "age4" ~ cal_year -4,
#                                  age == "age5" ~ cal_year -5,
#                                  age == "age6" ~ cal_year -6)) %>%
#   dplyr::select(-cal_year) %>%
#   group_by(brood_year) %>% 
#   dplyr::mutate(sum = sum(abundance),
#                 percent = abundance/sum) %>%
#   dplyr::select(-abundance,-sum) %>%
#   spread(age,percent)
   
write_csv(yukon_summer,"data/age_comps/processed_age_comps_summer_yukon.csv")
