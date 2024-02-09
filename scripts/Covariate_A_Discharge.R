library(here)
library(tidyverse)
library(lubridate)

# Kusko ===================================
# source:  https://nwis.waterdata.usgs.gov/nwis/inventory/?site_no=15565447
# for juveniles, mid-june to mid july outflow 

# Kuskokwim R at Crooked Creek AK - 15304000
 
kusko_discharge<- read_csv("data/kusko_discharge.csv") %>%
  filter(!is.na(discharge_cubic_ft_sec),
         !discharge_cubic_ft_sec == "Ice") %>%
  group_by(date) %>%
  dplyr::mutate(discharge_cubic_ft_sec = as.numeric(discharge_cubic_ft_sec)) %>%
  dplyr::summarise(discharge_cubic_ft_sec = mean(discharge_cubic_ft_sec))

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
  dplyr::summarise(max_discharge = max(discharge_cubic_ft_sec),
            min_discharge = min(discharge_cubic_ft_sec),
            mean_discharge = mean(discharge_cubic_ft_sec)) %>%
  dplyr::mutate(id="Kusko")


# Yukon =================================== 

# source:  https://nwis.waterdata.usgs.gov/nwis/inventory/?site_no=15565447
# for juveniles, mid-june to mid july outflow 

# YUKON R AT PILOT STATION AK
# Latitude 61°56'04",   Longitude 162°52'50"   NAD27
# Wade Hampton Census Area County, Alaska, Hydrologic Unit 19090304
# Drainage area: 318,300 square miles
# Datum of gage: 20.00 feet above   NGVD29.

#currently data from the summer --- lots of weirdness with formatting that needs to be fixed
yukon_discharge<- read_csv("data/yukon_discharge.csv") 

slash <- yukon_discharge %>%
              filter(grepl("/",date),
                     !discharge_cubic_ft_sec == "Eqp") %>%
          dplyr::mutate(date = mdy(date),
                        year = year(date),
                        month = month(date),
                        day = day(date),
                        discharge_cubic_ft_sec = as.numeric(discharge_cubic_ft_sec)) %>%
  dplyr::select(-date,-data_quality)
  

dash <- yukon_discharge %>%
  filter(grepl("-",date)) %>%
  separate(date, sep = " ", into = c("date", "del1", "del2", "del3")) %>%
  dplyr::select(-del1, -del2, -del3) %>%
  separate(discharge_cubic_ft_sec,into = c( "del1", "del2", "del3", "del4", "discharge_cubic_ft_sec")) %>%
  dplyr::select(date, discharge_cubic_ft_sec) %>%
  filter(!is.na(discharge_cubic_ft_sec)) %>%
  dplyr::mutate(date = ymd(date) ,
                year = year(date),
                month = month(date),
                day = day(date),
                discharge_cubic_ft_sec = as.numeric(discharge_cubic_ft_sec)) %>%
  dplyr::select(-date)

yukon_fixed<-rbind(dash,slash)
 
yukon_a <- yukon_fixed %>%  
  filter(month %in% c(6,7),
         case_when(month == 6 ~ !day < 15,
                   month == 7 ~ !day > 15,
                   TRUE ~ TRUE),
         !is.na(discharge_cubic_ft_sec))  %>%
  group_by(year) %>% 
  summarise(max_discharge = max(discharge_cubic_ft_sec),
            min_discharge = min(discharge_cubic_ft_sec),
            mean_discharge = mean(discharge_cubic_ft_sec)) %>%
  dplyr::mutate(id="Yukon")

# Combine yukon kusko ============================ 
discharge_both <- rbind(yukon_a, kusko_a)

write_csv(discharge_both,"data/processed_covariates/Stage_A_YK_Discharge.csv")




