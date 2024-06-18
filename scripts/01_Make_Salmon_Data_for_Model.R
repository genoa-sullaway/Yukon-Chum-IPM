# this pulls in data from each run, converts calendar year to brood year and runs some QAQC plots 
# this is done for Yukon - final format is abundance per brood year and age class 
library(tidyverse)
library(here)
library(readxl)

yukon_summer_df<-read_excel("data/Yukon_Escapement_ADFG/S Chum RR 2023.xlsx",sheet = 2)  

# spawner harvest recruit Abundances ========
## Summer ========
# yukon_summer <-  yukon_summer_df %>% 
#   select(1:4) %>% 
#   janitor::row_to_names(row_number = 1) %>%
#   dplyr::rename(cal_year = "Year",
#                 total_run =`Total Run`,
#                 Spawners="Escapement" )  %>%
#   dplyr::mutate(Spawners = as.numeric(Spawners),
#                 Harvest = as.numeric(Harvest),
#                 cal_year=as.numeric(cal_year),
#                 total_run = as.numeric(total_run))
# 
# yukon_summer_spawners <- yukon_summer %>%
#   select(cal_year, Spawners)
# 
# yukon_summer_harvest <- yukon_summer %>%
#   dplyr::rename(harvest = "Harvest") %>% 
#   select(cal_year, harvest)
# 
# yukon_summer_recruits <- yukon_summer %>%
#   select(cal_year, total_run)

## Fall ========
yukon_fall<- read_csv("data/Yukon_Escapement_ADFG/Yukon_Fall_Chum_RR_JTC.csv") %>%  
 dplyr::rename(cal_year = "Year")  

yukon_fall_spawners <- yukon_fall %>%
  dplyr::select(cal_year, Spawners)

yukon_fall_harvest <- yukon_fall %>%
  dplyr::mutate(harvest = Estimated_Run-Spawners) %>% 
  dplyr::select(cal_year, harvest)

yukon_fall_recruits <- yukon_fall %>%
  dplyr::rename(total_run = "Estimated_Run") %>% 
  dplyr::select(cal_year, total_run)

write_csv(yukon_fall_spawners, "data/processed_data/yukon_fall_spawners.csv")

write_csv(yukon_fall_harvest, "data/processed_data/yukon_fall_harvest.csv")

write_csv(yukon_fall_recruits,"data/processed_data/yukon_fall_recruits.csv")
 
# age comps ========
## Summer ========
# yukon_summer_age_abund<- yukon_summer_df %>%
#   dplyr::select(1,6:9) %>% 
#   janitor::row_to_names(row_number = 1) %>%
#   dplyr::rename(cal_year = "Year") %>%  
#   gather(2:5, key = "age", value = "abundance") %>% 
#   dplyr::mutate(cal_year = as.numeric(cal_year),
#                 abundance = as.numeric(abundance)) %>%
#   spread(age, abundance) 
 
# yukon_summer <- yukon_summer_df %>%
#   select(1:4) %>% 
#   janitor::row_to_names(row_number = 1)
# 
# yukon_summer_sp <- yukon_summer %>%
#   dplyr::rename(cal_year = "Year",
#                 spawners = "Escapement") %>% 
#   dplyr::select(cal_year, spawners)  %>%
#   dplyr::mutate(cal_year = as.numeric(cal_year),
#                 spawners = as.numeric(spawners)) %>% 
#   left_join(yukon_summer_age_abund) %>%
#   gather(3:6, key = "age", value = "percent") %>%
#   dplyr::mutate(abundance = percent*spawners,
#                 brood_year = case_when(age == "age3" ~ cal_year -3,
#                                        age == "age4" ~ cal_year -4,
#                                        age == "age5" ~ cal_year -5,
#                                        age == "age6" ~ cal_year -6)) %>%
#   dplyr::select(brood_year, age, abundance) %>%
#   spread(age,abundance) %>% 
#   dplyr::mutate(id = "summer")
# 
# write_csv(yukon_summer_sp, "output/yukon_summer_broodyear.csv")

## Fall ========
yukon_fall_ages <- readxl::read_excel("data/age_comps/Fall_Yukon_Calc_Source4.xlsx") %>%  # created in make_fall_yukon_age_comp.R - source from JTC report
  select(c(1:5)) %>% 
  gather(2:5, key = "age", value = "abund") %>%
  mutate(cal_year = case_when(age == "abund_0.3" ~ brood_year +3,
                              age == "abund_0.4" ~ brood_year +4,
                              age == "abund_0.5" ~ brood_year +5,
                              age == "abund_0.6" ~ brood_year +6)) %>%
  group_by(cal_year) %>%
  dplyr::mutate(sum = sum(abund),
                percent = abund/sum) %>%
  dplyr::select(cal_year,age,percent) %>%
  spread(age, percent)

write_csv(yukon_fall_ages, "data/processed_data/yukon_fall_age_comp.csv")

# Juveniles ========================================================
juv <- read_csv("data/Juv_Index_CC_aug2023/Index2.csv") %>%
  dplyr::select(Time, Estimate,CV) %>%
  rename(Year = "Time") 

# Proportion of juveniles per run ======================================
# Use Fall Yukon proportions 
# liz lee emailed this data in april 2024, see "script/explore_basis_proportions.R" for some exploratory info
fall_juv_proportions <- read_excel("data/BeringSea_Chum_Juv_annual_2003-2023_analysis_msa.xlsx") %>% 
  janitor::row_to_names(row_number = 1) %>%
  rename(reporting_group = `Reporting Group`) %>%
  mutate(Year = as.numeric(Year),
         Mean = as.numeric(Mean),
         SD = as.numeric(SD),
         reporting_group = case_when(reporting_group == "Yukon River Fall Run"~ "Yukon_Fall",
                                     reporting_group == "Coastal Western Alaska"~ "CWAK",
                                     TRUE ~ reporting_group )) %>%
  dplyr::select(1:4) %>%
  filter(reporting_group %in% c("Yukon_Fall"))
 
rollingmean_2020 <- juv %>% 
  filter(Year %in% c(2019,2021)) %>% 
  summarise(Mean = mean(Estimate))

# stand in guess! 
rollingmean_GSI_2020 <- 0.15 # fall_juv_proportions %>% 
  # filter(Year %in% c(2019,2021)) %>% 
  # summarise(Mean = mean(Mean))

rollingmean_GSI_2013 <- fall_juv_proportions %>% 
  filter(Year %in% c(2010,2011,2012)) %>% 
  summarise(Mean = mean(Mean))

rollingmean_GSI_2002 <- fall_juv_proportions %>% 
  filter(Year %in% c(2003,2004,2005)) %>% 
  dplyr::summarise(Mean = mean(Mean))

rollingmean_GSI_2008 <- fall_juv_proportions %>%  
  dplyr::summarise(Mean = mean(Mean))

fall_juv <- left_join(juv, fall_juv_proportions)  %>%
  mutate(Mean = case_when(Year == 2013 ~ rollingmean_GSI_2013$Mean,
                          Year == 2002 ~ rollingmean_GSI_2002$Mean,
                          Year == 2020 ~ 0.15,
                          Year %in% c(2008, 2009) ~  rollingmean_GSI_2008$Mean, 
                          TRUE ~ Mean),
         Estimate = case_when(Estimate==0 ~  rollingmean_2020$Mean,
                          TRUE ~ Estimate),
         fall_abundance = Estimate * Mean) %>%
  dplyr::select(Year, fall_abundance,CV)

 ggplot(data = fall_juv) +
   geom_line(aes(x=Year ,y = fall_abundance))

 # SAVE HERE ============
write_csv(fall_juv, "data/processed_data/tidy_juv_fall_yukon.csv")

# QAQC PLOTS =========== 
## Spawners ============
yukon_spawners %>% 
  gather(2:5, key = "key", value = "value") %>% 
  ggplot(aes(x = brood_year, y = value, color = key, group =key)) + 
    geom_line() +
    theme_classic( ) +
    facet_wrap(~id)


yukon_spawners %>% 
  gather(2:5, key = "key", value = "value") %>% 
  ggplot(aes(x = brood_year, y = value, color = id, group =id)) + 
  geom_line() +
  theme_classic( ) +
  facet_wrap(~key, scales = "free")
 