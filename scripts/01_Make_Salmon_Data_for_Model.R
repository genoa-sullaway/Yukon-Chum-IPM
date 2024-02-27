# this pulls in data from each run, converts calendar year to brood year and runs some QAQC plots 
# this is done for Yukon - final format is abundance per brood year and age class 
library(tidyverse)
library(here)
library(readxl)

# Make Spawner age comp =======
  ## Fall Yukon =======
  # created in make_fall_yukon_age_comp.R - source from JTC report
fall_age_comp <-read_csv("data/age_comps/processed_age_comps_Fallyukon_source4.csv") %>%
  dplyr::select(brood_year, age, percent) %>% 
  spread(age,percent) 

yukon_fall<- read_csv("data/Yukon_Escapement_ADFG/Yukon_Fall_Chum_RR_JTC.csv")  %>%
  rename(brood_year = "Year",
         spawners = "Spawners") %>% 
  dplyr::select(brood_year, spawners) %>%
  left_join(fall_age_comp) %>%
  dplyr::mutate(age3 = abund_0.3 *spawners,
                age4 = abund_0.4 *spawners,
                age5 = abund_0.5 *spawners,
                age6 = abund_0.6 *spawners) %>%
  dplyr::select(brood_year, spawners,age3,age4,age5,age6) %>%
  gather(3:6, key = "id", value = "abundance") %>% 
  dplyr::select(brood_year, id, abundance) %>%
  spread(id, abundance) %>% 
  dplyr::mutate(id = "fall")


  ## Summer Yukon =======
  # sent to me separately, by Fred west 2-22-2024
summer_age_comp<-read_csv("data/age_comps/processed_age_comps_summer_yukon.csv")  
 
yukon_summer <- read_excel("data/Yukon_Escapement_ADFG/S Chum RR 2023.xlsx", sheet = 2) %>%
  select(1:4) %>% 
  janitor::row_to_names(row_number = 1)

yukon_summer_sp <- yukon_summer%>%
  dplyr::rename(cal_year = "Year",
       spawners = "Escapement") %>% 
  dplyr::select(cal_year, spawners)  %>%
  dplyr::mutate(cal_year = as.numeric(cal_year),
                spawners = as.numeric(spawners)) %>% 
  left_join(summer_age_comp) %>%
  gather(3:6, key = "age", value = "percent") %>%
  dplyr::mutate(abundance = percent*spawners,
         brood_year = case_when(age == "age3" ~ cal_year -3,
                                age == "age4" ~ cal_year -4,
                                age == "age5" ~ cal_year -5,
                                age == "age6" ~ cal_year -6)) %>%
  dplyr::select(brood_year, age, abundance) %>%
  spread(age,abundance) %>% 
  mutate(id = "summer")
  

yukon_spawners = rbind(yukon_fall, yukon_summer_sp)

# kusko_estimated_parameters<- readRDS("output/optim_output_par_data2021.RDS")
# kusko<-data.frame(Year = c(1988:(2022-1)),
#                   Estimated_Run= as.vector(c(kusko_estimated_parameters[2:35])),
#                   id =3) %>% 
#   filter(Year > 2001) 
# 
# mean<-mean(kusko$Estimated_Run)
# kusko<-kusko %>%
#   rbind(df=data.frame(Year = c(2022), Estimated_Run = c(mean), id=c(3)))
# 
# sp <- rbind(yukon_summer, yukon_fall, kusko) %>%
#   spread(id, Estimated_Run)

# Juveniles ========================================================
juv <- read_csv("data/Juv_Index_CC_aug2023/Index2.csv") %>%
  dplyr::select(Time, Estimate) %>%
  rename(Year = "Time") 

# Make proportion of juveniles per run ======================================
mean_prop<- read_csv("data/mean_prop_basis.csv") # see "script/explore_basis_proportions.R"

juv_prop_ayk <- expand_grid(juv, mean_prop) %>%
  dplyr::mutate(juv_index = mean*Estimate) %>%
  dplyr::select(Year, reporting_group, juv_index) %>%
  dplyr::mutate(id = case_when(reporting_group == "Yukon_Summer" ~ 1,
                               reporting_group == "Yukon_Fall" ~ 2,
                               TRUE ~ 3)) %>%
  ungroup() %>%
  dplyr::select(-reporting_group) %>% 
  spread(id, juv_index) 

juv_prop_ayk[19,2]<- colMeans(juv_prop_ayk[-19,])[2] # get means of all columns except 2020, fill that in for 2020
juv_prop_ayk[19,3]<- colMeans(juv_prop_ayk[-19,])[3] # get means of all columns except 2020, fill that in for 2020
juv_prop_ayk[19,4]<- colMeans(juv_prop_ayk[-19,])[4]

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
 