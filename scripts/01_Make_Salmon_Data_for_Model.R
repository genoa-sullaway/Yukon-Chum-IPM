# this pulls in data from each run, converts calendar year to brood year and runs some QAQC plots 
# currently just experimenting with fall_yukon bc that has a complete dataset

#THIS IS NOT DONE ---- WAITING FOR REST OF AGE COMP DATA.

library(tidyverse)
library(here)

# Spawner age comp =======
age_comp<-(read_excel("data/age_comps/age_comp_assimilated.xlsx")) %>%
   mutate(percent = as.numeric(percent)) %>%
  select(-brood_year, -location_b) %>%
   as.data.frame() %>%
   filter(location_a == "fall_yukon") %>%
   spread(age, percent)

# Spawners =======
yukon_summer <- read_excel("data/Yukon_Escapement_ADFG/Yukon Summer Chum Total Run 1978-2022 Run Rec.xlsx") %>%
  dplyr::rename(cal_year = "Year",
       est_run = `Total Run`) %>% 
  dplyr::select(cal_year, est_run) %>%
  dplyr::mutate(id = 1)

yukon_fall<- read_csv("data/Yukon_Escapement_ADFG/Yukon_Fall_Chum_RR_JTC.csv")  %>%
  rename(cal_year = "Year",
         est_run = "Estimated_Run") %>% 
  dplyr::select(cal_year, est_run) %>%
#  filter(Year > 2001) %>%
  dplyr::mutate(id = 2) %>%
  left_join(age_comp) %>%
  dplyr::mutate(age_3_abundance = abund_0.3 *est_run,
                age_4_abundance = abund_0.4 *est_run,
                age_5_abundance = abund_0.5 *est_run,
                age_6_abundance = abund_0.6 *est_run) %>%
  dplyr::select(cal_year, est_run,age_3_abundance,age_4_abundance,age_5_abundance,age_6_abundance) %>%
  gather(3:6, key = "id", value = "abundance") %>%
  dplyr::mutate(brood_year = case_when(id == "age_3_abundance" ~ cal_year-3,
                                id == "age_4_abundance" ~ cal_year-4,
                                id == "age_5_abundance" ~ cal_year-5,
                                id == "age_6_abundance" ~ cal_year-6)) %>% 
  dplyr::select(brood_year, id, abundance) %>%
  spread(id, abundance) %>% 
  filter(!brood_year < 1974)
# 
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
  #filter(!reporting_group == "Kusko_Bristol") %>%
  dplyr::mutate(id = case_when(reporting_group == "Yukon_Summer" ~ 1,
                               reporting_group == "Yukon_Fall" ~ 2,
                               TRUE ~ 3)) %>%
  # group_by(reporting_group) %>% 
  # dplyr::mutate(juv_index = as.numeric(scale(juv_index))) %>% 
  ungroup() %>%
  dplyr::select(-reporting_group) %>% 
  spread(id, juv_index) 

juv_prop_ayk[19,2]<- colMeans(juv_prop_ayk[-19,])[2] # get means of all columns except 2020, fill that in for 2020
juv_prop_ayk[19,3]<- colMeans(juv_prop_ayk[-19,])[3] # get means of all columns except 2020, fill that in for 2020
juv_prop_ayk[19,4]<- colMeans(juv_prop_ayk[-19,])[4]

# QAQC PLOTS =========== 
yukon_fall %>% 
  gather(2:5, key = "key", value = "value") %>% 
  ggplot(aes(x = brood_year, y = value, color = key, fill =key)) + 
     geom_bar(position="stack", stat="identity" )

yukon_fall %>% 
  gather(2:5, key = "key", value = "value") %>% 
  ggplot(aes(x = brood_year, y = value, color = key, group =key)) + 
    geom_line() +
  theme_classic( )

yukon_fall %>% 
  gather(2:5, key = "key", value = "value") %>% 
  filter(!brood_year < 1980) %>%
  ggplot(aes(x = brood_year, y = value, color = key, group =key)) + 
  geom_line() +
  theme_classic( ) +
  facet_wrap(~key, scales = "free")


yukon_fall %>% 
  gather(2:5, key = "key", value = "value") %>% 
  filter(!brood_year < 1980) %>%
  group_by(key) %>%
  mutate(value = scale(value)) %>%
  ggplot(aes(x = brood_year, y = value, color = key, group =key)) + 
  geom_line() +
  theme_classic( ) +
  geom_hline(yintercept = 0)
  