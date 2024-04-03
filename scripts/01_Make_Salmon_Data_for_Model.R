# this pulls in data from each run, converts calendar year to brood year and runs some QAQC plots 
# this is done for Yukon - final format is abundance per brood year and age class 
library(tidyverse)
library(here)
library(readxl)

yukon_summer_df<-read_excel("data/Yukon_Escapement_ADFG/S Chum RR 2023.xlsx",sheet = 2)  

# Abundances ========
## Summer ========
yukon_summer <-  yukon_summer_df %>% 
  select(1:4) %>% 
  janitor::row_to_names(row_number = 1) %>%
  dplyr::rename(cal_year = "Year",
                total_run =`Total Run`,
                Spawners="Escapement" )  %>%
  dplyr::mutate(Spawners = as.numeric(Spawners),
                Harvest = as.numeric(Harvest),
                cal_year=as.numeric(cal_year),
                total_run = as.numeric(total_run))

yukon_summer_spawners <- yukon_summer %>%
  select(cal_year, Spawners)

yukon_summer_harvest <- yukon_summer %>%
  dplyr::rename(harvest = "Harvest") %>% 
  select(cal_year, harvest)

yukon_summer_recruits <- yukon_summer %>%
  select(cal_year, total_run)

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

# age comps ========
## Summer ========
yukon_summer_age_abund<- yukon_summer_df %>%
  dplyr::select(1,6:9) %>% 
  janitor::row_to_names(row_number = 1) %>%
  dplyr::rename(cal_year = "Year") %>%  
  gather(2:5, key = "age", value = "abundance") %>% 
  dplyr::mutate(cal_year = as.numeric(cal_year),
                abundance = as.numeric(abundance)) %>%
  spread(age, abundance) 
# 
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
  select(cal_year,age,percent) %>%
  spread(age, percent)

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
  spread(id, juv_index) %>%
  dplyr::rename(brood_year = "Year")

juv_prop_ayk[19,2]<- colMeans(juv_prop_ayk[-19,])[2] # get means of all columns except 2020, fill that in for 2020
juv_prop_ayk[19,3]<- colMeans(juv_prop_ayk[-19,])[3] # get means of all columns except 2020, fill that in for 2020
juv_prop_ayk[19,4]<- colMeans(juv_prop_ayk[-19,])[4]

write_csv(juv_prop_ayk, "data/tidy_BASIS_AYK_model.csv")


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
 