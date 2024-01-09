# covariates will need to be checked as model changes, this was copied from model script so all covariates came over the same form they would go in the model
library(rstan)
library(tidyverse)
library(here)
library(bayesplot)
library(rstanarm) 

library(RNetCDF)
library(tidync)
library(lubridate) 

# Load data =======================================================
# Spawners =======
yukon_summer <- read_excel("data/Yukon_Escapement_ADFG/Yukon Summer Chum Total Run 1978-2022 Run Rec.xlsx") %>%
  dplyr::select(Year, `Total Run`) %>%
  dplyr::rename(Estimated_Run = `Total Run`) %>%
  filter(Year > 2001) %>%
  dplyr::mutate(id = 1)#,
#Estimated_Run = as.numeric(scale(Estimated_Run)))

yukon_fall<- read_csv("data/Yukon_Escapement_ADFG/Yukon_Fall_Chum_RR_JTC.csv")  %>%
  select(Year, Estimated_Run) %>%
  filter(Year > 2001) %>%
  dplyr::mutate(id = 2)#,
#Estimated_Run = as.numeric(scale(Estimated_Run)))

kusko_estimated_parameters<- readRDS("output/optim_output_par_data2021.RDS")
kusko<-data.frame(Year = c(1988:(2022-1)),
                  Estimated_Run= as.vector(c(kusko_estimated_parameters[2:35])),
                  id =3) %>% 
  filter(Year > 2001) #%>%
#mutate(Estimated_Run = as.numeric(scale(Estimated_Run)))

mean<-mean(kusko$Estimated_Run)
kusko<-kusko %>%
  rbind(df=data.frame(Year = c(2022), Estimated_Run = c(mean), id=c(3)))

sp <- rbind(yukon_summer, yukon_fall, kusko) 

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
  dplyr::select(-reporting_group) #%>% 
#  spread(id, juv_index) 
# 
# juv_prop_ayk[19,2]<- colMeans(juv_prop_ayk[-19,])[2] # get means of all columns except 2020, fill that in for 2020
# juv_prop_ayk[19,3]<- colMeans(juv_prop_ayk[-19,])[3] # get means of all columns except 2020, fill that in for 2020
# juv_prop_ayk[19,4]<- colMeans(juv_prop_ayk[-19,])[4]

# load Covariates  ==========================================================
# covariates for stage 1 =======================
# theta 1 -- Zooplankton 
# this was created in "scripts/Explore_Zoop.R" 
# currently this is a themisto and calanus large copepod sum mean abundance for Fall, not a true index. 
# I also have a gelatinous zoop abundance i could add in
zoop_temp <- read_csv("data/covariate_large_zooplankton.csv") %>% 
  dplyr::select(YEAR, mean) 

# this is currently BS just to get enough years... need to ask for an expanded dataset 
insert <- data.frame(YEAR = c(2001,2020,2021,2022), 
                     mean = rep(rnorm(4, mean(zoop_temp$mean), sd(zoop_temp$mean))))

zoop <- zoop_temp %>%
  rbind(insert) %>% 
  filter(!YEAR<2001) %>%
  mutate(scale = scale(mean))

# covariates for stage 2 =======================
# hatchery pink =============
pink_cov <- read_csv("output/hatchery_Pink_Covariate_AKandAsia.csv")%>%
  filter(Year>2001) %>%
  mutate(scale = as.numeric(scale(sum)))

# hatchery chum ============
chum_cov <- read_csv("output/hatchery_Chum_Covariate_AKandAsia.csv") %>%
  filter(Year>2001) %>%
  mutate(scale = as.numeric(scale(sum)))

# theta 2 -- M2 ============
m2_cov<-read_csv("data/M2_df.csv") %>% # this was created in "MAPP/scripts_02/process_M2_degreedays.R" and the M2 file was copied to this datafile
  dplyr::mutate(DOY = lubridate::yday(dates),
                Year = lubridate::year(dates)) %>% 
  filter(!DOY>300) %>% 
  group_by(Year) %>%
  dplyr::summarise(degree_days = sum(temperature)) %>%
  filter(Year>2001) %>%
  dplyr::select(Year,degree_days) %>%
  mutate(degree_days = as.numeric(scale(degree_days))) # mean scale 



# Plot salmon
salmon <- ggplot(data = sp, aes(x=Year, y=Estimated_Run, group = id, color =id)) +
  geom_point() + 
  geom_line() +
  ggtitle("Adult Salmon")

juvsalmon <- ggplot(data = juv_prop_ayk, aes(x=Year, y=juv_index, group = id, color =id)) +
  geom_point() + 
  geom_line() +
  ggtitle("Juv Salmon")

salmon <- ggpubr::ggarrange(salmon, juvsalmon)

# Plot covariates ============
zoop <- ggplot(data = zoop, aes(x=YEAR, y=scale)) +
  geom_point() + 
  geom_line() +
  ggtitle("Large zoop")

chum <- ggplot(data = chum_cov, aes(x=Year, y=scale)) +
  geom_point() + 
  geom_line() +
  ggtitle("Chum hatchery")

pink <- ggplot(data = pink_cov, aes(x=Year, y=scale)) +
  geom_point() + 
  geom_line() +
  ggtitle("Pink hatchery")

temp <- ggplot(data = m2_cov, aes(x=Year, y=degree_days)) +
  geom_point() + 
  geom_line() +
  ggtitle("M2")

covariates <- ggpubr::ggarrange(zoop, chum, pink, temp)


