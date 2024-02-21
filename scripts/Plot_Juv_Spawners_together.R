# Plot spawners and juveniles together on absolute and relative scales 
library(tidyverse)
library(here)


# covariates will need to be checked as model changes, this was copied from model script so all covariates came over the same form they would go in the model
library(rstan)
library(tidyverse)
library(here)
library(bayesplot)
library(ggpubr)
library(readxl)
library(lubridate) 

# Load data =======================================================
# Spawners =======
yukon_summer <- read_excel("data/Yukon_Escapement_ADFG/Yukon Summer Chum Total Run 1978-2022 Run Rec.xlsx") %>%
  dplyr::select(Year, `Total Run`) %>%
  dplyr::rename(Estimated_Run = `Total Run`) %>%
  filter(Year > 2001) %>%
  dplyr::mutate(id = 1,
                reporting_group =  "Yukon_Summer") 

yukon_fall<- read_csv("data/Yukon_Escapement_ADFG/Yukon_Fall_Chum_RR_JTC.csv")  %>%
  select(Year, Estimated_Run) %>%
  filter(Year > 2001) %>%
  dplyr::mutate(id = 2,
                reporting_group =  "Yukon_Fall") 

kusko_estimated_parameters<- readRDS("output/optim_output_par_data2021.RDS")
kusko<-data.frame(Year = c(1988:(2022-1)),
                  Estimated_Run= as.vector(c(kusko_estimated_parameters[2:35])),
                  id = 3, 
                  reporting_group =  "Kusko_Bristol") %>% 
  filter(Year > 2001) 

mean<-mean(kusko$Estimated_Run)
kusko<-kusko %>%
  rbind(df=data.frame(Year = c(2022), Estimated_Run = c(mean), id=c(3), 
                      reporting_group ="Kusko_Bristol" ))

sp <- rbind(yukon_summer, yukon_fall, kusko) %>%
  group_by(reporting_group) %>%
  dplyr::mutate(Spawners_scaled = scale(Estimated_Run))

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
  ungroup() #%>%
 # dplyr::select(-reporting_group)  
 
# Plot spawners ========================================
#salmon <- 
  ggplot(data = sp, aes(x=Year, y=Estimated_Run, group = reporting_group,
                        color =reporting_group)) +
  geom_point() + 
  geom_line() +
  ggtitle("Adult Salmon")

# Plot juveniles ========================================
#juvsalmon <- 
  ggplot(data = juv_prop_ayk, aes(x=Year, y=juv_index, group = reporting_group, color =reporting_group)) +
  geom_point() + 
  geom_line() +
  ggtitle("Absolute Juv Salmon")
  
juv_scale <- juv_prop_ayk %>% 
    group_by(reporting_group) %>%
    dplyr::mutate(Juveniles_scaled = scale(juv_index))

ggplot(data = juv_scale, aes(x=Year, y=scaled, group = reporting_group, color =reporting_group)) +
  geom_point() + 
  geom_line() +
  ggtitle("Scaled Juv Salmon")+
  theme_classic() +
  geom_hline(yintercept =0, linetype =2)

# Plot together - Survey year ========================================
join_all_fish<- left_join(sp, juv_scale) %>%
  dplyr::select(Year, reporting_group, Spawners_scaled, Juveniles_scaled) %>%
  gather(3:4, key = "id", value ="value")

ggplot(data = join_all_fish, aes(x=Year, y=value, group = id, color =id)) +
  geom_point() + 
  geom_line() +
  ggtitle("Scaled Survey Year")+
  theme_classic() +
  geom_hline(yintercept =0, linetype =2) +
  facet_wrap(~reporting_group)

# Plot together - brood year ========================================
brood_year <-  sp  %>% 
                  dplyr::mutate(brood_year = Year - 4) %>%
                  left_join(juv_scale %>% rename(brood_year = "Year")) %>%
                  dplyr::select(brood_year, reporting_group, Spawners_scaled, Juveniles_scaled) %>%
                  gather(3:4, key = "id", value ="value")

ggplot(data = brood_year, aes(x=brood_year, y=value, group = id, color =id)) +
  geom_point() + 
  geom_line() +
  ggtitle("Scaled Brood Year")+
  theme_classic() +
  geom_hline(yintercept =0, linetype =2) +
  facet_wrap(~reporting_group) +
  xlab("Brood Year")
 