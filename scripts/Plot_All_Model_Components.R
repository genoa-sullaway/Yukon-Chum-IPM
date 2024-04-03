# covariates will need to be checked as model changes, this was copied from model script so all covariates came over the same form they would go in the model
library(tidyverse)
library(here)
library(readxl) 


library(RNetCDF)
library(tidync)
library(lubridate) 


# Havent saved the data yet so am just sourcing the script that makes them 
source("scripts/01_Make_Salmon_Data_for_Model.R")
# Load data =======================================================

# pull in data already tidyied HERE 

# combine data for FISH plots =======
recruits <- rbind(yukon_summer_recruits%>% mutate(id = "summer"),
                  yukon_fall_recruits%>% mutate(id = "fall"))

spawners <- rbind(yukon_summer_spawners%>% mutate(id = "summer"),
      yukon_fall_spawners%>% mutate(id = "fall"))

harvest <- rbind(yukon_summer_harvest %>% mutate(id = "summer"),
                  yukon_fall_harvest%>% mutate(id = "fall"))
 


# Plot from RR ===============
ggplot(data = recruits,aes(x=cal_year, y =total_run/1000000, group =id, color =id)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  scale_color_manual(name = "Yukon River Chum Recruits (Est Total Run)", values = c("#556B2F","#D2B48C")) + #"#FFC107")) + #PNWColors::pnw_palette("Starfish", n=2)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 12),
        legend.position="top") +
  xlab("Calendar Year") +
  ylab("Estimated Return\nAbundance (Millions)")+
  ggtitle("Total Run")
  
ggplot(data = harvest,aes(x=cal_year, y =harvest/1000000, group =id, color =id)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  scale_color_manual(name = "Yukon River Chum Harvest (Est Total Run)", values = c("#556B2F","#D2B48C")) + #"#FFC107")) + #PNWColors::pnw_palette("Starfish", n=2)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 12),
        legend.position="top") +
  xlab("Calendar Year") +
  ylab("Estimated Harvest\nAbundance (Millions)") +
  ggtitle("Harvest")

ggsave("output/Plot_Harvest_Chum.jpg",height = 4, width = 7)

ggplot(data = spawners,aes(x=cal_year, y =Spawners/1000000, group =id, color =id)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  scale_color_manual(name = "Yukon River Chum Spawners (Est Total Run)", values = c("#556B2F","#D2B48C")) + #"#FFC107")) + #PNWColors::pnw_palette("Starfish", n=2)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 12),
        legend.position="top") +
  xlab("Calendar Year") +
  ylab("Estimated Harvest\nAbundance (Millions)") +
  ggtitle("Spawners")

ggsave("output/Plot_Spawners_Chum.jpg",height = 4, width = 7)


# spawners mean scale =========== 
meanscale <- spawners %>%
  group_by(id) %>% 
  mutate(meanscale = as.numeric(scale(Spawners)))

ggplot(data = meanscale,aes(x=cal_year, y =meanscale, group =id, color =id)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  scale_color_manual(name = "Yukon River Chum Spawners (Est Total Run)", values = c("#556B2F","#D2B48C")) + #"#FFC107")) + #PNWColors::pnw_palette("Starfish", n=2)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 12),
        legend.position="top") +
  xlab("Calendar Year") +
  geom_hline(yintercept = 0, linetype = 2) + 
  ylab("Estimated Harvest\nAbundance (Millions)") +
  ggtitle("Spawners")

ggsave("output/Plot_Mean_Scale_Spawners_Chum.jpg",height = 4, width = 7)


# Runs on the same plot ==============


summer <- rbind(yukon_summer_recruits %>% 
                  mutate(id = "recruits") %>% 
                  rename(abundance = "total_run"),
                yukon_summer_spawners%>% 
                  mutate(id = "spawners") %>% 
                  rename(abundance = "Spawners"),
                yukon_summer_harvest %>% 
                  mutate(id = "harvest") %>% 
                  rename(abundance = "harvest"))
                
  
  fall <- rbind(yukon_fall_recruits %>% 
                  mutate(id = "recruits") %>% 
                  rename(abundance = "total_run"),
                yukon_fall_spawners%>% 
                  mutate(id = "spawners") %>% 
                  rename(abundance = "Spawners"),
                yukon_fall_harvest %>% 
                  mutate(id = "harvest") %>% 
                  rename(abundance = "harvest"))
   
  
  ggplot(data = summer,aes(x=cal_year, y =abundance/1000000, group =id, color =id)) +
    geom_line() +
    geom_point() +
    theme_classic() +
    scale_color_manual(name = "Yukon River Summer Chum",
                       values = PNWColors::pnw_palette("Starfish", n=3)) +  #"#FFC107")) + #PNWColors::pnw_palette("Starfish", n=2)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size = 12),
          legend.position="top") +
    xlab("Calendar Year") +
    ylab("Abundance (Millions)") +
    ggtitle("Summer")
  
  ggsave("output/Plot_Summer_Chum.jpg",height = 4, width = 7)
  
  ggplot(data = fall,aes(x=cal_year, y =abundance/1000000, group =id, color =id)) +
    geom_line() +
    geom_point() +
    theme_classic() +
    scale_color_manual(name = "Yukon River Fall Chum",
                       values = PNWColors::pnw_palette("Starfish", n=3)) +  #"#FFC107")) + #PNWColors::pnw_palette("Starfish", n=2)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size = 12),
          legend.position="top") +
    xlab("Calendar Year") +
    ylab("Abundance (Millions)") +
    ggtitle("Fall")

    ggsave("output/Plot_Fall_Chum.jpg", height = 4, width = 7)

# # Abundances ========
# yukon_summer <- read_excel("data/Yukon_Escapement_ADFG/S Chum RR 2023.xlsx",sheet = 2) %>%
#   select(1:4) %>% 
#   janitor::row_to_names(row_number = 1) %>%
#   dplyr::rename(cal_year = "Year",
#                 total_run =`Total Run`)  %>%
#   dplyr::mutate(Escapement = as.numeric(Escapement),
#                 Harvest = as.numeric(Harvest),
#                 cal_year=as.numeric(cal_year),
#                 total_run = as.numeric(total_run))
# 
# yukon_summer_spawners <- yukon_summer %>%
#   select(cal_year, Escapement)
# 
# yukon_summer_harvest <- yukon_summer %>%
#   select(cal_year, Escapement)
# 
# yukon_summer_recruits <- yukon_summer %>%
#   select(cal_year, total_run)



  dplyr::mutate(age3=as.numeric(age3),
                age4=as.numeric(age4),
                age5=as.numeric(age5),
                age6=as.numeric(age6)) %>%
  filter(!cal_year < 2005)
  #dplyr::select(Year, `Total Run`) %>%
  dplyr::rename(Total_Run = `Total Run`) %>%
  filter(Year > 2001) %>%
  dplyr::mutate(id = 1)#,
#Estimated_Run = as.numeric(scale(Estimated_Run)))

yukon_fall<- read_csv("data/Yukon_Escapement_ADFG/Yukon_Fall_Chum_RR_JTC.csv")  %>%
  select(Year, Estimated_Run) %>%
  filter(Year > 2001) %>%
  dplyr::mutate(id = 2)#,
#Estimated_Run = as.numeric(scale(Estimated_Run)))


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


