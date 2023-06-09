#   ==================================================================================================
# Project Name: AYK Chum Salmon Model --- Plot Raw data 
# Creator: Genoa Sullaway
# Date: Start March 2023
#
# Purpose: Plot raw data from Kusko RR up to 2021
#
# Load Packages =========================================================================================
library(tidyverse)
library(here)

# Load Data =========================================================================================
escapement <- read_csv("data/Processed_Data/kusko_escapement.csv") %>% 
  gather(1:9, key = "project", value = "escapement")

# This is proportions in each area/week/year - Pyj - 
prop<-read_csv("data/Processed_Data/Proportions_run_present_weekly.csv") %>% # only select some weeks for now because proportion has less weeks than the effort data...  
  # mutate(year = 1976:(1976+nrow(.)-1)) %>%
  # filter(year < 2008) %>%
  dplyr::select(-Year) %>% 
  dplyr::select(c(3:9))

#  observed catch per week 
obs_catch_week <- read_csv("data/Processed_Data/OLD/OLD_catch_week.csv")  

# Observed effort 
effort <- read_csv("data/Processed_Data/effort.csv")  

#obs Commercial subsitence catch
catch<-read_csv("data/Processed_Data/catch.csv") 


# Plot Escapement =========================================================================================

 ggplot(data = escapement,aes(x=year, y =escapement/1000)) +
  geom_point() +
  geom_line() + 
  facet_wrap(~project, scales = "free") +
  theme_classic() +
  ylab("Escapement (/1000)") 

# Plot Effort =========================================================================================
ggplot(data = effort %>% gather(1:7, key = "week", value = "effort"), aes(x=year, y =week, fill = effort)) +
  geom_tile() + 
  theme_classic() +
  ylab("Escapement (/1000)") 


# Plot Catch  =========================================================================================

ggplot(data = catch %>% gather(c(2:5), key = "id", value = "catch"),aes(x=Year, y =catch/1000)) +
  geom_point() +
  geom_line() + 
  facet_wrap(~id, scales = "free") +
  theme_classic() +
  ylab("Catch (/1000)") 
