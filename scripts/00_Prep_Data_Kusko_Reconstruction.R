# Start ==================================================================================================
#Project Name: AYK Chum Salmon Model
#Creator: Genoa Sullaway
#Date: Start March 2023
#
# Purpose: Process data for Kusko Run reconstruction 
#
# Load Packages =========================================================================================
library(tidyverse)
library(here)

# Load data =========================================================================================

## escapement 

escapement <- read.csv("data/Kusko_Reconstruction/Kusko_Data_Escapement.csv") %>% 
  dplyr::mutate(year = 1976:2021) %>% 
 # dplyr::select(-Reconstruction,-X) %>% 
 # gather(1:9, key = "project", value = "escapement") %>% 
 # dplyr::mutate(escapement = as.numeric(gsub(",","",escapement))) %>% 
  # filter(!project == "Reconstruction") %>% # not sure what this category is .. filtering for now 
  # dplyr::mutate(type = case_when(project %in% c("Kwethluk",
  #                                               "Tuluksak",
  #                                               "George",
  #                                               "Kogrukluk",
  #                                               "Tatlawiksuk",
  #                                               "Takotna") ~ "Weir",
  #                                               TRUE ~ "Sonar")) %>% 
  # mutate_all(na_if,"")  %>%
  replace(is.na(.), 0) #%>%
 # spread(project, escapement)

write_csv(escapement, "data/Processed_Data/kusko_escapement.csv")

#############
# these are the proportions of run present at each week 
P_yj <-  read.csv("data/Kusko_Reconstruction/P_yj_Data.csv") %>%
  janitor::row_to_names(row_number = 1) %>%
  #slice(1:36) %>% 
  #dplyr::mutate(year = 1976:2011) %>% 
  # gather(1:15, key = "date", value = "proportion") %>%
  # dplyr::mutate(proportion = as.numeric(gsub(",","",proportion))) %>%
  replace(is.na(.), 0)  %>%
  #dates are out of order, need to arrange them in order
  #spread(date, proportion)  %>%
  dplyr::select(`5/27 - 6/2`, # effort data doesn't have these first two columns and they are 0s
    `6/3 - 6/9`,
    `6/10 - 6/16`,
    `6/17 - 6/23`,
    `6/24 - 6/30`,
    `7/1 - 7/7`,
    `7/8 - 7/14`,
    `7/15 - 7/21`,
    `7/22 - 7/28`,
    `7/29-8/4`, #,
    #`8/5-8/11`,
    # `8/12-8/18`,
    # `8/19-8/25`,
    # `8/26-9/1`,
    #`9/2-9/8`
    ) # remove this column has all 0's and not in Pyj-- need to revisit this! 
 
write_csv(P_yj, "data/Processed_Data/Proportions_run_present_weekly.csv")

################ Catch 
catch <- read.csv("data/Kusko_Reconstruction/Kusko_Data_Catch.csv")  %>%
  replace(is.na(.), 0)  

write_csv(catch, "data/Processed_Data/catch.csv")

################ Effort 
catch_effort <-read.csv("data/Kusko_Reconstruction/Kusko_Data_Catch_Effort.csv", header = FALSE)  

col_odd <- seq_len(ncol(catch_effort)) %% 2

commercial_effort_df<- data.frame(catch_effort[ , col_odd == 0])

commercial_effort_yi <- commercial_effort_df %>%
  dplyr::slice(-2) %>%
  dplyr::slice(-48:-52) %>% 
  janitor::row_to_names(row_number = 1) %>%
  dplyr::mutate(year = 1976:2021)  #%>%
  #gather(1:13, key = "date", value = "effort")
 
write_csv(commercial_effort_yi, "data/Processed_Data/effort.csv")

