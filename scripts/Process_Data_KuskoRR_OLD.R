## escapement 

escapement <- read.csv("data/Kusko_Reconstruction/OLD/Kusko_DataV1_Escapement.csv") %>% 
  dplyr::mutate(year = 1976:2011) %>% 
  dplyr::select(-Reconstruction,-X) %>% 
  gather(1:7, key = "project", value = "escapement") %>% 
  dplyr::mutate(escapement = as.numeric(gsub(",","",escapement))) %>% 
  # filter(!project == "Reconstruction") %>% # not sure what this category is .. filtering for now 
  # dplyr::mutate(type = case_when(project %in% c("Kwethluk",
  #                                               "Tuluksak",
  #                                               "George",
  #                                               "Kogrukluk",
  #                                               "Tatlawiksuk",
  #                                               "Takotna") ~ "Weir",
  #                                               TRUE ~ "Sonar")) %>% 
  # mutate_all(na_if,"")  %>%
  replace(is.na(.), 0) %>%
  spread(project, escapement)

write_csv(escapement, "data/Processed_Data/OLD/OLD_kusko_escapement.csv")

#############
# these are the proportions of run present at each week 
P_yj <-  read.csv("data/Kusko_Reconstruction/OLD/P_yj_Data_V1.csv") %>%
  janitor::row_to_names(row_number = 1) %>%
  slice(1:36) %>% 
  dplyr::mutate(year = 1976:2011) %>% 
  gather(1:15, key = "date", value = "proportion") %>%
  dplyr::mutate(proportion = as.numeric(gsub(",","",proportion))) %>%
 # replace(is.na(.), 0)  %>%
  #dates are out of order, need to arrange them in order
  spread(date, proportion)  %>%
  dplyr::select(`5/27 - 6/2`, # effort data doesnt have these first two columns and they are 0s
     `6/3 - 6/9`,
    `6/10 - 6/16`,
    `6/17 - 6/23`,
    `6/24 - 6/30`,
    `7/1 - 7/7`,
    `7/8 - 7/14`,
    `7/15 - 7/21`,
    `7/22 - 7/28`,
    `7/29-8/4`,
    `8/5-8/11`,
    `8/12-8/18`,
    `8/19-8/25`,
    `8/26-9/1`,
    `9/2-9/8`) # remove this column has all 0's and not in Pyj-- need to revisit this! 

write_csv(P_yj, "data/Processed_Data/OLD/OLD_Proportions_run_present_weekly.csv")

################ Catch 
catch <- read.csv("data/Kusko_Reconstruction/OLD/Kusko_Data_V1_Catch.csv")  %>%
  dplyr::mutate(Commercial=as.numeric(gsub(",","",Commercial)),
                Subsistence = as.numeric(gsub(",","",Subsistence)))

write_csv(catch, "data/Processed_Data/OLD/OLD_catch.csv")

################ Effort 
catch_effort <-read.csv("data/Kusko_Reconstruction/Bethel_Effort.csv", header = FALSE)  

col_odd <- seq_len(ncol(catch_effort)) %% 2

commercial_effort_df<- data.frame(catch_effort[ , col_odd == 0])

commercial_effort_yi <- commercial_effort_df %>%
  dplyr::slice(-2) %>%
  janitor::row_to_names(row_number = 1) %>%
  dplyr::mutate(year = 1976:2011) 

write_csv(commercial_effort_yi, "data/Processed_Data/OLD/OLD_effort.csv")

################################################################################ 
################################################################################ 

catch_week<- data.frame(catch_effort[ , col_odd == 1])

commercial_catch_yi <- catch_week %>%
  dplyr::slice(-2) %>%
  janitor::row_to_names(row_number = 1) %>%
  dplyr::mutate(year = 1976:2011)  #%>%
#gather(1:13, key = "date", value = "effort")

write_csv(commercial_catch_yi, "data/Processed_Data/OLD/OLD_catch_week.csv")
