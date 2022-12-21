## Useful ML model links: 
## https://www.r-bloggers.com/2019/08/maximum-likelihood-estimation-from-scratch/
## https://towardsdatascience.com/maximum-likelihood-estimation-in-r-b21f68f1eba4
library(here)
library(tidyverse)
 
escapement <- read.csv("data/Kusko_Reconstruction/Kusko_DataV1_Escapement.csv") %>% 
  dplyr::mutate(year = 1976:2011) %>% 
  select(-Reconstruction,-X) %>% 
  gather(1:7, key = "project", value = "escapement") %>% 
 mutate(escapement = as.numeric(gsub(",","",escapement))) %>% 
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
  
# Estimate C_hat --- Catch data. 

catch_y <- read.csv("data/Kusko_Reconstruction/Kusko_Data_V1_Catch.csv")  %>%
  dplyr::mutate(Commercial=as.numeric(gsub(",","",Commercial)),
         Subsistence = as.numeric(gsub(",","",Subsistence)))

 
 #obs_effort, B_yj eq 5 -- this needs to be tidy-ed
catch_effort <-read.csv("data/Kusko_Reconstruction/Bethel_Effort.csv", header = FALSE)  

col_odd <- seq_len(ncol(catch_effort)) %% 2

commercial_catch_df <- catch_effort[ , col_odd == 1]   

commercial_catch_yi <- commercial_catch_df %>%
  slice(-2) %>%
  janitor::row_to_names(row_number = 1) %>%
  dplyr::mutate(year = 1976:2011)  %>%
  gather(1:13, key = "date", value = "catch") %>% 
  dplyr::mutate(catch=as.numeric(gsub(",","",catch))) %>% 
  spread(date, catch)

commercial_effort_df<- catch_effort[ , col_odd == 0]    

commercial_effort_yi <- commercial_effort_df %>%
  slice(-2) %>%
  janitor::row_to_names(row_number = 1) %>%
  dplyr::mutate(year = 1976:2011)  %>%
  gather(1:13, key = "date", value = "effort") %>% 
  dplyr::mutate(effort=as.numeric(gsub(",","",effort))) %>% 
  spread(date, effort) 

P_yj <-  read.csv("data/Kusko_Reconstruction/P_yj_Data_V1.csv") %>%
  janitor::row_to_names(row_number = 1) %>%
  slice(1:36) %>% 
  dplyr::mutate(year = 1976:2011) %>% 
  gather(1:15, key = "date", value = "proportion") %>%
  mutate(proportion = as.numeric(gsub(",","",proportion))) %>%
  replace(is.na(.), 0)  %>%
  spread(date, proportion)  %>%
  dplyr::select(-`5/27 - 6/2`,
                -`6/3 - 6/9`
                ) # remove this column has all 0's and not in Pyj-- need to revisit this! 
 
#total returns to be compared with N_hat...
N_y = rowSums(escapement) + rowSums(catch) # Eq. 6

#Eq. 4 -- not sure if this is actually Nyhat because nothing was estimated?? unlcear in paper. 
#total returns per week/year is a fraction of the fish that are within the bethel area (Pyj)
N_yj_hat = as.matrix(N_y*P_yj[,2:14]) 

# want error for each week and year -- Log normal Error 
error_yj = matrix( ncol = ncol(P_yj)-1, nrow = nrow(P_yj))
for (i in 1:ncol(P_yj)-1 ) {
error_yj[,i] = rlnorm(n= 36)#, meanlog = 0, sdlog= sd(commercial_catch_yi[,i]))
}

#Catchability q = catch/effort (assumping catch = mortality??)
mat_commercial_catch_yi<- as.matrix(commercial_catch_yi)
mat_commercial_effort_yi<-as.matrix(commercial_effort_yi)
q=mat_commercial_catch_yi[,2:14]/mat_commercial_effort_yi[,2:14]
q[is.nan(q)] <- 0

#B_yj is bethel efort week/year
B_yj<-mat_commercial_effort_yi[,2:14]
  
###### Baranov Catch for each week/year, yj
C_yj_hat = matrix( ncol = ncol(P_yj)-1, nrow = nrow(P_yj))

for (i in 1:ncol(P_yj)-1 ) {
   C_yj_hat[,i] = N_yj_hat[,i]*(1-exp(-q[,i]*B_yj[,i]))*exp(error_yj[,i])
}
   