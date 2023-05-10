## Useful ML model links: 
## https://www.r-bloggers.com/2019/08/maximum-likelihood-estimation-from-scratch/
## https://towardsdatascience.com/maximum-likelihood-estimation-in-r-b21f68f1eba4
library(here)
library(tidyverse)
 
########## consider this approach for the Optim: copied from thorson spatio temporal modeling lecture 1

######### Method 2 -- Optimize using R
# Step 1 -- define function
NegLogLike_Fn = function(Par, Data){
  # Parameters
  Mean_hat = Par[1]
  SD_hat = Par[2]
  
  # Log-likelihood
  LogLike_i = dnorm( Data$y, mean=Mean_hat, sd=SD_hat, log=TRUE )
  NegLogLike = -1 * sum(LogLike_i)
  return( NegLogLike )
}
# step 2 -- minimize negative log-likelihood to estimate parameters
Data = list( 'y'=CPUE )
Start = c(1,1)
NegLogLike_Fn(Par=Start, Data=Data)
Opt = optim( par=Start, fn=NegLogLike_Fn, Data=Data, lower=c(0.01,0.01), upper=Inf, method="L-BFGS-B", hessian=TRUE )
print( Opt$par ) # Estimated parameters
print(sqrt( diag( solve(Opt$hessian) )) ) # standard errors




#I just copied and pasted this information from the excel run reconstruction 
escapement <- read.csv("data/Kusko_Reconstruction/Kusko_DataV1_Escapement.csv") %>% 
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
  
# Estimate C_hat --- Catch data. -- also from old excel sheet 
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
  gather(1:13, key = "date", value = "catch") #%>% 
  # dplyr::mutate(catch=as.numeric(gsub(",","",catch))) %>% 
  # spread(date, catch)

commercial_effort_df<- catch_effort[ , col_odd == 0]    

commercial_effort_yi <- commercial_effort_df %>%
  slice(-2) %>%
  janitor::row_to_names(row_number = 1) %>%
  dplyr::mutate(year = 1976:2011)  %>%
  gather(1:13, key = "date", value = "effort")
# %>% 
#   dplyr::mutate(effort=as.numeric(gsub(",","",effort))) %>% 
#   spread(date, effort) %>%
#   dplyr::select(year, #`5/27 - 6/2`,
#                 #`6/3 - 6/9`,
#                 `6/10 - 6/16`,
#                 `6/17 - 6/23`,
#                 `6/24 - 6/30`,
#                 `7/1 - 7/7`,
#                 `7/8 - 7/14`,
#                 `7/15 - 7/21`,
#                 `7/22 - 7/28`,
#                 `7/29-8/4`,
#                 `8/5-8/11`,
#                 `8/12-8/18`,
#                 `8/19-8/25`,
#                 `8/26-9/1`,
#                 `9/2-9/8`) # remove this column has all 0's and not in Pyj-- need to revisit this! 

# these are the proportions of run present at each week 
P_yj <-  read.csv("data/Kusko_Reconstruction/P_yj_Data_V1.csv") %>%
  janitor::row_to_names(row_number = 1) %>%
  slice(1:36) %>% 
  dplyr::mutate(year = 1976:2011) %>% 
  gather(1:15, key = "date", value = "proportion") %>%
  dplyr::mutate(proportion = as.numeric(gsub(",","",proportion))) %>%
  replace(is.na(.), 0)  %>%
  #dates are out of order, need to arrange them in order
  spread(date, proportion)  %>%
  dplyr::select(#`5/27 - 6/2`, # effort data doesnt have these first two columns and they are 0s
                #`6/3 - 6/9`,
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
 
#total returns to be compared with N_hat...
N_y = rowSums(escapement) + rowSums(catch_y) # Eq. 6

#Eq. 4 -- not sure if this is actually Nyhat because nothing was estimated?? unlcear in paper. 
#total returns per week/year is a fraction of the fish that are within the bethel area (Pyj)
N_yj_hat = as.matrix(N_y*P_yj)#[,2:14]) 

# want error for each week and year -- Log normal Error 
error_yj = matrix( ncol = ncol(P_yj), nrow = nrow(P_yj))
for (i in 1:ncol(P_yj) ) {
error_yj[,i] = rlnorm(n= 36, meanlog = 0, sdlog= 1) #sd(commercial_catch_yi[,i]))
}

########################################################################################################
########################################################################################################

# Calculate catchability, q. Using slides from Est Fish Abundance from Curry, Leslie Depletion Estimator. 
# Lecture 6 - Change in Ratio Estimator

# Process: 
# Calculate CPUE, and Kt (Cumulative CPUE)
# Fit regression model
# Extract intercept
# Divide intercept by the slope (%) 
cpue_df = left_join(commercial_catch_yi, commercial_effort_yi) %>% 
  dplyr::mutate(catch=as.numeric(catch),
                effort = as.numeric(effort), 
                cpue = catch/effort,
                cpue = case_when(is.nan(cpue)~ 0, 
                                 TRUE ~ cpue)) %>% 
  group_by(year) %>% 
  dplyr::mutate(
         k=cumsum(catch), 
         k_lag = (k-catch))

years<-unique(cpue_df$year)

q_df <- data.frame(year = years, 
                   q= rep(NA,times = length(years)), 
                   N0 = rep(NA,times = length(years)))
 
for (i in 1:length(years)) {
reg_df <- cpue_df %>% 
              filter(year == years[i])
  
lm <- lm(cpue ~ k_lag, data=reg_df)
 
# We will recall from lecture that our slope is our estimate of catchability (q)
q.pz <- abs(coef(lm)[2])
 
# And our intercept is equal to q*N0, where N0 is our initial population size
int.pz <- coef(lm)[1]
 
# We can calculate our estimate of initial abundance at the beginning of the
#   depletion experiment by dividing our intercept (q*N0) by q
N0.pz <- int.pz/q.pz
 
q_df[i,2]<-q.pz
q_df[i,3]<-N0.pz
 }

q_df[is.na(q_df)] <- 0

#B_yj is bethel efort week/year
B_yj<-mat_commercial_effort_yi
  
###### Baranov Catch for each week/year, yj
C_yj_hat = matrix( ncol = ncol(P_yj), nrow = nrow(P_yj))

q_vec<- q_df$q

for (i in 1:ncol(P_yj) ) {
   C_yj_hat[,i] = N_yj_hat[,i]*(1-exp(-q_vec*B_yj[,i]))*exp(error_yj[,i])
}

C_yj_hat[is.nan(C_yj_hat)] <- 0
   
colnames(C_yj_hat) <- names(P_yj)
   
# How it is calculated in excel: 
# (proportion (P_ij)*modeled N)*(1-exp(-q*just effort)))
# fixed Q but need to figure out what modeled N is....


 