# Start ==================================================================================================
# Project Name: AYK Chum Salmon Model
# Creator: Genoa Sullaway
# Date: Start March 2023
#
# Purpose: Recreate ADFG Kuskokwim Chum Run reconstruction in R based on excel sheet
# based on Bue and Molyneaux Kuskokwim chum salmon run reconstruction 
#
# Note: this script does the RR until 2007 to match the paper, see: Kusko_RunReconstruction_2021 for the current data! 
# Load Packages =========================================================================================
library(tidyverse)
library(here)

# Load data =========================================================================================
upper_year = 2012 # for filtering datasets 
 
# Escapement - Weir estimates by project 
escapement <- read_csv("data/Processed_Data/OLD/OLD_kusko_escapement.csv") %>%
 filter(year < upper_year & year >1987) 

proj_names<-colnames(escapement)[2:8]

inriver <- read_csv("data/Processed_Data/OLD/inriver.csv") %>%
 filter(year < upper_year & year >1987) %>%
  mutate(Reconstruction=replace_na(Reconstruction, 0))


inriver_na <- read_csv("data/Processed_Data/OLD/inriver.csv") %>%
  filter(year < upper_year & year >1987) 

# This is proportions in each area/week/year - Pyj - right now, just fit for 2008
prop<- read_csv("data/Processed_Data/OLD/OLD_Proportions_run_present_weekly.csv") %>% # only select some weeks for now because proportion has less weeks than the effort data...
  mutate(year = 1976:(1976+nrow(.)-1)) %>%
  filter(year < upper_year & year >1987) %>%
  dplyr::select(-year) %>%
  select(c(3:12)) #getting rid of first two weeks bc that is what bue does ....

# Calculate Pyj based on Bethel CPUE
# Load bethel CPUE and calculate here
 
# prop<-read_csv("data/Processed_Data/Prop_V2.csv") %>%
#   filter(year < 2008 & year >1987) %>%
#   dplyr::select(-year)

#  observed catch per week 
obs_catch_week <- read_csv("data/Processed_Data/OLD/OLD_catch_week.csv") %>%
  filter(year < upper_year & year >1987) %>%
  dplyr::select(-year)  
 
# Observed effort 
effort <- read_csv("data/Processed_Data/OLD/OLD_effort.csv") %>% # from bethel csv
  filter(year < upper_year & year >1987)  
 
#obs Commercial subsitence catch
  catch<-read_csv("data/Processed_Data/OLD/OLD_catch.csv") %>% # from bethel csv
  filter(Year < upper_year & Year >1987)  
  
# format effort for equation 
B_yj = as.matrix(effort)  
 
#years <- c(1976:1976+nrow(obs_catch_week)-1)  #1976:2021 #length of years in dataset
Nyear <- as.numeric(nrow(prop))
T = 282 #Nyear*4 # "Total number of observations from all data sets" page 6 -here: number of years * 4 data sets. is this right? 
weeks = as.numeric(ncol(prop))
projects = ncol(escapement)-1 # number of weir projects - the year column
#err_variance = 0 # error for catch equation... 

# Set up data that are inputs to likelihood fxns =========================================================================================
obs_escape_project <- as.matrix(escapement[,2:8])
obs_escape <- as.matrix(rowSums(escapement[,2:8]))
# obs_catch <- as.matrix(rowSums(catch[,2:3]))
obs_commercial <- as.matrix(catch[,2])
obs_subsistence <- as.matrix(catch[,3])
obs_N = as.matrix(obs_escape + obs_subsistence + obs_commercial + inriver[2] ) # +catch[,4] + catch[,5]) # on Page 5 of model paper this is N_y, in excel this is "# of fish accounted for"
obs_inriver = as.matrix(obs_subsistence + obs_commercial + inriver_na[2])

# NLL Function =========================================================================================
NLL <- function(par,
                data,
                weeks,
                projects,
                Nyear, 
                weights,
                T){ 
    
# Extract parameters and data: ============================================================================

  # grep("ln_q_vec", par_names)
  ln_q_vec <- par[1] 
  
  #  grep("pred_N", par_names)
   ln_pred_N <- par[2:25]
  #ln_pred_N <- par[2:21]
  # ln_pred_N <- par[2:37]
  
 # grep("ln_pred_slope", par_names)  
 # ln_pred_slope <- par[38:44]
  #ln_pred_slope <- par[22:28]
 ln_pred_slope <- par[26:32]

  q_vec <- exp(ln_q_vec)
  pred_N <- exp(ln_pred_N)
  pred_slope <- exp(ln_pred_slope)
  
# Extract Data: ============================================================================
  B_yj=as.matrix(data$B_yj)[,1:10]
  #obs_catch =as.matrix(data$obs_catch)
  obs_catch_week=as.matrix(data$obs_catch_week) 
  obs_N=as.matrix(data$obs_N)
  obs_escape_project = as.matrix(data$obs_escape_project)
  obs_subsistence = as.matrix(data$obs_subsistence)
  obs_commercial = as.matrix(data$obs_commercial)
  
  # Predict N - Observed Total Return =========================================================================================

  # N_yi = number of chum present in commercial district by week/year (Eq 4)
  #summing across weeks for Nyi is supposed to give Ny, total fish present across years 
  
  N_yi <- matrix(nrow = Nyear, ncol = weeks)
 
  for (j in 1:Nyear) {
    for (i in 1:weeks) {
    if(prop[j,i] > 0 & obs_catch_week[j,i]>0){
    N_yi[j,i] =  as.matrix(pred_N[j]*prop[j,i])
      }
    else(N_yi[j,i] <- NA)
    }
      }

  # Predict C - Catch, using Baranov catch equation: ============================================================================
  
  # N is # of fish (week by year)
  # B is effort (week by year)
  # for right now estimate 1 q for whole data set
  #pred_catch = N_yi_vec_prop*(1-(exp(-q_vec*B_yj_vec))) 
   
  pred_catch <- matrix(ncol = weeks, nrow = Nyear)
   
  for (i in 1:weeks) {
    for (j in 1:Nyear) { 
      if(is.na(N_yi[j,i])){
        pred_catch[j,i] <- NA 
      }
      else( pred_catch[j,i] = N_yi[j,i]*(1-(exp(-q_vec*B_yj[j,i]))))
    }
  }
 
  # pred_catch_col <- rowSums(pred_catch)
  # Predict E - Escapement =========================================================================================
#Eq 1 expands the data and yield "observed escapement"
  # this is equation 1 (trying to code it exactly as it is even though it seems super weird...)
  obs_e_proj <-matrix(NA, ncol = projects, nrow = Nyear)  
  # fix week --> project title 
  for (p in 1:projects) {
    for (j in 1:Nyear) {
      obs_e_proj[j,p] = pred_slope[p]*obs_escape_project[j,p]
    }
  }
  obs_escape<- rowSums(obs_e_proj)
  
  #equation 6 in Bue paper 
  #obs_N = as.matrix(obs_escape + obs_subsistence + obs_commercial)# + inriver[2] ) # +catch[,4] + catch[,5]) # on Page 5 of model paper this is N_y, in excel this is "# of fish accounted for"

  # equation 2 yields predicted escapement 
  pred_E = pred_N - obs_subsistence - obs_commercial #+ inriver[2]
   
  # Calculate NLLs ===================================================================

 SSQ_catch <- sum(ifelse(!is.na(pred_catch), (log(obs_catch_week)-log(pred_catch))^2/(weights[1]^2),0))
   
   # NLL_catch  <- dnorm(x=log(obs_catch_week+1e-6), mean=log(pred_catch+1e-6),
   #                               sd = 0.1, log = TRUE)     

#     for (i in 1:weeks) {
#     for (j in 1:Nyear) { 
#       if(pred_catch[j,p]>0){
#   NLL_catch[j,i]  <- dnorm(x=log(obs_catch_week[j,i]+1e-6), mean=log(pred_catch[j,i]+1e-6),
#                       sd = 0.1, log = TRUE)     
#       }
#     }
#   }
  SSQ_escapement <- matrix(NA, ncol = projects, nrow = Nyear)  
 
  for (p in 1:projects) {
    for (j in 1:Nyear) {
          if(obs_e_proj[j,p]>0){
  SSQ_escapement[j,p] <- (log(obs_e_proj[j,p])-log(pred_E[j]))^2/(weights[2]^2) 
          } else{
            SSQ_escapement[j,p] <- 0}
    }
  }

  SSQ_escapement_sum<-sum(SSQ_escapement)
  # excel only does SSQ for certain years... code that below: 2000,2003,2004,2006
  # grep("2000", escapement$year)  
  #  grep("2002", escapement$year)  
  # grep("2003", escapement$year)  
  #   grep("2006", escapement$year)
  
  # OG
  # SSQ_TotalRun <- matrix(ncol = 1, nrow = Nyear)
  # 
  # for (i in 1:Nyear) {
  #   if(i %in% c(13,15,16,19))  {
  #     SSQ_TotalRun[i]  <-  (log(obs_N[i]+1e-6)-log(pred_N[i]+1e-6))^2/(weights[3]^2)
  #   } else{
  #     SSQ_TotalRun[i] <- 0 }
  # }
  # SSQ_TotalRun_sum<-sum(SSQ_TotalRun)
  
  SSQ_TotalRun <- matrix(ncol = 1, nrow = Nyear)
 
  for (i in 1:Nyear) {
    if(i %in% c(13,15,16,19))  {
      SSQ_TotalRun[i]  <-  (log(obs_inriver[i]+1e-6)-log(pred_N[i]+1e-6))^2/(weights[3]^2)
    } else{
      SSQ_TotalRun[i] <- 0 }
  }
  SSQ_TotalRun_sum<- sum( SSQ_TotalRun)
#  SSQ_TotalRun_sum  <- sum(ifelse(!is.na(pred_N), (log(obs_N+1e-6)-log(pred_N+1e-6))^2/(weights[3]^2),0))
  
  # NLL_escapement  <- dnorm(x=log(obs_escape+1e-6), mean=log(pred_E+1e-6),
  #                     sd = 0.1, log = TRUE)  
 
  # NLL_N_TotalRun_sum  <- dnorm(x=log(obs_N+1e-6), mean=log(pred_N+1e-6),
  #                          sd = 0.1, log = TRUE)  
  # 
  #NLL <- (-1 * (weights[1] * sum(NLL_catch, na.rm = TRUE))* (weights[2] * sum(NLL_escapement, na.rm = TRUE))* (weights[3] * sum(NLL_N_TotalRun_sum, na.rm = TRUE))) * (273/2)
  NLL <- log(SSQ_escapement_sum+SSQ_TotalRun_sum+SSQ_catch)*T/2
  # Return the total objective function value
  return(NLL)
  
}

# Parameter starting values ===================================================================
bue_estimated <- read_csv("data/Processed_Data/OLD/Estimated_N_OldModel_XLS.csv") %>% # this is from the older excel sheet, columns Q,R,FW 
  filter(param == "N", year_or_project < upper_year) %>%
  arrange(year_or_project)

bue_estimated_slope <- read_csv("data/Processed_Data/OLD/Estimated_N_OldModel_XLS.csv") %>% # this is from the older excel sheet, columns Q,R,FW 
  filter(param == "Slope" ) %>%
  arrange(year_or_project)

ln_q_vec <- log(0.0000441) 
#ln_pred_N <- rep(log(2000000),Nyear)
ln_pred_N <-log(bue_estimated$value) # rep(log(2000000),Nyear)
escapement_slope <-  log(bue_estimated_slope$value) #rep(log(150), times = projects) 
 
pars_start<- c( 
  ln_q_vec,
  ln_pred_N,
  escapement_slope)

par_names <- c(
         "ln_q_vec",
  paste0("ln_pred_N", c(1:Nyear)),
  paste0("ln_pred_slope", c(1:projects))) 
# #in paper
# w_catch <- 2.0
# w_escapement <- 1.0
# w_inriver <- 0.5
# weights <- c(w_catch,w_escapement,w_inriver)

#in excel
w_catch <- 1.0
w_escapement <- 1.0
w_inriver <- 0.25
weights <- c(w_catch,w_escapement,w_inriver)

# List input data  ===================================================================
data <- list(B_yj=B_yj, 
            # obs_catch_week=obs_catch,
             obs_catch_week=obs_catch_week,
             inriver =inriver,
             obs_N=obs_N,
             obs_escape_project=obs_escape_project,
             obs_commercial = obs_commercial,
             obs_subsistence=obs_subsistence)

#check that NLL fxn works on its own, it does
NLL(par=pars_start,
    data =data,
    weeks=weeks,
    projects=projects,
    Nyear=Nyear,
    weights = weights,
    T=T)

# Optimize ============================================================================================================
fit_nlm <- nlminb(
  start = pars_start,
  objective = NLL,
  data =data,
  weeks=weeks,
  projects=projects,
  Nyear=Nyear,
  weights = weights,
  T=T,
  control = list(iter.max = 1e6, eval.max = 1e6, trace = 1)
)

# Access the estimated parameter values
param_est <- fit_nlm$par
exp(param_est)

fit_nlm$objective
 
saveRDS(exp(param_est),"output/OLD_optim_output_par.RDS")

 

