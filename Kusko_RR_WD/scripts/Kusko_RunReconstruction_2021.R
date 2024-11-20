# Start ==================================================================================================
# Project Name: AYK Chum Salmon Model
# Creator: Genoa Sullaway
# Date: Start March 2023
#
# Purpose: Recreate ADFG Kuskokwim Chum Run reconstruction in R based on excel sheet
# based on Bue and Molyneaux Kuskokwim chum salmon run reconstruction which has data unitl 2011, here I use the same
# structure but add in 2021 data provided by ADFG. 
# Load Packages =========================================================================================
library(tidyverse)
library(here)

# Load data =========================================================================================
# Escapement - Weir estimates by project 
escapement <- read_csv("data/Processed_Data/kusko_escapement.csv")

# This is proportions in each area/week/year - Pyj - 
prop<-read_csv("data/Processed_Data/Proportions_run_present_weekly.csv") %>% # only select some weeks for now because proportion has less weeks than the effort data...  
 # mutate(year = 1976:(1976+nrow(.)-1)) %>%
 # filter(year < 2008) %>%
  dplyr::select(-Year) %>% 
  dplyr::select(c(3:9))

#  observed catch per week 
obs_catch_week <- read_csv("data/Processed_Data/catch_week.csv")  

# Observed effort 
effort <- read_csv("data/Processed_Data/effort.csv")  

#obs Commercial subsitence catch
catch<-read_csv("data/Processed_Data/catch.csv") 

# format effort for equation 
B_yj = as.matrix(effort[,1:7])  

#years <- c(1976:1976+nrow(obs_catch_week)-1)  #1976:2021 #length of years in dataset
Nyear <- as.numeric(nrow(prop))
T =  Nyear*4 # "Total number of observations from all data sets" page 6 -here: number of years * 4 data sets. is this right? 
weeks = as.numeric(ncol(prop))
projects = ncol(escapement)-1 # number of weir projects - the year column
err_variance = 0 # error for catch equation... 

# Set up data that are inputs to likelihood fxns =========================================================================================
obs_escape_project <- as.matrix(escapement[,1:9])
obs_escape <- as.matrix(rowSums(escapement[,1:9]))
#obs_catch <- as.matrix(rowSums(catch[,2:3]))
obs_commercial <- as.matrix(catch[,2])
obs_subsistence <- as.matrix(catch[,3])
#equation 6 in Bue paper 
obs_N = as.matrix(obs_escape + obs_subsistence + obs_commercial +catch[,4] + catch[,5]) # on Page 5 of model paper this is N_y, in excel this is "# of fish accounted for"
colnames(obs_N)<- NULL

# NLL Function =========================================================================================
NLL <- function(par,
                data,
                weeks,
                projects,
                Nyear, 
                weights){ 
    
# Extract parameters and data: ============================================================================

  # grep("ln_q_vec", par_names)
  ln_q_vec <- par[1] 
  
  # grep("pred_N", par_names)
  ln_pred_N <- par[2:47]
  
  # grep("ln_pred_slope", par_names)  
  ln_pred_slope <- par[48:56]

  q_vec <- exp(ln_q_vec)
  pred_N <- exp(ln_pred_N)
  pred_slope <- exp(ln_pred_slope)
  
# Extract Data: ============================================================================
   
  B_yj=as.matrix(data$B_yj)
  obs_catch_week=as.matrix(data$obs_catch_week)
  obs_N=as.matrix(data$obs_N)
  obs_escape_project = as.matrix(data$obs_escape_project)
  obs_subsistence = as.matrix(data$obs_subsistence)
  obs_commercial = as.matrix(data$obs_commercial)
  
  # Predict N - Observed Total Return =========================================================================================
  #pred_N = pred_E + rowSums(pred_catch) 
  
  # N_yi = number of chum present in commercial district by week/year (Eq 4)
  #summing across weeks for Nyi is supposed to give Ny, total fish present across years 
  N_yi <- matrix(nrow = Nyear, ncol = weeks)
  for (j in 1:Nyear) {
    N_yi[j,] =  as.matrix(pred_N[j]*prop[j,])
  }

  # Predict C - Catch, using Baranov catch equation: ============================================================================
  
  # N is # of fish (week by year)
  # B is effort (week by year)
  # for right now estimate 1 q for whole data set
  #pred_catch = N_yi_vec_prop*(1-(exp(-q_vec*B_yj_vec))) 
   
  pred_catch <- matrix(ncol = weeks, nrow = Nyear)
  
  for (i in 1:weeks) {
    for (j in 1:Nyear) { 
      pred_catch[j,i] = N_yi[j,i]*(1-(exp(-q_vec*B_yj[j,i])))
    }
  }
  
  
  # Predict E - Escapement =========================================================================================
#Eq 1 expands the data and yield "observed escapement"
  # this is equation 1 (trying to code it exactly as it is even though it seems super weird...)
  obs_e_week <-matrix(NA, ncol = projects, nrow = Nyear)  
  
  for (p in 1:projects) {
    for (j in 1:Nyear) {
      obs_e_week[j,p] = pred_slope[p]*obs_escape_project[j,p]
    }
  }
  obs_escape<- rowSums(obs_e_week)
  
  # equation 2 yields predicted escapement 
  pred_E = pred_N - obs_subsistence - obs_commercial
   
  # Calculate NLLs ===================================================================
  
  NLL_catch  <- dnorm(x=log(obs_catch_week+1e-6), mean=log(pred_catch+1e-6),
                      sd = 0.1, log = TRUE)     
  
  NLL_escapement  <- dnorm(x=log(obs_escape+1e-6), mean=log(pred_E+1e-6),
                      sd = 0.1, log = TRUE)  
  
  NLL_N_TotalRun_sum  <- dnorm(x=log(obs_N+1e-6), mean=log(pred_N+1e-6),
                           sd = 0.1, log = TRUE)  
  
  NLL <- -1 * (weights[1] * sum(NLL_catch, na.rm = TRUE))* (weights[2] * sum(NLL_escapement, na.rm = TRUE))* (weights[3] * sum(NLL_N_TotalRun_sum, na.rm = TRUE))
  
  # Return the total objective function value
  return(NLL)
  
}

# Parameter starting values ===================================================================
ln_q_vec <- log(0.0000441) 
ln_pred_N <- rep(log(2000000),Nyear)
escapement_slope <- rep(log(104), times = projects) 

pars_start<- c( 
  ln_q_vec,
  ln_pred_N,
  escapement_slope)

par_names <- c(
         "ln_q_vec",
  paste0("ln_pred_N", c(1:Nyear)),
  paste0("ln_pred_slope", c(1:projects))) 

w_catch <- 2.0
w_escapement <- 1.0
w_inriver <- 0.5
weights <- c(w_catch,w_escapement,w_inriver) 

# List input data  ===================================================================
data <- list(B_yj=B_yj, 
             obs_catch_week=obs_catch_week,
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
    weights = weights)

# Optimize ============================================================================================================
fit_nlm <- nlminb(
  start = pars_start,
  objective = NLL,
  data =data,
  weeks=weeks,
  projects=projects,
  Nyear=Nyear,
  weights = weights,
  control = list(iter.max = 1e6, eval.max = 1e6, trace = 1)
)

# Access the estimated parameter values
param_est <- fit_nlm$par
exp(param_est)
 
 saveRDS(exp(param_est),"output/current_optim_output_par.RDS")




