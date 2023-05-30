# Start ==================================================================================================
# Project Name: AYK Chum Salmon Model
# Creator: Genoa Sullaway
# Date: Start March 2023
#
# Purpose: Recreate ADFG Kuskokwim Chum Run reconstruction in R based on excel sheet
# based on Bue and Molyneaux Kuskokwim chum salmon run reconstruction 
#
# Load Packages =========================================================================================
library(bbmle)
library(tidyverse)
library(here)

# Load data =========================================================================================
# Escapement - Weir estimates by project 
escapement <- read_csv("data/Processed_Data/OLD/OLD_kusko_escapement.csv") %>%
  filter(year < 2008) 
catch<-read_csv("data/Processed_Data/OLD/OLD_catch.csv") %>%
  filter(Year < 2008) 
# Effort 
effort <- read_csv("data/Processed_Data/OLD/OLD_effort.csv") %>%
  filter(year < 2008) 
# proportions in each area/week/year - Pyj - right now, just fit for 2011
prop<- read_csv("data/Processed_Data/OLD/OLD_Proportions_run_present_weekly.csv") %>% # only select some weeks for now because proportion has less weeks than the effort data...  
  mutate(year = 1976:(1976+nrow(.)-1)) %>%
  filter(year < 2008) %>%
  dplyr::select(-year) %>% 
  dplyr::select(c(2:14)) #getting rid of first and last week because of 0's 

# using observed catch/week in catch equation
obs_catch_week <- read_csv("data/Processed_Data/OLD/OLD_catch_week.csv") %>% 
  dplyr::select(-year) 

years <-unique(escapement$year)  #1976:2021 #length of years in dataset
Nyear <- length(years)
projects = ncol(escapement)-1 # number of weir projects - the year column
T =  Nyear*4 # "Total number of observations from all data sets" page 6 -here: number of years * 4 data sets. is this right? 
weeks = ncol(prop)
err_variance = 0 # error for catch equation... 

# Set up data that are inputs to likelihood fxns =========================================================================================
obs_escape_project <- as.matrix(escapement[,2:8])
obs_escape <- as.matrix(rowSums(escapement[,2:8]))
#obs_catch <- as.matrix(rowSums(catch[,2:3]))
obs_commercial <- as.matrix(catch[,2])
obs_subsistence <- as.matrix(catch[,3])
#equation 6 in Bue paper 
obs_N = as.matrix(obs_escape + obs_subsistence + obs_catch) # +catch[,4] + catch[,5]) # on Page 5 of model paper this is N_y, in excel this is "# of fish accounted for"
colnames(obs_N)<- NULL

# Baranov Data Input =========================================================================================

B_yj = as.matrix(effort[,1:13]) # observed effort per week/year

N_yi <- matrix(nrow = Nyear, ncol = weeks)
for (j in 1:Nyear) {
  N_yi[j,] =  as.matrix(pred_N[j]*prop[j,])
}

# NLL Function =========================================================================================
NLL <- function(pars,
                #data
                #N_yi,
                B_yj,
                N_yi,
                obs_N,
                obs_escape_project,
                #values
                weeks,
                err_variance,
                projects,
                Nyear,
                weights){ 
  
  # Step 1: Extract parameters, based on their location
  grep("ln_q_vec", par_names)
  #ln_q_vec <- pars_start[1]) # 
  ln_q_vec <- pars_start[1] 
   
  # Step 2: Predict C 
    # Predict C - Catch, using Baranov catch equation: ============================================================================
 
  pred_catch <- matrix(ncol = weeks, nrow = Nyear)
  
  error <- matrix(0, ncol = weeks, nrow = Nyear)
  for (i in 1:weeks) {
    for (j in 1:Nyear) {
      error[j,i] <- rnorm(0,err_variance, n=1)
      pred_catch[j,i] = N_yi[j,i]*(1-(exp(-ln_q_vec*B_yj[j,i])))*exp(error[j,i])
    }
  }
   
  NLL_catch<-matrix(nrow = Nyear, ncol = projects)
   
  for (j in 1:Nyear) {
    for (p in 1:projects) {
      if(pred_catch[j,p] > 0 & obs_catch_week[j,p]  > 0) { # Ensure we don't try to take the log of 0!
        NLL_catch[j,p] <- dnorm(log(as.numeric(obs_catch_week[j,p])), log(pred_catch[j,p]), weights[1], log = TRUE)        
     }
    }
  }
  
  NLL_catch[is.na(NLL_catch)] = 0
  # NLL_catch <- replace_na(NLL_catch,0)
  NLL_catch_sum <- colSums(NLL_catch)
  NLL_catch_sum <- sum(NLL_catch_sum)
  
  objFxn <-  NLL_catch_sum   
  # Return the total objective function value
  return(objFxn)
}

# Parameters and parameter starting values ===================================================================

# Baranov parameters:
ln_q_vec <- 0.1 #0.001 - 0.5 is standard 

pred_N <-obs_N #matrix(nrow = Nyear, ncol =1, 600000) #600000 
# N_sigma <- 0.1

pars_start<- c(# Baranov 
  ln_q_vec )#,
  #baranov_sigma,
  # escapement parameters 
  #escapement_slope,
#   pred_N
#   # escapement_sigma,
#   # Total return parameters
#   #N_sigma # variation for lambda
# ) 

par_names <- c(# baranov parameters
  "ln_q_vec")#,
  #"baranov_sigma",
  #escapement parameters 
 # paste0("escapement_slope", c(1:projects)),
#   paste0("pred_N", c(1:Nyear))
#   #"escapement_sigma", 
#   #total return parameters
#   #"N_sigma"
# )  
# assign weights 
#The parameter weighting scheme used for the demonstration was 0.5 for the inriver component, 1.0 for the weir and sonar counts, and 2.0 for the catch-effort model. 
#These parameter weights are the opposite of what the casual reader might expect, with smaller numbers indicating more weight and larger values indicating less weight. 
w_catch <- 2.0
w_escapement <- 1.0
w_inriver <- 0.5

weights <- c(w_catch,w_escapement,w_inriver) 

# Run Optim ============================================================================================================
optim_output  <- optim(par=pars_start, # starting values for parameter estimations 
                       fn=NLL, #NLL is function that you create above 
                       # data
                       B_yj=B_yj,
                       N_yi =N_yi,
                       obs_N=obs_N,
                       obs_escape_project=obs_escape_project,
                       #values
                       weeks=weeks,
                       projects=projects,
                       Nyear=Nyear,
                       err_variance =err_variance,
                       weights = weights,
                       method="BFGS",
                       hessian=FALSE,
                       control=list(trace=TRUE, maxit=1e6, #evalmax = 1e6,
                                    pgtol = 0, factr=0))

# # Access the estimated parameter values
param_est <- optim_output$par
param_est

# saveRDS(param_est,"output/optim_output_par.RDS")
