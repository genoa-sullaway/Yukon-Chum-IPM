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

# This is proportions in each area/week/year - Pyj - right now, just fit for 2008
prop<- read_csv("data/Processed_Data/OLD/OLD_Proportions_run_present_weekly.csv") %>% # only select some weeks for now because proportion has less weeks than the effort data...  
  mutate(year = 1976:(1976+nrow(.)-1)) %>%
  filter(year < 2008) %>%
  dplyr::select(-year) %>% 
  dplyr::select(c(2:14)) #getting rid of first and last week because of 0's 

#  observed catch per week 
obs_catch_week <- read_csv("data/Processed_Data/OLD/OLD_catch_week.csv") %>% 
  filter(year < 2008) %>%
  dplyr::select(-year) 

# Observed effort 
effort <- read_csv("data/Processed_Data/OLD/OLD_effort.csv") %>%
  filter(year < 2008) 
# format effort for equation 
B_yj = as.matrix(effort[,1:13])  

#years <- c(1976:1976+nrow(obs_catch_week)-1)  #1976:2021 #length of years in dataset
Nyear <- as.numeric(nrow(prop))
T =  Nyear*4 # "Total number of observations from all data sets" page 6 -here: number of years * 4 data sets. is this right? 
weeks = as.numeric(ncol(prop))
 
# NLL Function =========================================================================================
NLL <- function(par,
                data,
                weeks,
                projects,
                Nyear,
                weights){ 
 
  # Step 1: Extract parameters and data  
 # grep("ln_q_vec", par_names)
  ln_q_vec <- par[1] 
  
  #grep("pred_N", par_names)
  ln_pred_N <- par[2:33]
  
  q_vec <- exp(ln_q_vec)
  pred_N <- exp(ln_pred_N)
  
#set up data in vectors 
    # I vectorized to see if that makes a difference (it doesnt)
  B_yj=data$B_yj 
  obs_catch_week=data$obs_catch_week
  
  B_yj_vec <- as.vector(B_yj)
  obs_catch_week_vec <- as.vector(as.matrix(obs_catch_week))
  
  N_yi_vec_prop <-as.vector(as.matrix(pred_N*prop))
  
  # Step 2: Predict C 
    # Predict C - Catch, using Baranov catch equation: ============================================================================

  # Catch equation
  # N is # of fish (week by year)
  # B is effort (week by year)
  # for right now estimate 1 q for whole data set
 pred_catch = N_yi_vec_prop*(1-(exp(-q_vec*B_yj_vec))) 
     
 # calculate negative log liklihood
 # add a small constant to avoid log 0's
 NLL_catch  <- dnorm(x=log(obs_catch_week_vec+1e-6), mean=log(pred_catch+1e-6),
                          sd = 0.1, log = TRUE)     
 
 NLL <- -1 * (weights[1] * sum(NLL_catch, na.rm = TRUE))
 
# Return the total objective function value
return(NLL)
 
}

# Parameters and parameter starting values ===================================================================

# Baranov parameters:
ln_q_vec <- log(0.0002 ) 
ln_pred_N <- rep(log(94832),Nyear)

pars_start<- c( 
  ln_q_vec,
  ln_pred_N)
  
par_names <- c(
  "ln_q_vec",
   paste0("ln_pred_N", c(1:Nyear)))#,
  
w_catch <- 2.0
w_escapement <- 1.0
w_inriver <- 0.5
weights <- c(w_catch,w_escapement,w_inriver) 

data <- list(B_yj=B_yj,  obs_catch_week=obs_catch_week )

#check that NLL fxn works on its own, it does
NLL(par=pars_start,
    data =data,
     weeks=weeks,
     projects=projects,
     Nyear=Nyear,
     weights = weights)

# Run Optim ============================================================================================================
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

# optim_output  <- stats::optim(par=pars_start, # starting values for parameter estimations
#                               fn=NLL, #NLL is function that you create above
#                               # data
#                               data =data,
#                               # values
#                               weeks=weeks,
#                               projects=projects,
#                               Nyear=Nyear,
#                               weights = weights,
#                               method="BFGS",
#                               # lower = c(0.0000004, 60000),
#                               # upper = c(0.5, 1e9),
#                               control = list(maxit = 1e4, pgtol = 0, factr=0, trace = 1))

# saveRDS(param_est,"output/optim_output_par.RDS")

