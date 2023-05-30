# Start ==================================================================================================
# this script is an older version I pulled from Git ot help wiht troubleshooting, from May10 commit. 
#Project Name: AYK Chum Salmon Model
#Creator: Genoa Sullaway
#Date: Start March 2023
#
# Purpose: Recreate ADFG Kuskokwim Chum Run reconstruction in R based on excel sheet
# based on Bue and Molyneaux Kuskokwim chum salmon run reconstruction 
#
# Load Packages =========================================================================================
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
projects = 7 # number of weir projects 
T =  Nyear*4 # "Total number of observations from all data sets" page 6 - I am not sure if 36 is right?? 
weeks = ncol(prop)

# Set up data that are inputs to likelihood fxns =========================================================================================
obs_escape_project <- as.matrix(escapement[,2:8])
obs_escape <- as.matrix(rowSums(escapement[,2:8]))
obs_catch <- as.matrix(rowSums(catch[,2:3]))
obs_commercial <- as.matrix(catch[,2])
obs_subsistence <- as.matrix(catch[,3])
obs_N = as.matrix(obs_escape + obs_subsistence + obs_catch) # on Page 5 of model paper this is N_y, in excel this is "# of fish accounted for"
colnames(obs_N)<- NULL
 
# Baranov Data Input =========================================================================================

# this is a data input to the Baranov catch equation prediction (?) though notes say it is Nhat ?? 
N_yi = as.matrix(obs_N*prop)  # number of chum present in commercial district by week/year
B_yj = as.matrix(effort[,1:13]) # observed effort per week/year

# NLL Function =========================================================================================
NLL <- function(pars,
                #data
                N_yi,
                B_yj,
                obs_N,
                obs_escape_project,
                #values
                weeks,
                projects,
                Nyear,
                weights
                
                # ln_q_vec, # parameter- Baranov
                # baranov_sigma, # parameter - Baranov
                # ln_slope, # parameter
                
                # ln_sigma_C,  
                ) { 
  
  # Extract parameters, based on their location
  ln_q_vec <- pars_start[1] # 
  grep("ln_q_vec", par_names)
  
  # baranov_sigma <- pars_start[2] # 
  # grep("baranov_sigma", par_names)
  # 
  escapement_slope <- pars_start[2:8] # 
  grep("escapement_slope", par_names)
  
  # N_sigma <- pars_start[10]
  # grep("N_sigma", par_names)
  
  # Step 1: Exponentiate model parameters back into normal space
  # q <- exp(ln_q)
  # slope <- exp(ln_slope)
  # 
  # Step 2: Predict C, N, E
  
  # Predict C - Catch, using Baranov catch equation: =========================================================================================

    pred_catch <- matrix(ncol = weeks, nrow = Nyear)
  
    error <- matrix(ncol = weeks, nrow = Nyear)
  
    for (i in 1:weeks) {
      for (j in 1:Nyear) {
      error[j,i] <- 1# rnorm(0,0, n=1)
      pred_catch[j,i] = N_yi[j,i]*(1-exp(-ln_q_vec*B_yj[j,i]))*exp(error[j,i])
      }
    }
    
    pred_catch[is.nan(pred_catch)] <- 0
    
    colnames(pred_catch) <- names(prop)
    
    # Predict N - Observed Total Return =========================================================================================

    #I am not sure how to do this part
    lambda = 1#rnorm(0,N_sigma, n=Nyear)
    pred_N = obs_N*exp(lambda) # the paper actually has this equation:  pred_N = pred_N*exp(lamba), but I dont understand how that works because then where does the pred_N come from??? 
  
    # Predict E - Escapement =========================================================================================
    
    # not sure where this is used yet... error_fixed_escape <- rnorm(0, 1, n=36) # currently fixing sigma here
    pred_escape_pj <- matrix(ncol = projects, nrow = Nyear)
    
    for (p in 1:projects) {
      for (j in 1:Nyear) {
        pred_escape_pj[j,] = escapement_slope[p]*obs_escape_project[j,]
      }
    }
    
    colnames(pred_escape_pj) <- colnames(obs_escape_project)
    
    # not sure where this is used yet... pred_escape = (pred_N-obs_subsistence-obs_commercial)*exp(error_fixed_escape)   
    # pred_N is the last thing optimized
    
  # Step 3: Extract model-predicted quantities for comparison to our observed data
    # From paper re weights: "A maximum likelihood model that allowed for the weighting (wi , wc , and wN) of individual datasets was used"
   
    sumpred_catch<-rowSums(pred_catch)
    for (j in 1:Nyear) {
       if(sumpred_catch[j] > 0) { # Ensure we don't try to take the log of 0!
     NLL_catch <- sum((log(obs_catch[j]) - log(sumpred_catch[j])))^2/(weights[1]^2) # catch is summed for the pred_catch, because it is estimated based on proportion of vessels in an area per week and we dont have that ifnormation for observed catch,
   # total annual observed catch is subtracted from total annual predicted catch, then squared, divided by weights, and finally, these differences are summed across all years. 
       }
    }

    sumpred_escape<-rowSums(pred_escape_pj)
    for (j in 1:Nyear) {
      if(sumpred_escape[j] > 0) {
    NLL_escapement <- sum((log(obs_escape[j]) - log(sumpred_escape[j])))^2/(weights[2]^2) 
        }
    }
    
    #no 0's here so I dont think I have to do the loop.....
    NLL_num_fish <- sum((log(obs_N) - log(pred_N))^2)/(weights[3]^2) 
   
    # Calculate total objective function 
   objFxn <- T/2*log(NLL_catch + NLL_escapement + NLL_num_fish) # T is number of obs.... 
  
  # Return the total objective function value
  return(objFxn)
}

# Parameters and parameter starting values ===================================================================

# Baranov parameters:
  ln_q_vec <- 0.25 #0.001 - 0.5 is standard 
  baranov_sigma <- 0.1  
  
  escapement_slope <-rep(0.5, times = projects) # need to have its own list of slopes for each project 
  escapement_sigma <- 0.1
  
  N_sigma <- 0.1
  
  pars_start<- c(# Baranov 
                    ln_q_vec,
                  #  baranov_sigma,
                    # escapement parameters 
                    escapement_slope#,
                   # escapement_sigma,
                    # Total return parameters
                    #N_sigma # variation for lambda
                    ) 
  
par_names <- c(# baranov parameters
                          "ln_q_vec",
                         # "baranov_sigma",
                         #  escapement parameters 
                           paste0("escapement_slope", c(1:projects))#,
                         # "escapement_sigma", 
                         # total return parameters
                        #  "N_sigma"
                        )  
  #assign weights 
  w_c <- 0.5
  w_i <- 0.3
  w_n <- 0.2
  
  weights <- c(w_c, w_i, w_n) 

# Run Optim ============================================================================================================
  
optim_output  <- optim(par=pars_start, # starting values for parameter estimations 
                       fn=NLL, #NLL is function that you create above 
                       # data/fixed values go below
                       N_yi=N_yi,
                       B_yj=B_yj,
                       obs_N=obs_N,
                       obs_escape_project=obs_escape_project,
                       #values
                       weeks=weeks,
                       projects=projects,
                       Nyear=Nyear,
                       weights = weights,
                       method="BFGS",
                       hessian=FALSE,
                       control=list(trace=TRUE, maxit=1e5))
  
  
 
  # Print the result
  print(optim_output)
  
  # Access the estimated parameter values
  param_est <- optim_output$par

  
  # Access the objective function value at the estimated parameters
  obj_fun_val <- optim_output$value
  print(obj_fun_val)
  
  # Access the convergence code
  conv_code <- optim_output$conv
  print(conv_code)
  
 