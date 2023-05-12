# Start ==================================================================================================
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
escapement <- read_csv("data/Processed_Data/kusko_escapement.csv") %>%
  filter(year < 2008) 
catch<-read_csv("data/Processed_Data/catch.csv") %>%
  filter(Year < 2008) 
# Effort 
effort <- read_csv("data/Processed_Data/effort.csv") %>%
  filter(year < 2008) 
# proportions in each area/week/year - Pyj - right now, just fit for 2011
prop<- read_csv("data/Processed_Data/Proportions_run_present_weekly.csv")[4:10] %>% # only select some weeks for now because proportion has less weeks than the effort data...  
  mutate(year = 1976:2021) %>%
  filter(year < 2008) %>%
  select(-year)

years <-unique(escapement$year)  #1976:2021 #length of years in dataset
Nyear <- length(years)
projects = ncol(escapement)-1 # number of weir projects - the year column
T =  Nyear*4 # "Total number of observations from all data sets" page 6 -here: number of years * 4 data sets. is this right? 
weeks = ncol(prop)

# Set up data that are inputs to likelihood fxns =========================================================================================
obs_escape_project <- as.matrix(escapement[,1:9])
obs_escape <- as.matrix(rowSums(escapement[,1:9]))
obs_catch <- as.matrix(rowSums(catch[,2:3]))
obs_commercial <- as.matrix(catch[,2])
obs_subsistence <- as.matrix(catch[,3])
#equation 6 in Bue paper 
obs_N = as.matrix(obs_escape + obs_subsistence + obs_catch)# +catch[,4] + catch[,5]) # on Page 5 of model paper this is N_y, in excel this is "# of fish accounted for"
colnames(obs_N)<- NULL
 
# Baranov Data Input =========================================================================================
N_yi = as.matrix(obs_N*prop)  # number of chum present in commercial district by week/year
B_yj = as.matrix(effort[,1:7]) # observed effort per week/year

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
                weights) { 
  
  # Step 1: Extract parameters, based on their location
  
  grep("ln_q_vec", par_names)
  ln_q_vec <- pars_start[1] # 
  
  # baranov_sigma <- pars_start[2] # 
  # grep("baranov_sigma", par_names)
  grep("escapement_slope", par_names)  
  escapement_slope <- pars_start[2:10] # 
  
  grep("pred_N", par_names)  
  pred_N <- pars_start[11:42] 
  
  # escapement_sigma <- pars_start[12]
  # grep("escapement_sigma", par_names)
  
  # N_sigma <- pars_start[13]
  # grep("N_sigma", par_names)
  
  # Step 2: Predict C, N, E
  
  # Predict C - Catch, using Baranov catch equation: =========================================================================================

    pred_catch <- matrix(ncol = weeks, nrow = Nyear)
  
    error <- matrix(0, ncol = weeks, nrow = Nyear)
  
    for (i in 1:weeks) {
      for (j in 1:Nyear) {
     # error[j,i] <- rnorm(0,baranov_sigma, n=1)
      pred_catch[j,i] = N_yi[j,i]*(1-exp(-ln_q_vec*B_yj[j,i]))*exp(error[j,i])
      }
    }
    
    pred_catch[is.na(pred_catch)] <- 0
    
    colnames(pred_catch) <- names(prop)

    # Predict E - Escapement =========================================================================================
 
    #colnames(pred_escape_pj) <- colnames(obs_escape_project)
      pred_escape_pj <-matrix(NA, ncol = projects, nrow = Nyear) #obs_escape_project #"starting values" matrix(ncol = projects, nrow = Nyear)
     
     # pred_E <- matrix(ncol = projects, nrow = Nyear)
     
     for (p in 1:projects) {
       for (j in 1:Nyear) {
         pred_escape_pj[j,p] = escapement_slope[p]*obs_escape_project[j,p]
       }
     }
     
     pred_E <- rowSums(pred_escape_pj)
     
     # Predict N - Observed Total Return =========================================================================================
     
     # blank this out for now unless I decide to add in process variation?? 
     #lambda = rnorm(0,0, n=Nyear)
     #pred_N = pred_N*exp(lambda) # this is kind of useless but ill leave it in for now
     
     for (j in 1:Nyear) {
       pred_N[[j]] = pred_E[[j]] + obs_subsistence[[j]] + obs_catch[[j]] # resovled equation 2 so that it equals N, makes more sense to me to allow you to optimize that way??
       # pred_E[[j]] = (pred_N[[j]] - obs_subsistence[[j]] - obs_catch[[j]])
     }
     
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
    
 
    for (j in 1:Nyear) {
      if(pred_N[j] > 0) {
    NLL_N_TotalRun <- sum((log(obs_N) - log(pred_N))^2)/(weights[3]^2) 
        }
    }
    
    # Calculate total objective function 
   objFxn <- T/2*log(NLL_catch + NLL_escapement + NLL_N_TotalRun) # T is number of obs.... 
  
  # Return the total objective function value
  return(objFxn)
}

# Parameters and parameter starting values ===================================================================

# Baranov parameters:
  ln_q_vec <- 0.25 #0.001 - 0.5 is standard 
  #baranov_sigma <- 0.1  
  
  escapement_slope <- rep(0.5, times = projects) # need to have its own list of slopes for each project 
 # escapement_sigma <- 0.1
  pred_N <- obs_N #matrix(nrow = Nyear, ncol =1, obs_N) # starting values are just observed values?? 
 # N_sigma <- 0.1
  
  pars_start<- c(# Baranov 
                    ln_q_vec,
                    #baranov_sigma,
                    # escapement parameters 
                    escapement_slope,
                    pred_N
                   # escapement_sigma,
                    # Total return parameters
                    #N_sigma # variation for lambda
                    ) 
  
par_names <- c(# baranov parameters
                          "ln_q_vec",
                          #"baranov_sigma",
                         #  escapement parameters 
                          paste0("escapement_slope", c(1:projects)),
                          paste0("pred_N", c(1:Nyear))
                         #"escapement_sigma", 
                         # total return parameters
                         # "N_sigma"
                         )  
  #assign weights 
#The parameter weighting scheme used for the demonstration was 0.5 for the inriver component, 1.0 for the weir and sonar counts, and 2.0 for the catch-effort model. 
#These parameter weights are the opposite of what the casual reader might expect, with smaller numbers indicating more weight and larger values indicating less weight. 
  w_catch <- 2.0
  w_escapement <- 1.0
  w_inriver <- 0.5
  
  weights <- c(w_catch,w_escapement,w_inriver) 

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
  
  
  # 
  # # Print the result
    print(optim_output)
  # 
  # # Access the estimated parameter values
   param_est <- optim_output$par
 
   
   
  # # Access the objective function value at the estimated parameters
  # obj_fun_val <- optim_output$value
  # print(obj_fun_val)
  # 
  # # Access the convergence code
  # conv_code <- optim_output$conv
  # print(conv_code)
  # 
  # 