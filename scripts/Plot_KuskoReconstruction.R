# Start ==================================================================================================
#Project Name: AYK Chum Salmon Model
#Creator: Genoa Sullaway
#Date: Start May 2023
#
# Purpose: Compare predicted and observed from Kuskokwim Run Reconstruction model - output, optim is from Optimization_Kusko_reconstruction.R
# Load Packages =========================================================================================
library(tidyverse)
library(here)
# 
# ln_q_vec_par <- exp(optim_output$par[1])
# baranov_sigma_par <- exp(optim_output$par[2])
# escapement_slope_par <- exp(optim_output$par[3:11])
# 
# escapement_sigma_par <- exp(optim_output$par[12])
# N_sigma_par <- exp(optim_output$par[13])

estimated_parameters<-optim_output$par

par_names <- c(# baranov parameters
  "ln_q_vec",
  "baranov_sigma",
  #  escapement parameters 
  paste0("escapement_slope", c(1:projects)),
  "escapement_sigma", 
  # total return parameters
  "N_sigma")  

#assign weights 
names(estimated_parameters) <- par_names
# I think to plot I need to recreate the NLL function without the LL components to then get pred N and compare it to obs N?

# NLL Function =========================================================================================
predict_NLL <- function(pars,
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
  ln_q_vec <- exp(pars[1]) # 
  grep("ln_q_vec", par_names)
  
  baranov_sigma <- exp(pars[2]) # 
  grep("baranov_sigma", par_names)
  
  escapement_slope <- exp(pars[3:11]) # 
  grep("escapement_slope", par_names)
  
  escapement_sigma <- exp(pars[12])
  grep("escapement_sigma", par_names)
  
  N_sigma <- exp(pars[13])
  grep("N_sigma", par_names)
  
  
  # Step 2: Predict C, N, E
  # Predict C - Catch, using Baranov catch equation: =========================================================================================
  
  pred_catch <- matrix(ncol = weeks, nrow = Nyear)
  error <- matrix(ncol = weeks, nrow = Nyear)
  
  for (i in 1:weeks) {
    for (j in 1:Nyear) {
      error[j,i] <- rnorm(0,baranov_sigma, n=1)
      pred_catch[j,i] = N_yi[j,i]*(1-exp(-ln_q_vec*B_yj[j,i]))*exp(error[j,i])
    }
  }
  
  pred_catch[is.na(pred_catch)] <- 0
  colnames(pred_catch) <- names(prop)
  
  # Predict N - Observed Total Return =========================================================================================
  
  #I am not sure how to do this part
  lambda = rnorm(0,N_sigma, n=Nyear)
  pred_N = obs_N*exp(lambda) # the paper actually has this equation:  pred_N = pred_N*exp(lamba), but I dont understand how that works because then where does the pred_N come from??? 
  
  # Predict E - Escapement =========================================================================================
  
  pred_escape_pj <- matrix(ncol = projects, nrow = Nyear)
  
  for (p in 1:projects) {
    for (j in 1:Nyear) {
      pred_escape_pj[j,] = escapement_slope[p]*obs_escape_project[j,]
    }
  }
  
  colnames(pred_escape_pj) <- colnames(obs_escape_project)
  
###########################################################################3
 output <- list(pred_catch,pred_escape_pj,pred_N)
  # Return the predicted values based on parameter estimates in optimization script 
  return(output)
}

pre_outputs <- predict_NLL(par=estimated_parameters, # starting values for parameter estimations 
                           # data/fixed values go below
                           N_yi=N_yi,
                           B_yj=B_yj,
                           obs_N=obs_N,
                           obs_escape_project=obs_escape_project,
                           #values
                           weeks=weeks,
                           projects=projects,
                           Nyear=Nyear,
                           weights = weights)


pred_catch<-pre_outputs[[1]]
pred_escape_pj<-pre_outputs[[2]]

pred_N<-data.frame(Year = c(1976:2021), Pred_N = c(pre_outputs[[3]]))  

### Plot predicted N
ggplot(data = pred_N,aes(x=Year, y = Pred_N/1000)) +
  geom_bar(stat= "identity") +
  theme_classic() +
  ylab("Total Run (thousands of fish") +
  geom_vline(xintercept = 2000) + 
  geom_vline(xintercept = 2007)  

### Plot predicted N
ggplot(data = pred_N,aes(x=Year, y = Pred_N/1000)) +
  geom_point() +
  geom_line() + 
  theme_classic() +
  ylab("Total Run (thousands of fish") +
  geom_vline(xintercept = 2007, linetype =2, color ="blue")  + #end of Bue study
  geom_vline(xintercept = 1986, linetype =2, color ="blue")  # start of Bue study





