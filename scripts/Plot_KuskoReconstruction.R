# Start ==================================================================================================
#Project Name: AYK Chum Salmon Model
#Creator: Genoa Sullaway
#Date: Start May 2023
#
# Purpose: Compare predicted and observed from Kuskokwim Run Reconstruction model - output, optim is from Optimization_Kusko_reconstruction.R
# Load Packages =========================================================================================
library(tidyverse)
library(here)
 
estimated_parameters<-optim_output$par

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
 
  for (j in 1:Nyear) {
    pred_N[[j]] = pred_E[[j]] + obs_subsistence[[j]] + obs_catch[[j]] # resovled equation 2 so that it equals N, makes more sense to me to allow you to optimize that way??
    # pred_E[[j]] = (pred_N[[j]] - obs_subsistence[[j]] - obs_catch[[j]])
  }
 
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

pred_N<-data.frame(Year = c(1976:2007), Pred_N = c(pre_outputs[[3]]))  

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





