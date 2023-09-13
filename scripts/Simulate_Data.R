library(here)
library(tidyverse)
library(readxl)

# Purpose =============================================================================
# per advice of Megan, simulate data with known parameters so you can make sure your code is running correctly
# This simulates spawners, basically using a random walk?  
# then uses beverton holt to calculate recruits based on known alpha and beta
# now I can use the spawner recruit to build a bayesian beverton holt estimation


# Load run reconstruction data  ========================================================
# to get mean to put in as initial population size for each stock
yukon_fall<- read_csv("data/Yukon_Escapement_ADFG/Yukon_Fall_Chum_RR_JTC.csv")  
yukon_spring <- read_excel("data/Yukon_Escapement_ADFG/Yukon Summer Chum Total Run 1978-2022 Run Rec.xlsx")

kusko_estimated_parameters<- readRDS("output/optim_output_par_data2021.RDS") 
kusko<-data.frame(Year = c(1988:(2022-1)),   
                   pred_N_est= as.vector(c(kusko_estimated_parameters[2:35])))  
# set up fxn  ========================================================
sim_data<-function(min_ssb,max_ssb,true_alpha1,
                       true_alpha2,true_beta1,true_beta2,
                       true_sigma_y){

  # Simulated spawners-per-recruit data
  num_points <- 100
  
  # Create synthetic ssb data spanning both stages
  ssb_stage1 <- runif(num_points, min_ssb, max_ssb / 2)
  ssb_stage2 <-  runif(num_points, max_ssb / 2, max_ssb)
  ssb <- c( ssb_stage2,ssb_stage1) # switched so that the larger population is first

  # Simulate recruitment data using the Beverton-Holt model
  recruitment_stage1 <- true_alpha1* ssb_stage1 / (1 + (true_beta1 * ssb_stage1))
  recruitment_stage2 <- true_alpha2* ssb_stage2 / (1 + (true_beta2 * ssb_stage2))
  r <- c(recruitment_stage2,recruitment_stage1)
  
  # Add observation error
  obs_error <- rnorm(length(r), 0, true_sigma_y)
  observed_r <- r + obs_error
  
  # Create a data frame for the simulated data
  simulated_data <- data.frame(group = c(rep(1,times = num_points), rep(2, times = num_points)),
                               time = c(rep(1:100, times = 2)),
                               ssb = ssb, r = observed_r)
  plot <- ggplot(data = simulated_data, aes(x=ssb, y = r)) + 
    geom_point() + 
    facet_wrap(~group )
  
# num_time_steps <- 100    # Number of time steps
# r <- rnorm(mean=0.03, sd=0.5, n=num_time_steps)  # Intrinsic growth rate
#  
# # Create an empty vector to store the population data
# population <- numeric(num_time_steps)
# 
# # Initialize the first time step with the initial population size
# population[1] <- initial_population
#  
# # Simulate the population dynamics using growth model
# for (t in 2:num_time_steps) {
#   population[t] <- population[t - 1] * exp(r[t])  

return(list(plot,simulated_data ))
}

sim_yukon_spring <- sim_data(min_ssb = min(yukon_spring$Escapement),#10 ,
                          max_ssb = max(yukon_spring$Escapement),#1000,
                          true_alpha1 = 0.02,
                          true_alpha2 = 0.02,
                          true_beta1 = 7*10^-6,
                          true_beta2 = 7*10^-6,
                          true_sigma_y = 10)
sim_yukon_spring[[1]]

sim_yukon_fall <- sim_data(min_ssb = min(yukon_fall$Estimated_Run),#10 ,
                             max_ssb = max(yukon_fall$Estimated_Run),#1000,
                             true_alpha1 = 0.02,
                             true_alpha2 = 0.02,
                             true_beta1 = 9.3*10^-6,
                             true_beta2 = 9.3*10^-6,
                             true_sigma_y = 10)
sim_yukon_fall[[1]]

sim_kusko <- sim_data(min_ssb = min(kusko$pred_N_est),#10 ,
                             max_ssb = max(kusko$pred_N_est),#1000,
                             true_alpha1 = 0.05,
                             true_alpha2 = 0.05,
                             true_beta1 = 7*10^-6,
                             true_beta2 = 7*10^-6,
                             true_sigma_y = 10)

sim_kusko[[1]]
 
write_csv(sim_yukon_spring[[2]], "data/Simulated_Yukon_Spring.csv")
write_csv(sim_yukon_fall[[2]], "data/Simulated_Yukon_Fall.csv")
write_csv(sim_kusko[[2]], "data/Simulated_Kusko.csv")
