library(here)
library(tidyverse)
library(readxl)
library(ggpubr)
library(stats)

set.seed(123)  # Set a seed for reproducibility

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
  
  # make covariate =============
  theta_1 <- -0.5

  # Set the parameters for the ARMA process
  phi <- 0.3  # Autoregressive coefficient (controls autocorrelation)
  sigma <- 1  # Standard deviation of the white noise (controls variability)

  # Simulate the AR(1) time series with mean 0
  # right now no matrix just do one covariate... 
  covar_mat_1 <- arima.sim(model = list(ar = c(phi), order = c(1, 0, 0)), n = num_points, sd = sigma)
  
  # Plot the simulated time series
  # plot(temperature, type = "l", col = "blue", xlab = "Time", ylab = "Temperature")
  
  # Population across multiple stages
  N_stage_a <- runif(num_points, min_ssb, max_ssb / 2)
  N_stage_b <- (true_alpha1* N_stage_a) /  (1+ (true_beta1 * N_stage_a)) + (theta_1*covar_mat_1)
  N_stage_c <- (true_alpha2* N_stage_b) / (1+ (true_beta2 * N_stage_b))
  #r <- c(N_stage_c,N_stage_b,N_stage_a)
  r <- c(N_stage_a,N_stage_b,N_stage_c)  
  # Add observation error
  obs_error <- rnorm(length(r), 0, true_sigma_y)
  observed_r <- r + obs_error
  
  # Create a data frame for the simulated data
  simulated_data <- data.frame(stage = c(rep("a",times = num_points), 
                                         rep("b", times = num_points),
                                         rep("c", times = num_points)),
                               time = c(rep(1:100, times = 3)),
                                n_stage = observed_r)  %>%
                    spread(stage, n_stage)  
    
  
  plota <- ggplot(data = simulated_data, aes(x=a, y = b)) + 
    geom_point()  
  plotb <- ggplot(data = simulated_data, aes(x=b, y = c)) + 
    geom_point()
  plot <- ggarrange(plota,plotb)
  
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
  
# save covariate, right now same for each river so just saving one... this will change
  saveRDS(covar_mat_1, "output/covar_temp_sim.RDS")
  
return(list(plot,simulated_data ))
}

sim_yukon_spring <- sim_data(min_ssb = min(yukon_spring$Escapement),#10 ,
                          max_ssb = max(yukon_spring$Escapement),#1000,
                          true_alpha1 = 0.2,
                          true_alpha2 = .06,
                          true_beta1 = 9.3*10^-6,
                          true_beta2 =  9.3*10^-6,#2*10^-6,
                          true_sigma_y = 10)
sim_yukon_spring[[1]]

sim_yukon_fall <- sim_data(min_ssb = min(yukon_fall$Estimated_Run),#10 ,
                             max_ssb = max(yukon_fall$Estimated_Run),#1000,
                             true_alpha1 = 0.02,
                             true_alpha2 = 0.06,
                             true_beta1 = 9.3*10^-6,
                             true_beta2 = 9.3*10^-6,
                             true_sigma_y = 10)
sim_yukon_fall[[1]]

sim_kusko <- sim_data(min_ssb = min(kusko$pred_N_est),#10 ,
                             max_ssb = max(kusko$pred_N_est),#1000,
                             true_alpha1 = 0.05,
                             true_alpha2 = 0.07,
                             true_beta1 = 7*10^-6,
                             true_beta2 = 7*10^-6,
                             true_sigma_y = 10)

sim_kusko[[1]]
 
write_csv(sim_yukon_spring[[2]], "data/Simulated_Yukon_Spring.csv")
write_csv(sim_yukon_fall[[2]], "data/Simulated_Yukon_Fall.csv")
write_csv(sim_kusko[[2]], "data/Simulated_Kusko.csv")
