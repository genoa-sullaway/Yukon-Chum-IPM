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

#fs = 500 #
fs = 2440
Ps = 0.5
num_points <- 101

# set up fxn  ========================================================
sim_data<-function(min_ssb,max_ssb,true_p_1,true_p_2,
                   true_c_1,true_c_2,
                   true_sigma_y){

   
  
 # theta_1 <- -0.5
  # 
  # # Set the parameters for the ARMA process
  # phi <- 0.3  # Autoregressive coefficient (controls autocorrelation)
  # sigma <- 1  # Standard deviation of the white noise (controls variability)
  # 
  # # Simulate the AR(1) time series with mean 0
  # # right now no matrix just do one covariate... 
  # covar_mat_1 <- arima.sim(model = list(ar = c(phi), order = c(1, 0, 0)), n = num_points, sd = sigma)
  
  # Plot the simulated time series
  # plot(temperature, type = "l", col = "blue", xlab = "Time", ylab = "Temperature")
  
  # Population across multiple stages
  #N_fw <- runif(num_points, min_ssb, max_ssb/2)
  # add a starting value
  N_sp = matrix(nrow=num_points, ncol=1, NA)
  N_sp[1,] <- min_ssb
  
  obs_error_j  <- rnorm( num_points, 0, true_sigma_y)
  obs_error_sp  <- rnorm( num_points, 0, true_sigma_y)
  N_eggs = matrix(nrow=num_points,ncol=1,NA)
  kappa_fw =  matrix(nrow=num_points,ncol=1,NA)
  N_j =  matrix(nrow=num_points,ncol=1,NA)
  kappa_sp =  matrix(nrow=num_points,ncol=1,NA)
 
  for (i in 2:num_points) {
 
  N_eggs[i,] = fs*Ps*N_sp[i-1,]
  kappa_fw[i,] <- (true_p_1)/(1 + ((true_p_1*N_eggs[i,])/true_c_1)) # + obs_error_j[[i]] 
  if(  kappa_fw[i,] < 0){
    kappa_fw[i,] = kappa_fw[i,] * -1
  }
  N_j[i,] = (N_eggs[i,]*kappa_fw[i,]) + obs_error_j[[i]] 
  kappa_sp[i,] <- (true_p_2)/(1 + ((true_p_2*N_j[i,])/true_c_2))  
  if(  kappa_sp[i,] < 0){
    kappa_sp[i,] = kappa_sp[i,] * -1
  }
  N_sp[i,] = (N_j[i,]*kappa_sp[i,]) + obs_error_sp[[i]]  
  }
  
  # N_eggs[2:8]
  # kappa_fw[2:8]
  # kappa_sp[2:8]
  # N_j[2:8]
  # N_sp[2:8]
  
  survival <- c(kappa_fw,kappa_sp,N_j,N_sp)  
   
  # Create a data frame for the simulated data
  simulated_data <- data.frame(stage = c(rep("kappa_j",times = num_points),
                                         rep("kappa_sp", times = num_points),
                                         rep("N_j", times = num_points),
                                         rep("N_sp", times = num_points)), 
                               time = c(rep(1:num_points, times = 4)),
                               kappa_stage = survival)  %>%
                    spread(stage, kappa_stage)
     
   
  plota <- ggplot(data = simulated_data[2:101,], aes(x=time, y = kappa_j)) +
    geom_point()
  plotb <- ggplot(data = simulated_data[2:101,], aes(x=time, y = kappa_sp)) +
    geom_point()
  plotc <- ggplot(data = simulated_data[2:101,], aes(x=time, y = N_j)) +
    geom_point()
  plotd <- ggplot(data = simulated_data[2:101,], aes(x=time, y = N_sp)) +
    geom_point()
  
 plot <- ggarrange(plota,plotb,plotc,plotd)
  
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
 # saveRDS(covar_mat_1, "output/covar_temp_sim.RDS")
 
return(list(plot,simulated_data))
}
# Call function ================== 
 
sim_yukon_spring <- sim_data(min_ssb = min(yukon_spring$Escapement),#10 ,
                          max_ssb = max(yukon_spring$Escapement),#1000,
                          true_p_1 = 0.05, # egg to juvenile just taking this from mousalli and hilborn paper
                          true_p_2 = 0.15, # juvenile to adult
                          true_c_1 = 100000,
                          true_c_2 = 100000000,
                          true_sigma_y = 1000)
sim_yukon_spring[[1]]
test<-data.frame(sim_yukon_spring[[2]])

sim_yukon_fall <- sim_data(min_ssb = min(yukon_fall$Estimated_Run), 
                             max_ssb = max(yukon_fall$Estimated_Run), 
                           true_p_1 = 0.03,
                           true_p_2 = 0.18,
                           true_c_1 = 1000000,
                           true_c_2 = 1000000000,
                             true_sigma_y = 0.1)
sim_yukon_fall[[1]]

sim_kusko <- sim_data(min_ssb = min(kusko$pred_N_est),#10 ,
                            max_ssb = max(kusko$pred_N_est),#1000,
                            true_p_1 = 0.08,
                            true_p_2 = 0.14,
                            true_c_1 = 1000000,
                            true_c_2 = 1000000000,
                            true_sigma_y = 0.1)

sim_kusko[[1]]
 
write_csv(sim_yukon_spring[[2]][2:101,], "data/Simulated_Yukon_Spring.csv")
write_csv(sim_yukon_fall[[2]][2:101,], "data/Simulated_Yukon_Fall.csv")
write_csv(sim_kusko[[2]][2:101,], "data/Simulated_Kusko.csv")
