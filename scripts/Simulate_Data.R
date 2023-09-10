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

sim_spawners<-function(initial_population){
# Set parameters for the exponential growth model
num_time_steps <- 100    # Number of time steps
r <- rnorm(mean=0.01, sd=0.5, n=num_time_steps)  # Intrinsic growth rate
 
# Create an empty vector to store the population data
population <- numeric(num_time_steps)

# Initialize the first time step with the initial population size
population[1] <- initial_population

# Simulate the population dynamics using growth model
for (t in 2:num_time_steps) {
  population[t] <- population[t - 1] * exp(r[t])
}
 
return(population)
}

spawners_yukon_fall <- sim_spawners(initial_population = mean(yukon_fall$Estimated_Run))
spawners_kusko <- sim_spawners(initial_population = mean(kusko$pred_N_est))
spawners_yukon_spring <- sim_spawners(initial_population = mean(yukon_spring$Escapement))

# Plot the simulated time series
plot(1:100, spawners_yukon_fall , type = "l", xlab = "Time", ylab = "S")
plot(1:100, spawners_kusko , type = "l", xlab = "Time", ylab = "S")
plot(1:100, spawners_yukon_spring , type = "l", xlab = "Time", ylab = "S")

# now that I have a spawner population, assign alpha and beta and calculate recruits? 
# Bev Holt Function  ====================================================================================
bev_holt_sim_function <- function(spawners, alpha, beta){ # Initial population size
  #alpha <-0.019  # Parameter alpha
  #beta <- 9.3*10^-7      # Parameter beta
  num_time_steps <- 100     # Number of time steps

  # Create an empty vector to store the population data
  recruits <- numeric(num_time_steps)
  
  # Simulate the population dynamics using the Beverton-Holt model with alpha and beta
  for (t in 2:num_time_steps) {
    recruits[t] <- (alpha * spawners[t]) / (1 + beta * spawners[t])
  }
  
  # Create a time vector
  sr<-tibble("time" = c(1:num_time_steps),
                 "recruits" = recruits, "spawners"=spawners) %>%
    filter(!recruits == 0)
  
  return(sr)
}

# Simulate 3 stocks ===========

sim_yukon_fall <- bev_holt_sim_function(spawners = spawners_yukon_fall, alpha = 0.02, beta = 9.3*10^-6)

# Plot the simulated time series
plot(sim_yukon_fall$spawners, sim_yukon_fall$recruits, type = "p", xlab = "S", ylab = "R", 
     main = "Simulated Fall Yukon")

sim_kusko <- bev_holt_sim_function(spawners = spawners_kusko, alpha = 0.05, beta = 7*10^-6)

plot(sim_kusko$spawners, sim_kusko$recruits, type = "p", xlab = "S", ylab = "R", 
     main = "Simulated Kusko")

sim_yukon_spring <- bev_holt_sim_function(spawners = spawners_yukon_spring, 
                                          alpha = 0.02, beta = 7*10^-6)

plot(sim_yukon_spring$spawners, sim_yukon_spring$recruits, type = "p", xlab = "S", ylab = "R", 
     main = "Simulated Spring Yukon")
 