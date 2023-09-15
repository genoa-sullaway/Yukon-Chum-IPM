library(rstan)
library(tidyverse)
library(here)

# load simulated data =======================================================
# to start, use simulated data from "scripts/simulate_data.R" 
sim_yukon_spring_df <- read_csv("data/Simulated_Yukon_Spring.csv")
sim_yukon_fall_df <- read_csv("data/Simulated_Yukon_Fall.csv")
sim_kusko_df <- read_csv("data/Simulated_Kusko.csv")

# load Covariates  ==========================================================
covar_temp <- data.frame(temp = c(readRDS("output/covar_temp_sim.RDS")))
covar_1 <- rep(covar_temp$temp, times = 3)
# setup inputs ==============================================================
warmups <- 1000
total_iterations <- 4000
max_treedepth <-  15
n_chains <-  4
n_cores <- 4
adapt_delta <- 0.95

# Organize data call inputs ================================================
K = 3 # number of stocks involved
N = sum(N_stock)
N_stock <- c(nrow(sim_yukon_spring_df), nrow(sim_yukon_fall_df), nrow(sim_kusko_df))  
g = c(rep(1, times = N_stock[1]),  # Vector of group assignments.
      rep(2, times = N_stock[2]),
      rep(3, times = N_stock[3]))
ncovars = 1 # right now just trying with temperature for stage 1      

stage_a <- c(as.integer(sim_yukon_spring_df$a), 
                 as.integer(sim_yukon_fall_df$a),
                 as.integer(sim_kusko_df$a))

stage_b <- c(as.integer(sim_yukon_spring_df$b), 
                as.integer(sim_yukon_fall_df$b),
                as.integer(sim_kusko_df$b))

stage_c <- c(as.integer(sim_yukon_spring_df$c), 
                as.integer(sim_yukon_fall_df$c),
                as.integer(sim_kusko_df$c))

data_list <- list(N = N, 
                  K = K, 
                  g = g,
                  N_stock = N_stock, 
                  stage_a = stage_a, 
                  stage_b = stage_b,
                  stage_c = stage_c,
                  ncovars = ncovars,
                  covar_1 = covar_1)

# data <- list(N = nrow(sim_yukon_spring),
#              rec = as.integer(sim_yukon_spring$recruits),
#              ssb = as.integer(sim_yukon_spring$spawners),
#              max_r = max(sim_yukon_spring$recruits))

bh_fit <- stan(
  file = here::here("scripts", "stan_mod_BH_V1.stan"),
  data = data_list,
  chains = n_chains,
  warmup = warmups,
  iter = total_iterations,
  cores = n_cores,
  refresh = 250,
  # init = list(
  #   #  alpha = 0.02, beta = 7*10^-6)
  #   list(
  #     log_alpha = log(0.02),
  #     log_beta = log(7*10^-6)
  #   ),
  #   list(
  #     log_alpha = log(0.025),
  #     log_beta = log(7.3*10^-6)
  #   ),
  #   list(
  #     log_alpha = log(0.04),
  #     log_beta = log(7.1*10^-6)
  #   ),
  #   list(
  #     log_alpha = log(0.011),
  #     log_beta = log(7.13*10^-6)
  #   )
  # ),
  
  control = list(max_treedepth = max_treedepth,
                 adapt_delta = adapt_delta)
)
 
