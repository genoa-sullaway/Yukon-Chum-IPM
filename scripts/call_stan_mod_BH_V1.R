library(rstan)

# to start, use simulated data from "scripts/simulate_data.R"
# first try with 1 stock, then will build heirarchical to 3 stocks

#df <- read_csv("data/input_dat_stan.csv")
#sim_yukon_fall

warmups <- 1000

total_iterations <- 4000

max_treedepth <-  15

n_chains <-  4

n_cores <- 4

adapt_delta <- 0.95

# List data for multiple stocks ================
# these are the files: 
# sim_yukon_spring
# sim_yukon_fall
# sim_kusko
# currently all the same length but probably wont be with real data


K = 3 # number of stocks involved
N = sum(N_stock)
N_stock <- c(nrow(sim_yukon_spring), nrow(sim_yukon_fall), nrow(sim_kusko))# N_Stock is the length of each timeseries for each stock

rec_list <- c(as.integer(sim_yukon_spring$recruits), 
                 as.integer(sim_yukon_fall$recruits),
                 as.integer(sim_kusko$recruits))

ssb_list <- c(as.integer(sim_yukon_spring$spawners),
                 as.integer(sim_yukon_fall$spawners),
                 as.integer(sim_kusko$spawners))
 
data_list <- list(N = N, 
                  K = K, 
                  N_stock = N_stock, 
                  rec = rec_list, 
                  ssb = ssb_list)

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

#rstanarm::launch_shinystan(bh_fit)
