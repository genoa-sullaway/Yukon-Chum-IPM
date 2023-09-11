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
 
data <- list(N = nrow(sim_yukon_spring),
             rec = as.integer(sim_yukon_spring$recruits),
             ssb = as.integer(sim_yukon_spring$spawners),
             max_r = max(sim_yukon_spring$recruits))

bh_fit <- stan(
  file = here::here("scripts", "stan_mod_BH_V1.stan"),
  data = data,
  chains = n_chains,
  warmup = warmups,
  iter = total_iterations,
  cores = n_cores,
  refresh = 250,
  init = list(
    #  alpha = 0.02, beta = 7*10^-6)
    list(
      log_alpha = log(0.02),
      log_beta = log(7*10^-6)
    ),
    list(
      log_alpha = log(0.025),
      log_beta = log(7.3*10^-6)
    ),
    list(
      log_alpha = log(0.04),
      log_beta = log(7.1*10^-6)
    ),
    list(
      log_alpha = log(0.011),
      log_beta = log(7.13*10^-6)
    )
  ),
  control = list(max_treedepth = max_treedepth,
                 adapt_delta = adapt_delta)
)

#rstanarm::launch_shinystan(bh_fit)
