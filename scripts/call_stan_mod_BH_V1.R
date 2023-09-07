library(rstan)

# to start, use simulated data from "scripts/simulate_data.R"
# first try with 1 stock, then will build heirarchical to 3 stocks

#df <- read_csv("data/input_dat_stan.csv")
sim_yukon_fall

warmups <- 1000

total_iterations <- 2000

max_treedepth <-  10

n_chains <-  4

n_cores <- 4

adapt_delta <- 0.95

data <- list(n = nrow(sim_yukon_fall),
             r = sim_yukon_fall$recruits,
             ssb = sim_yukon_fall$spawners,
             max_r = max(sim_yukon_fall$recruits))

bh_fit <- stan(
  file = here::here("scripts", "stan_mod_BH_V1.stan"),
  data = data,
  chains = n_chains,
  warmup = warmups,
  iter = total_iterations,
  cores = n_cores,
  refresh = 250,
  init = list(
    list(
      log_alpha = log(1 * data$max_r),
      log_beta = log(2* max(df$ssb))
    ),
    list(
      log_alpha = log(3 * data$max_r),
      log_beta = log(.5 *max(df$ssb))
    ),
    list(
      log_alpha = log(1 * data$max_r),
      log_beta = log(1.1*max(df$ssb))
    ),
    list(
      log_alpha = log(.8 * data$max_r),
      log_beta = log(5*max(df$ssb))
    )
  ),
  control = list(max_treedepth = max_treedepth,
                 adapt_delta = adapt_delta)
)

#rstanarm::launch_shinystan(bh_fit)
