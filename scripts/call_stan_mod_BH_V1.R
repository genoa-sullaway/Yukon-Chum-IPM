library(rstan)
 
df <- read_csv("data/input_dat_stan.csv")
 
warmups <- 1000

total_iterations <- 2000

max_treedepth <-  10

n_chains <-  4

n_cores <- 4

adapt_delta <- 0.95

data <- list(n = nrow(df),
             r = df$r,
             ssb = df$ssb,
             max_r = max(df$r)
)




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
      h = 0.4,
      log_alpha = log(1 * data$max_r),
      log_beta = log(2* max(df$ssb))
    ),
    list(
      h = 0.21,
      log_alpha = log(3 * data$max_r),
      log_beta = log(.5 *max(df$ssb))
    ),
    list(
      h = 0.8,
      log_alpha = log(1 * data$max_r),
      log_beta = log(1.1*max(df$ssb))
    ),
    list(
      h = 0.3,
      log_alpha = log(.8 * data$max_r),
      log_beta = log(5*max(df$ssb))
    )
  ),
  control = list(max_treedepth = max_treedepth,
                 adapt_delta = adapt_delta)
)

#rstanarm::launch_shinystan(bh_fit)
