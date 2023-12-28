library(rstan)
library(tidyverse)
library(here)
library(bayesplot)
library(rstanarm) 

# load simulated data =======================================================
# to start, use simulated data from "scripts/simulate_data.R" 
#sim_dat <- read_csv("data/Simulated_DatBH.csv")

sim_dat <- readRDS("data/Simulated_DatBH.RDS")

# sim_yukon_fall_df <- read_csv("data/Simulated_Yukon_Fall.csv")
# sim_kusko_df <- read_csv("data/Simulated_Kusko.csv")

# load Covariates  ==========================================================

# setup inputs ==============================================================
warmups <- 1000
total_iterations <- 4000
max_treedepth <-  15
n_chains <- 1
n_cores <- 4
adapt_delta <- 0.95

Ps <- 0.5 # proportion of females - assumption, need to lit check
fs <- 2440 # fecundity - Gilk and Baumer 2009 estimate for Kusko Chum

# Organize data call inputs ================================================
K = 3 # number of stocks involved
N_stock <- c(nrow(sim_dat$N_sp), nrow(sim_dat$N_sp), nrow(sim_dat$N_sp))  
N = N_stock[1]
g = c(rep(1, times = N_stock[1]), # Vector of group assignments.
      rep(2, times = N_stock[2]),
      rep(3, times = N_stock[3]))
#ncovars = 1 # right now just trying with temperature for stage 1      
 
data_stage_j <- sim_dat$N_j #c(as.integer(sim_dat$N_j))#, 
                 # as.integer(sim_yukon_fall_df$N_j),
                 # as.integer(sim_kusko_df$N_j))

data_stage_r <- sim_dat$N_r
 
data_stage_sp <- sim_dat$N_sp #c(as.integer(sim_dat$N_sp))#, 
                # as.integer(sim_yukon_fall_df$N_sp),
                # as.integer(sim_kusko_df$N_sp))

kappa_j_start =  c(runif(1, 0.03, 0.07),
                   runif(1, 0.03, 0.07),
                   runif(1, 0.03, 0.07))
kappa_sp_start =  c(runif(1, 0.12, 0.2),
                    runif(1, 0.12, 0.2),
                    runif(1, 0.12, 0.2))
 
basal_p_1 = c(0.05,0.05,0.05) # straight from simulation 
basal_p_2 = c(0.15,0.15,0.15) # straight from simulation 
  
cov1 = sim_dat$cov1
cov2 = sim_dat$cov2

data_list <- list(Ps = Ps,
                  fs=fs,
                  N = N, 
                  K = K,  
                  data_stage_j = data_stage_j, 
                  data_stage_sp = data_stage_sp,
                  kappa_sp_start = kappa_sp_start,
                  data_stage_r =data_stage_r,
                  kappa_j_start = kappa_j_start,
                  basal_p_1=basal_p_1,basal_p_2=basal_p_2,
                  cov1 = cov1,
                  cov2 = cov2) 

bh_fit <- stan(
  file = here::here("scripts", "stan_mod_BH_V1.stan"),
  data = data_list,
  chains = n_chains,
  warmup = warmups,
  iter = total_iterations,
  cores = n_cores)
 
# rlnorm(log(data_stage_sp[,3]), 10);
# rlnorm( data_stage_sp[,3], 10);
# rnorm( data_stage_sp[,3], 10);

bh_summary <- summary(bh_fit)$summary %>% 
  as.data.frame() %>% 
  mutate(variable = rownames(.)) %>% 
  select(variable, everything()) %>% 
  as_data_frame()
 
bh_summary %>% 
  slice(1:18) %>%
  ggplot() + 
  geom_linerange(aes(variable, ymin = `2.5%`,ymax = `97.5%`)) + 
  geom_crossbar(aes(variable, mean, ymin = `25%`, ymax = `75%`), fill= 'grey') + 
  facet_wrap(~variable, scales = 'free') +
  geom_point(data = obs_df, aes(variable, mean), color = "red" ) #observed
 
 mcmc_trace(bh_fit)#, pars = c("c_1"))
 mcmc_trace(bh_fit, pars = c("c_2[1]","c_2[2]","c_2[3]"))
 mcmc_trace(bh_fit, pars = c("theta1"))
 mcmc_trace(bh_fit, pars = c("theta2"))

 mcmc_trace(bh_fit, pars = c("sigma_y_j"))
 mcmc_trace(bh_fit, pars = c("sigma_y_sp"))

 
#hist(rnorm(1000, 750000, 1e5))
 
#  # save plots ==============
# # 
# # pdf("output/trace.pdf")
# # MCMCtrace(bh_fit, params = c("log_c_1"), pdf = FALSE)
# # MCMCtrace(bh_fit, params = c("log_c_2"), pdf = FALSE)
# # MCMCtrace(bh_fit, params = c("p_1"), pdf = FALSE)
# # MCMCtrace(bh_fit, params = c("p_2"), pdf = FALSE)
# # dev.off()
# # 
# # MCMCtrace(bh_fit, params = c("log_c_1"), pdf = FALSE)
# # MCMCtrace(bh_fit, params = c("log_c_2"), pdf = FALSE)
# # MCMCtrace(bh_fit, params = c("p_1"), pdf = FALSE)
# # MCMCtrace(bh_fit, params = c("p_2"), pdf = FALSE)
# # 
# # MCMCtrace(bh_fit, params = c("sigma_y_j"), pdf = FALSE)
# # MCMCtrace(bh_fit, params = c("sigma_y_sp"), pdf = FALSE)
# #   
# # # refresh = 250,
# #   #init = init_list,
# #   # control = list(max_treedepth = max_treedepth,
# #   #                adapt_delta = adapt_delta))
# # 
#  
# 
# 
# 
# 
#  