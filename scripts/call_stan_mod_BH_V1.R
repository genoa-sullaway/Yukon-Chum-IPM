library(rstan)
library(tidyverse)
library(here)

# load simulated data =======================================================
# to start, use simulated data from "scripts/simulate_data.R" 
sim_dat <- read_csv("data/Simulated_DatBH.csv")
# sim_yukon_fall_df <- read_csv("data/Simulated_Yukon_Fall.csv")
# sim_kusko_df <- read_csv("data/Simulated_Kusko.csv")

# load Covariates  ==========================================================
#covar_temp <- data.frame(temp = c(readRDS("output/covar_temp_sim.RDS")))
#covar_1 <- rep(covar_temp$temp, times = 3)

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
K = 1 # number of stocks involved
N_stock <- c(nrow(sim_dat))
#N_stock <- c(nrow(sim_yukon_spring_df), nrow(sim_yukon_fall_df), nrow(sim_kusko_df))  
N = sum(N_stock)
g = c(rep(1, times = N_stock[1]))#, # Vector of group assignments.
      # rep(2, times = N_stock[2]),
      # rep(3, times = N_stock[3]))
#ncovars = 1 # right now just trying with temperature for stage 1      
 
data_stage_j <- c(as.integer(sim_dat$N_j))#, 
                 # as.integer(sim_yukon_fall_df$N_j),
                 # as.integer(sim_kusko_df$N_j))
 
data_stage_sp <- c(as.integer(sim_dat$N_sp))#, 
                # as.integer(sim_yukon_fall_df$N_sp),
                # as.integer(sim_kusko_df$N_sp))

kappa_j_start =  runif(1, 0.05, 0.155) # starting values for kappa so there arent NAs, this doesnt really do anything. 
kappa_sp_start =  runif(1, 0.145, 0.155)
 
 
data_list <- list(Ps = Ps,
                  fs=fs,
                  # n_init_years=n_init_years,
                  N = N, 
                  K = K, 
                  # g = g,
                  # N_stock = N_stock, 
                  data_stage_j = data_stage_j, 
                  data_stage_sp = data_stage_sp,
                  kappa_sp_start = kappa_sp_start,
                  kappa_j_start = kappa_j_start) 
                  # ncovars = ncovars,
                  # covar_1 = covar_1)
  

# init_list <- list(
#   list(p_1= 0.05 , 
#        p_2 =0.10,
#        c_1 = 1500000,
#        c_2 = 1800000000,
#        log_N_sp_start= 12.5,
#        log_N_j_start = 15.3,
#        log_N_egg_start= 17,
#        sigma_y_j = 0.02, 
#        sigma_y_sp = 0.03)#,
  # list(
  #   p_1= 0.04 , 
  #   p_2 =0.11,
  #   c_1 = 1600000,
  #   c_2 = 1700000000,
  #   log_N_sp_start= 13,
  #   log_N_j_start = 15.6,
  #   log_N_egg_start= 18.5,
  #   sigma_y_j = 0.015, 
  #   sigma_y_sp = 0.034),
  # list(
  #   p_1= 0.041 , 
  #   p_2 =0.13,
  #   c_1 = 1700000,
  #   c_2 = 1900000000,
  #   log_N_sp_start= 13.25,
  #   log_N_j_start = 15.4,
  #   log_N_egg_start= 18.75,
  #   sigma_y_j = 0.015, 
  #   sigma_y_sp = 0.034),
  # list(
  #   p_1= 0.022, 
  #   p_2 =0.14,
  #   c_1 = 1800000,
  #   c_2 = 1600000000,
  #   log_N_sp_start= 13.6,
  #   log_N_j_start = 15,
  #   log_N_egg_start= 17.7,
  #   sigma_y_j = 0.014, 
  #   sigma_y_sp = 0.033)
  #)

bh_fit <- stan(
  file = here::here("scripts", "stan_mod_BH_V1.stan"),
  data = data_list,
  chains = n_chains,
  warmup = warmups,
  iter = total_iterations,
  cores = n_cores)

MCMCtrace(bh_fit) #, params = c("log_c_1", "log_c_2",
                             #"p_1", "p_2","sigma_y_j","sigma_y_sp"), pdf = FALSE)


 MCMCtrace(bh_fit, params = c("log_c_1"), pdf = FALSE)
 MCMCtrace(bh_fit, params = c("log_c_2"), pdf = FALSE)
 MCMCtrace(bh_fit, params = c("p_1"), pdf = FALSE)
 MCMCtrace(bh_fit, params = c("p_2"), pdf = FALSE)
 
 MCMCtrace(bh_fit, params = c("sigma_y_j"), pdf = FALSE)
 MCMCtrace(bh_fit, params = c("sigma_y_sp"), pdf = FALSE)
 
 MCMCsummary(bh_fit,params = c("log_c_1", "log_c_2",
                               "p_1", "p_2","sigma_y_j","sigma_y_sp"))
 
 
 
 
 # save plots ==============

pdf("output/trace.pdf")
MCMCtrace(bh_fit, params = c("log_c_1"), pdf = FALSE)
MCMCtrace(bh_fit, params = c("log_c_2"), pdf = FALSE)
MCMCtrace(bh_fit, params = c("p_1"), pdf = FALSE)
MCMCtrace(bh_fit, params = c("p_2"), pdf = FALSE)
dev.off()

MCMCtrace(bh_fit, params = c("log_c_1"), pdf = FALSE)
MCMCtrace(bh_fit, params = c("log_c_2"), pdf = FALSE)
MCMCtrace(bh_fit, params = c("p_1"), pdf = FALSE)
MCMCtrace(bh_fit, params = c("p_2"), pdf = FALSE)

MCMCtrace(bh_fit, params = c("sigma_y_j"), pdf = FALSE)
MCMCtrace(bh_fit, params = c("sigma_y_sp"), pdf = FALSE)
  
# refresh = 250,
  #init = init_list,
  # control = list(max_treedepth = max_treedepth,
  #                adapt_delta = adapt_delta))

 




 