
library(actuaryr)
library(readxl)
library(tidyverse)
library(dplyr)
library(rstan)
library(dirmult)
library(here)
library(Rlab)

# Load data for baseline ============== 
warmups <- 2000
total_iterations <- 4000
max_treedepth <-  15
n_chains <- 1
n_cores <- 4
adapt_delta <- 0.95

# load salmon data ================================================
# data_stage_j = N_j_sim_observed, # after translated from "simulation" to "basis index observed" using Q multiplier. 
# data_stage_return = N_returning_sim_s,
# data_stage_sp = N_sp_sim_s
summer_age_comp<-read_csv("data/age_comps/processed_age_comps_summer_yukon.csv")  %>%
  filter(!cal_year < 2005)

# summer_brood <- read_csv("output/yukon_summer_broodyear.csv")%>%
#   filter(!brood_year < 2002) # for now to simplify matching with juveniles

# spawner data by age class 
spawners_age_class <- read_excel("data/Yukon_Escapement_ADFG/S Chum RR 2023.xlsx", sheet = 2) %>%
  dplyr::select(1,11:14) %>%
  janitor::row_to_names(row_number = 1) %>%
  dplyr::rename(cal_year = "Year")  %>%
  dplyr::mutate(age3=as.numeric(age3),
                age4=as.numeric(age4),
                age5=as.numeric(age5),
                age6=as.numeric(age6)) %>%
  filter(!cal_year < 2005)

### harvest below weir 
harvest <- read_excel("data/Yukon_Escapement_ADFG/S Chum RR 2023.xlsx", sheet = 2) %>%
  dplyr::select(1:2,4) %>%
  janitor::row_to_names(row_number = 1)  %>%
  dplyr::rename(cal_year = "Year") %>%
  dplyr::mutate(cal_year = as.numeric(cal_year),
                Harvest = as.numeric(Harvest),
                Escapement = as.numeric(Escapement)) %>%
  filter(!cal_year < 2005) %>% # from brood year 2002 (first year of juvenile data), the first year that fish could return is 2005 if its a 3yo, the last yera it coudl return is 2007 if its a 6yo.
  as.data.frame()   

data_stage_sp <- harvest %>%
                    dplyr::select(Escapement)

data_stage_return <- harvest %>%
                      dplyr::mutate(return=Escapement+Harvest) %>%
                      dplyr::select(return)
# juv data =========
data_stage_j<- read_csv("data/tidy_BASIS_AYK_model.csv") %>%
  dplyr::select(1,2) %>% # yukon summer is column labeled 1, yukon fall is 2, kusko is 3
  dplyr::rename(abund = `1`) %>%
  filter(!brood_year>2017) #if a fish is caught in 2023 and is 6 years old,
# then its brood  year is 2017. Need to trim so the indexing works... I think...

# Init ===================
nByrs= nrow(data_stage_j) # difference between the two needs to be 3 (t+A-a, if a=1 and A=4)
nRyrs = nrow(data_stage_sp)  
pops = 1 #seq(1,3,1) #population pointer vector
population<- c(rep(1,nByrs)) #population pointer vector
K = 1 # number of stocks  
A = 4 # age classes 
#n.pop <-length(unique(population)) #number of population
#year <- rep(seq(1,n), n.pop) #creating a year pointer

Ps = 0.5 # proportion of females - assumption, need to lit check
fs = as.matrix(c(1800, 2000, 2200, 2440)) # fecundity - Gilk-Baumer 2009 estimate for Kusko Chum is: 2440. I added extra numbers temporarily just so that younger fish reproduce less, but will have to look up data for this more...

#Bev Holt parameters ===================
# p for alpha, and c for carrying capacity 
basal_p_1 = 0.2
basal_p_2 = 0.4
# 
# log_c_1 = 18.4
# log_c_2 = 15
# 
# c_1 <- as.matrix(nrow = 1, ncol =1, exp(log_c_1))
# c_2 <- as.matrix(nrow = 1, ncol =1, exp(log_c_2))

# Covariate data ===================
# cov1 <-  matrix(nrow = nByrs, ncol =1, rnorm(nByrs*1, 0, 2))  #Cov 1 data
# cov2 <- matrix(nrow = nByrs, ncol =1, rnorm(nByrs*1, 0, 2))  #Cov2 data
# 
# sigma_coef1 <- as.matrix(nrow = 1, ncol =1, 0.1)
# sigma_coef2 <- as.matrix(nrow = 1, ncol =1, 0.1)
# 
# mu_coef1 <- 0.1 #rnorm(0, 10)
# mu_coef2 <- -0.2 #rnorm(0, 10)
# 
# theta1 <- rnorm(1,mu_coef1,sigma_coef1[1,1])
# theta2 <- rnorm(1,mu_coef2,sigma_coef2[1,1])

# theta1 <- c(0.1) #rep(0.1,n), rep(0.3,n), rep(0.4,n)) #relationship for simulated data
# theta2 <- c(-0.2) #relationship for simulated data

# simulate age strucutre =========
# currently fixing this but will likely eventually have it as a random variable. 

# //matrix<lower=0>[nRyrs, K,A] o_run;      // Observed run size by age class
# real o_run[nRyrs, K,A]; // Observed run size by age class
# real<lower=0, upper=1> o_run_comp[nRyrs, K,A]; // Observed age composition by year
# vector [nRyrs] ess_age_comp; 
# 
# pi = c(NA)
# Dir_alpha = c(NA)
# p = matrix(nrow=nRyrs,ncol=A,NA)
# g = matrix(nrow=nRyrs,ncol=A,NA)

# prob = c(rbeta(1,1,1),
#          rbeta(1,1,1),
#          rbeta(1,1,1),
#          rbeta(1,1,1))
# prob = c(0.1148158, # rbeta(1,1,1),
#          0.2157721, # rbeta(1,1,1),
#          0.5999373)  # rbeta(1,1,1))
# D_scale = 0.4411873 # 
# 
# pi[1] = prob[1] 
# pi[2] = prob[2] * (1 - pi[1])
# pi[3] = prob[3] * (1 - pi[1] - pi[2])
# pi[4] = 1 - pi[1] - pi[2] - pi[3]
# D_sum = 1/D_scale^2

# for (a in 1:A) {
#   for (t in 1:nRyrs) {
#     Dir_alpha[a] = D_sum * pi[a]
#     g[t,a] = rgamma(n=1,Dir_alpha[a],1)
#     #Rlab::rgamma(n=1,alpha = Dir_alpha[a],beta = 1)
#   }
# }
# 
# for (a in 1:A) {
#   for (t in 1:nRyrs) {
#     p[t,a] = g[t,a]/sum(g[t,1:A])
#   }
# }

# Error  ===================
#error is fixed in model right now so fix it here. 
process_error_j <- matrix(nrow=K,ncol=1,rep(5, times =K))  #matrix(nrow=nByrs,ncol=1,rep(1, times =nByrs )) #rnorm(nByrs*1,1,0.2))
process_error_sp <- matrix(nrow=K,ncol=1,rep(5, times =K)) #matrix(nrow=nRyrs,ncol=1,rep(2, times =nRyrs )) #rnorm(nByrs*1,5, 1))
process_error_r <- matrix(nrow=K,ncol=1,rep(5, times =K))  #matrix(nrow=nRyrs,ncol=1,rep(3, times =nRyrs )) #rnorm(nByrs*1,5, 1))

# make pop model matricies and starting values ========= 
# kappa_j =  matrix(nrow=nByrs,ncol=K,NA)
# kappa_marine =  matrix(nrow=nByrs,ncol=K,NA)

# N_j =  matrix(nrow=nByrs,ncol=K,NA)
# N_e_sum = matrix(nrow=nByrs,ncol=K,NA)
# N_recruit = matrix(nrow=nByrs,ncol=K,NA)
# 
# N_e = array(data = NA, dim = c(nRyrs, K,A))
# N_returning = array(data = NA, dim = c(nRyrs, K,A))
# 
# N_sp = array(data = NA, dim = c(nRyrs, K,A))
# starting values =========
# mean.spawn <- exp(rep(c(rnorm(1,14,2), rnorm(1,14,2), rnorm(1,14,2), rnorm(1,14,2)), times=A))
# N_sp[1:A,1,] <- mean.spawn
# 
# N_e[1,1,] = exp(rnorm(A,20,2))
# N_j[1,1] = exp(rnorm(1,20,2)) #mean(juv$abund)# rnorm(K,20,10) 
# N_recruit[1,1] = exp(rnorm(1,14,2)) #mean(harvest_escapement$Harvest)
# N_returning[1:A,1,]  = rep(exp(rnorm(1,15,2))*pi, times =A)
# N_e_sum[1,1] = exp(rnorm(1,35,2))


# simulate H_b ========== 
# this is the harvest, going to do a percent of the population instead of whole numbers, struggling with this part a little bit 
#H_b  <-  array(data = rdirichlet(n=nRyrs, alpha=rep(10,A)), dim = c(nRyrs, K, A)) # higher value for alpha is a more tighlty clustered distribution 

o_run <- array(data = as.matrix(spawners_age_class[,2:5]), dim = c(nRyrs, K,A))
#o_run <- as.matrix(yukon_summer[,2:5]) # observed run size by age class
o_run_comp_mat <- as.matrix(spawners_age_class[,2:5]) # proportional age comp by year

# currently this is spawner age comps -- need to get harvest age comps.... 
harvest_mat  <-  matrix(harvest$Harvest,
                        nrow = length(harvest$Harvest),
                        ncol = K)  
 
H_b =  matrix(NA,nrow = nRyrs,ncol = A)
 for (t in 1:nRyrs) {
    H_b[t,] =  harvest_mat[t,1]*o_run_comp_mat[t,]
 }

# Calculate ESS ============= 
var_age <- mean( c(var(o_run_comp_mat[1:nByrs,1]),
                   var(o_run_comp_mat[1:nByrs,2]),
                   var(o_run_comp_mat[1:nByrs,3]),
                   var(o_run_comp_mat[1:nByrs,4])))

num = nRyrs / (1 + var_age / nRyrs)
ess_age_comp = rep(num, times = nRyrs)
  

# population starting values supplied as data ==========
# same starting values used in simulation... may need to adjust based on data
log_N_j_start =  matrix(nrow=1,ncol=K,NA)
log_N_e_sum_start = matrix(nrow=1,ncol=K,NA)
log_N_recruit_start = matrix(nrow=1,ncol=K,NA)

log_N_egg_start = array(data = NA, dim = c(A, K,A))
log_N_returning_start = array(data = NA, dim = c(A, K,A))
log_N_sp_start = array(data = NA, dim = c(A, K,A))

for(k in 1:K) { 
  log_N_j_start[k,1] = rnorm(1,20,2)
  log_N_recruit_start[k,1] = rnorm(1,14,2)
  log_N_e_sum_start[k,1] = rnorm(1,35,2)
  for(t in 1:4){
    for(a in 1:A){ 
      log_N_egg_start[t,k,a] = rnorm(1,30,2)
      log_N_sp_start[t,k,a] = rnorm(1,14,2)
      log_N_returning_start[t,k,a] = rnorm(1,14,2)
    } 
  }
}

## assign data list ==========
data_list <- list(nByrs=nByrs,
                  nRyrs=nRyrs,
                  A=A,
                  K=K, 
                  Ps=Ps,
                  fs=fs,
                  
                  data_stage_j = data_stage_j, # after translated from "simulation" to "basis index observed" using Q multiplier. 
                  data_stage_return = data_stage_return,
                  data_stage_sp = data_stage_sp,
                  
                  H_b=H_b,
                  o_run_comp=o_run_comp_mat,
                  ess_age_comp=ess_age_comp,
                  
                  log_N_j_start = log_N_j_start,
                  log_N_recruit_start = log_N_recruit_start,
                  log_N_e_sum_start=log_N_e_sum_start,
                  log_N_egg_start=log_N_egg_start,
                  log_N_sp_start=log_N_sp_start,
                  log_N_returning_start=log_N_returning_start,
                  
                  sigma_y_j=process_error_j,
                  sigma_y_r=process_error_r,
                  sigma_y_sp=process_error_sp,
                  kappa_marine_start = matrix(basal_p_2, nrow = 1, ncol = 1), #kappa_marine_start,
                  kappa_j_start = matrix(basal_p_1,nrow = 1, ncol = 1),
                  
                  # cov1 = cov1,
                  # cov2 = cov2,
                  # ncovars1 = 1,
                  # ncovars2 = 1, 
                  # p_1=p_1,
                  # p_2=p_2,
                  # log_p_1=log_p_1,
                  # log_p_2=log_p_2,
                  # sigma_coef1 = sigma_coef1,
                  # sigma_coef2=sigma_coef2,
                  # basal_p_1=basal_p_1,
                  # basal_p_2=basal_p_2,
           
                  # prob=prob,
                  # c_1=c_1,
                  # c_2=c_2,
                  # log_c_1 = log_c_1,
                  # log_c_2=log_c_2,
                  # D_scale = D_scale,
                  # 
                  # g = g,
                  # p=p,
                  # Dir_alpha=Dir_alpha
                  ) 

# create initial values ===============
# 
# init_fn <- function(chain_id=1) {
#   list(
#     "log_c_1" = as.matrix(nrow = 1, ncol =1,rnorm(1,18.4, 1)), 
#     "log_c_2" = as.matrix(nrow = 1, ncol =1,rnorm(1,15, 1)),  
#     "log_p_1" = as.matrix(nrow = 1, ncol =1,rnorm(1,-1.6, 0.5)), 
#     "log_p_2" = as.matrix(nrow = 1, ncol =1,rnorm(1,-0.9, 0.5)),  
#     "log_catch_q" = as.matrix(nrow = 1, ncol =1,rnorm(n=K, 0, 0.05)), 
#     "D_scale"= rbeta(1,1,1))
#     # "theta1" = as.matrix(nrow = 1, ncol =1,rnorm(n=K, 0.1, 5)),
#     # "theta2" = as.matrix(nrow = 1, ncol =1,rnorm(n=K, -0.2,10)),
#     # "g"= matrix(data=rep( c(rnorm(1,40,1), rnorm(1,80,1)) , nRyrs), 
#     #              nrow=nByrs, ncol=A, byrow=TRUE)
# }
# 
# # Initial List of Lists for Multiple Chains
# init_ll <- lapply(1:n_chains, function(id) init_fn(chain_id = id))
# call mod  ===========================
bh_fit <- stan(
  file = here::here("scripts", "stan_mod_BH_DATA.stan"),
  data = data_list,
  chains = n_chains,
  warmup = warmups,
  iter = total_iterations,
  cores = n_cores) #init=init_ll)

write_rds(bh_fit, "output/stan_fit_SIMULATED_OUTPUT_statespace.RDS")


