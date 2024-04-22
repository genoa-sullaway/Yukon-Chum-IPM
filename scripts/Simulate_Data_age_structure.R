library(actuaryr)
library(readxl)
library(tidyverse)
library(dplyr)
library(rstan)
library(dirmult)
library(here)
library(Rlab)

#### Simulating data 
# Load data for baseline ============== 
warmups <- 2000
total_iterations <- 4000
max_treedepth <-  15
n_chains <- 1
n_cores <- 4
adapt_delta <- 0.95

# Init ===================
#K = 1 # number of stocks  
A = 4 # age classes 
nByrs= 80 #105 #number of samples per population
nRyrs = nByrs +A+1# 83 #108
t_start = 5
pops = 1 #seq(1,3,1) #population pointer vector
population<- c(rep(1,nByrs)) #population pointer vector
 
Ps = 0.5 # proportion of females - assumption, need to lit check
fs = as.vector(c(1800, 2000, 2200, 2440)) # fecundity - Gilk-Baumer 2009 estimate for Kusko Chum is: 2440. I added extra numbers temporarily just so that younger fish reproduce less, but will have to look up data for this more...
#fs = as.vector(c(2440, 2440, 2440, 2440)) # fecundity - Gilk-Baumer 2009 estimate for Kusko Chum is: 2440. I added extra numbers temporarily just so that younger fish reproduce less, but will have to look up data for this more...

#Bev Holt parameters ===================
# p for alpha, and c for carrying capacity 

log_c_1 = 18.4
log_c_2 = 17

c_1 = exp(log_c_1) # as.matrix(nrow = 1, ncol =1, exp(log_c_1)) 
c_2 = exp(log_c_2) # as.matrix(nrow = 1, ncol =1, exp(log_c_2)) 

# SURVIVAL/ COVARIATE ===================
ncovars1 =2
basal_p_1 = 0.2 # base survival
#basal_p_2 = 0.4

cov1 <-  matrix(nrow = nByrs, ncol =ncovars1, rnorm(nByrs, 0, 2),rnorm(nByrs, 0, 1))  #Cov 1 data
#cov2 <- matrix(nrow = nByrs, ncol =1, rnorm(nByrs*1, 0, 2))  #Cov2 data
 # 
# only need sigma and mu coeff if they are set up hierarchically
 # sigma_coef1 <- as.matrix(nrow = 1, ncol =1, 0.1)
 #  sigma_coef2 <- as.matrix(nrow = 1, ncol =1, 0.1)
 #   
 # mu_coef1 <- 0.1 #rnorm(0, 10)
 #  mu_coef2 <- -0.2 #rnorm(0, 10)
 #  
 # theta1 <- rnorm(1,mu_coef1,sigma_coef1[1,1])
 #  theta2 <- rnorm(1,mu_coef2,sigma_coef2[1,1])

 theta1 <- c(0.5,-0.1) #rep(0.1,n), rep(0.3,n), rep(0.4,n)) #relationship for simulated data
# theta2 <- c(-0.2) #relationship for simulated data
   
 p_1 = as.vector(NA) #matrix(nrow=nByrs,ncol=1,NA)
 # p_2 =  matrix(nrow=nByrs,ncol=K,NA)
 
 # lines below need to be edited if I use more stocks!! theta and cov 1 should be multipled and summed, can get away with just multiplying here because there is only 1 stock and 1 covar
 # save for when I try to run with covariates 
 cov_eff1 = matrix(NA, nrow = nByrs, ncol = ncovars1)

  for(t in 1:nByrs){
   for (c in 1:ncovars1) {
     cov_eff1[t,c] = theta1[c]*cov1[t,c]
   }
    p_1[t]  = 1 / 1+ exp(-basal_p_1 - (sum(cov_eff1[t,1:c]))) # covariate impacts survival, impact is measured through theta
 }
    p_2 = 0.4
 
  # p_2[,1]  = 1 / 1+ exp(-basal_p_2  - (theta2 *cov2[,1])) 
    
# AGE STRUCTURE =========
  Dir_alpha = c(NA)
  p = c(NA) #matrix(nrow=K,ncol=A,NA)
  g = c(NA) #matrix(nrow=K,ncol=A,NA)
  D_scale = 0.3 
  
  pi = c(0.2148158, 0.1909981, 0.3164682, 0.2777180)

  D_sum = 1/D_scale^2

  for (a in 1:A) {
      Dir_alpha[a] = D_sum * pi[a]
      g[a] = rgamma(n=1,Dir_alpha[a],1)
  }

    for (a in 1:A) {
       p[a] = g[a]/sum(g[1:A])
    }
  
# Process error  ===================
# error is fixed in model right now so fix it here. 
process_error_j = 1 # matrix(nrow=K,ncol=1,rep(1, times =K))  #matrix(nrow=nByrs,ncol=1,rep(1, times =nByrs )) #rnorm(nByrs*1,1,0.2))
process_error_sp = 1 # matrix(nrow=K,ncol=1,rep(1, times =K)) #matrix(nrow=nRyrs,ncol=1,rep(2, times =nRyrs )) #rnorm(nByrs*1,5, 1))
process_error_r = 1 # matrix(nrow=K,ncol=1,rep(1, times =K))  #matrix(nrow=nRyrs,ncol=1,rep(3, times =nRyrs )) #rnorm(nByrs*1,5, 1))

# make pop model matricies and starting values ========= 
kappa_j =  vector( )
kappa_marine = vector( )

N_j =  vector() # matrix(nrow=nByrs,ncol=K,NA)
N_e_sum =  vector() #= matrix(nrow=nByrs,ncol=K,NA)
N_e  = matrix(NA, nrow = nRyrs,ncol=A)
N_recruit =  vector() #= matrix(nrow=nRyrs,ncol=K,NA)
N_ocean = matrix(NA, nrow = nRyrs,ncol=A)
N_returning = matrix(NA, nrow = nRyrs,ncol=A)
N_sp = matrix(NA, nrow = nRyrs,ncol=A)

## starting values ========
N_j[1] = exp(rnorm(1,20,2)) #mean(juv$abund)# rnorm(K,20,10) 
N_recruit[1] = exp(rnorm(1,14,2)) #mean(harvest_escapement$Harvest)
 
for(t in 1:t_start){
  N_ocean[t,] = exp(rnorm(1,19.5,2))*p
  N_returning[t,]= exp(rnorm(1,19,2))*p
  N_sp[t,] = exp(rnorm(1,18,2))*p
  N_e[t,] = exp(rnorm(1,20,2))*p
}
  
N_e_sum[1] = exp(rnorm(1,30,2))
 
catch_q = 1.3 #exp(rnorm(1,0,0.5))
log_catch_q= log(catch_q)

# Harvest H_b ========== 
# this is the harvest, going to do a percent of the population instead of whole numbers
# simulating with dirichlet leads to age structure being the same across all ages at 0.25
  ##H_b  <-  array(data = rdirichlet(n=nRyrs, alpha=rep(10,A)), dim = c(nRyrs, K, A)) # higher value for alpha is a more tightly clustered distribution 
#H_b  <-  array(data = rdirichlet(n=nRyrs, alpha=rep(10,A)), dim = c(nRyrs, K, A)) # higher value for alpha is a more tightly clustered distribution 
  ##H_b  <-  array(data = rmultinom(n=nRyrs,size =500, prob = pi), 
#               dim = c(nRyrs, K, A))  

# age specific marine mortality =============== 
M = c(0.1, 0.2, 0.3, 0.4)
 
# POPULATION MODEL ============ 
  # for(k in 1:K){  # loop for each population
     for (t in 2:nByrs){ # loop for each brood year 
        
         kappa_j[t] =  p_1[t]/(1+((p_1[t]*N_e_sum[t-1])/c_1)) # Eq 4.1  - Bev holt transition estimating survival from Egg to Juvenile (plugs into Eq 4.4) 
       
         N_j[t] = kappa_j[t]*N_e_sum[t-1] # Eq 4.4  generated estimate for the amount of fish each year and stock that survive to a juvenile stage
         
         kappa_marine[t] =  p_2/ (1 + ((p_2*N_j[t])/c_2)) # Eq 4.1   - Bev holt transition estimating survival from juvenile to spawner (plugs into Eq 4.4) 
         
         N_recruit[t] = kappa_marine[t]*N_j[t] # Eq 4.5 generated estiamte for the amount of fish each year and stock that survive to a spawning stage
 
         for (a in 1:A) { 
           N_ocean[t+a,a] = N_recruit[t]*p[a] # fix age structure for now
           
           # age-specific FIXED marine mortality - add a new stage, N_ocean
           N_returning[t+a,a] = N_ocean[t+a,a]*exp(-M[a]) 
           # N_returning[t+a,a] = N_recruit[t]*p[a] # fix age structure for now
        
           N_sp[t+a,a] = N_returning[t+a,a] #-100 #* (1-H_b[t+A-a, k, a]) # harvest percent, 1-H_b are the ones that stay
           
           #N_sp[t+a,a,k] = N_returning[t,a,k] #-100 #* (1-H_b[t+A-a, k, a]) # harvest percent, 1-H_b are the ones that stay
           # 
          N_e[t+a,a] = fs[a]*Ps*N_sp[t+a,a] #  generated estimate for the amount of eggs produced that year for that stock.
         
           }
        # transition back to brood years - plug in ages manually
         N_e_sum[t] = sum(N_e[t,1:A]) #N_e[t+1,k,1] + N_e[t+2,k,2]+N_e[t+3,k,3]+N_e[t+4,k,4]
   } 

 
barplot(t(N_returning[5:nByrs,]))

p
# calculate Obs Run Comp  ============
o_run_comp = array(data = NA, dim = c(nRyrs,A))
o_run_comp_sp= array(data = NA, dim = c(nRyrs,A))
  for (t in 1:nRyrs) {
    for (a in 1:A) {
        if(t< nByrs+1){
    o_run_comp[t,a] = N_ocean[t,a]/sum(N_ocean[t,1:A])
    o_run_comp_sp[t,a] = N_sp[t,a]/sum(N_sp[t,1:A])
    }
  }
}
 
# fill in the weird gaps
for(t in 1:6){
  for (a in 1:A) {
  o_run_comp[t,a]  = p[[a]]
  o_run_comp[t+(nByrs-1),a] = p[[a]]
  }
}

barplot(t(o_run_comp))
colMeans(o_run_comp)
p
 
# Fix ESS ============= 
ess_age_comp = rep(300, times = nByrs)

# PROCESS MODEL ============= 
N_j[1] = mean(N_j[2:nByrs]) #mean(N_j[2:n,1:n.pop]) # just so i don't get yelled at about NA's later down the road 
N_j_sim_hat <- vector()# matrix(nrow=nByrs,ncol=K,NA)
 
N_sp_sim <- vector() #array(data = NA, dim = c(nRyrs, K ))
N_sp_sim_s <-  vector() #array(data = NA, dim = c(nRyrs, K))

N_returning_sim <-  vector() #array(data = NA, dim = c(nRyrs, K))
N_returning_sim_s <-  vector() #array(data = NA, dim = c(nRyrs, K))

N_returning[is.na(N_returning)] <- 0

 # sum together for observation model
N_returning_sim[1:nRyrs]<- N_returning[1:nRyrs,1] + N_returning[1:nRyrs,2] +
                               N_returning[1:nRyrs,3] + N_returning[1:nRyrs,4]

N_sp[is.na(N_sp)] <- 0
N_sp_sim[1:nRyrs]<- N_sp[1:nRyrs,1] + N_sp[1:nRyrs,2] + N_sp[1:nRyrs,3]+N_sp[1:nRyrs,4]
 
    N_j_sim_hat  = rlnorm(nByrs, log(N_j[2:nByrs]), process_error_j)
    N_returning_sim_s  = rlnorm(nRyrs, log(N_returning_sim[2:nRyrs]), process_error_r)
    N_sp_sim_s  = rlnorm(nRyrs, log(N_sp_sim[2:nRyrs]), process_error_sp)
    
    # incorporate Q, to connect different data sources in population model 
    N_j_sim_observed=catch_q*N_j_sim_hat
barplot(t(N_j_sim_hat)    )
# STAN STARTING VALUES ==========
  # same starting values used in simulation... 
  N_j_start =  as.vector(NA)
  N_e_sum_start = as.vector(NA)
  N_recruit_start = as.vector(NA)
  
  N_egg_start = matrix(NA,nrow=t_start, ncol=A)
  N_ocean_start = matrix(NA,nrow=t_start, ncol=A)#vector() # ages # array(data = NA, dim = c(1, A))
  N_returning_start = matrix(NA,nrow=t_start, ncol=A)#vector() # ages # array(data = NA, dim = c(1, A))
  N_sp_start = matrix(NA,nrow=t_start, ncol=A)#vector() # array(data = NA, dim = c(1, A,K))
 
  # for(k in 1:K) { 
   N_j_start = exp(rnorm(1,20,2))
   N_recruit_start = exp(rnorm(1,14,2))
   N_e_sum_start = exp(rnorm(1,30,2))
   # N_sp_start = exp(rnorm(1,18,2))*p 
 
   # for(a in 1:A){
        for(t in 1:t_start){
         N_ocean_start[t,] = exp(rnorm(1,19.5,2))*p
         N_returning_start[t,] =exp(rnorm(1,21,2))*p
         N_sp_start[t,] = exp(rnorm(1,18,2))*p 
         N_egg_start[t,] = exp(rnorm(1,40,2))*p
        }

# PLOT data =======
data_list_plot <- list(nByrs=nByrs,
                    nRyrs=nRyrs,
                    A=A,
                    t_start = t_start,
                    Ps=Ps,
                    fs=fs,
                    data_stage_j = N_j_sim_observed, # after translated from "simulation" to "basis index observed" using Q multiplier. 
                    data_stage_return = N_returning_sim_s,
                    data_stage_sp = N_sp_sim_s,
                   N_j_start =  N_j_start,
                   N_recruit_start = N_recruit_start,
                   N_e_sum_start= N_e_sum_start,
                   N_egg_start= N_egg_start,
                   N_sp_start= N_sp_start,
                   N_returning_start= N_returning_start,
                  catch_q = log_catch_q, # not a data input just for later
                  sigma_y_j=process_error_j,
                  sigma_y_r=process_error_r,
                  sigma_y_sp=process_error_sp,
                  kappa_marine_start = p_2,# matrix(p_2, nrow = 1, ncol = 1), #kappa_marine_start,
                  kappa_j_start = p_1,#matrix(p_1,nrow = 1, ncol = 1),
                  
                  log_p_1=log_p_1,
                  log_p_2=log(p_2),
                  M =M,
                  pi=pi,
                  c_1=c_1,
                  c_2=c_2,
                  log_c_1 = log_c_1,
                  log_c_2=log_c_2,
                  D_scale = D_scale,
                  o_run_comp=o_run_comp,
                  ess_age_comp=ess_age_comp,
                  g = g,
                  p=p,
                  Dir_alpha=Dir_alpha,
                  "theta1[1]"=theta1[1],
                  "theta1[2]"=theta1[2]) 

 ## STAN data ==========
 data_list_stan <- list(nByrs=nByrs,
                   nRyrs=nRyrs,
                   A=A,
                   t_start = t_start,
                   Ps=Ps,
                   fs=fs,
                   data_stage_j = N_j_sim_observed, # after translated from "simulation" to "basis index observed" using Q multiplier. 
                   data_stage_return = N_returning_sim_s,
                   data_stage_sp = N_sp_sim_s,
                   N_j_start =  N_j_start,
                   N_recruit_start = N_recruit_start,
                   N_ocean_start = N_ocean_start,
                   N_e_sum_start = N_e_sum_start,
                   N_egg_start = N_egg_start,
                   N_sp_start = N_sp_start,
                   N_returning_start= N_returning_start,
                   sigma_y_j=process_error_j,
                   sigma_y_r=process_error_r,
                   sigma_y_sp=process_error_sp,
                   kappa_marine_start = p_2,
                   kappa_j_start = basal_p_1,
                   basal_p_1=basal_p_1,
                   cov1=cov1,
                   ncovars1=ncovars1,
                   pi=pi,
                   M = M,
                   o_run_comp=o_run_comp,
                   ess_age_comp=ess_age_comp)

# call mod  ===========================
bh_fit <- stan(
  file = here::here("scripts", "stan_mod_BH_SIM.stan"), #different than data model so I can move priors around 
  data = data_list_stan,
  chains = n_chains,
  warmup = warmups,
  iter = total_iterations,
  cores = n_cores) # init=init_ll)

write_rds(bh_fit, "output/stan_fit_SIMULATED_OUTPUT.RDS")
 

# OLD ========
# ESS ======= 
# var_age <- mean(c(var(o_run_comp[1:nByrs,1]),
#                   var(o_run_comp[1:nByrs,2]),
#                   var(o_run_comp[1:nByrs,3]),
#                   var(o_run_comp[1:nByrs,4])))

# nRyrs / (1 + var_age / nRyrs)
# num = nRyrs / (1 + var_age / nRyrs)
# ess_age_comp = rep(num, times = nRyrs)

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