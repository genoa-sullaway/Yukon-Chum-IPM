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
nRyrs = nByrs+A+1# 83 #108
t_start = 5

Ps = 0.5 # proportion of females - assumption, need to lit check
fs = as.vector(c(1800, 2000, 2200, 2440)) # fecundity - Gilk-Baumer 2009 estimate for Kusko Chum is: 2440. I added extra numbers temporarily just so that younger fish reproduce less, but will have to look up data for this more...
#fs = as.vector(c(2440, 2440, 2440, 2440)) # fecundity - Gilk-Baumer 2009 estimate for Kusko Chum is: 2440. I added extra numbers temporarily just so that younger fish reproduce less, but will have to look up data for this more...

#Bev Holt parameters ===================
# p for alpha, and c for carrying capacity 

log_c_1 = 18.4
log_c_2 = 17.5

c_1 = exp(log_c_1) # as.matrix(nrow = 1, ncol =1, exp(log_c_1)) 
c_2 = exp(log_c_2) # as.matrix(nrow = 1, ncol =1, exp(log_c_2)) 

# SURVIVAL/COVARIATE ===================
ncovars1 =2
ncovars2 =1
basal_p_1 = 0.2 #base survival
basal_p_2 = 0.4

cov1 <- matrix(nrow = nByrs, ncol = ncovars1, rnorm(nByrs, 0, 2),rnorm(nByrs, 0, 1))  #Cov 1 data
cov2 <- matrix(nrow = nByrs, ncol = ncovars2, rnorm(nByrs, 0, 2)) #,rnorm(nByrs, 0, 1))  #Cov2 data

 # only need sigma and mu coeff if they are set up hierarchically
 # sigma_coef1 <- as.matrix(nrow = 1, ncol =1, 0.1)
 #  sigma_coef2 <- as.matrix(nrow = 1, ncol =1, 0.1)
 #   
 #  mu_coef1 <- 0.1 #rnorm(0, 10)
 #  mu_coef2 <- -0.2 #rnorm(0, 10)
 #  
 #  theta1 <- rnorm(1,mu_coef1,sigma_coef1[1,1])
 #  theta2 <- rnorm(1,mu_coef2,sigma_coef2[1,1])

 theta1 <- c(0.5,-0.1) #rep(0.1,n), rep(0.3,n), rep(0.4,n)) #relationship for simulated data
 theta2 <- c(-0.5)#, -0.6) #relationship for simulated data
   
 p_1 = as.vector(NA) #matrix(nrow=nByrs,ncol=1,NA)
 p_2 = as.vector(NA) #matrix(nrow=nByrs,ncol=K,NA)
 
 # lines below need to be edited if I use more stocks!! theta and cov 1 should be multipled and summed, can get away with just multiplying here because there is only 1 stock and 1 covar
 
 cov_eff1 = matrix(NA, nrow = nByrs, ncol = ncovars1)
 cov_eff2 = matrix(NA, nrow = nByrs, ncol = ncovars2)
 
  for(t in 1:nByrs){
   for (c in 1:ncovars1) {
     cov_eff1[t,c] = theta1[c]*cov1[t,c]
   }
    for (c in 1:ncovars2) {
      cov_eff2[t,c] = theta2[c]*cov2[t,c]
    }
    p_1[t]  = 1 / 1+ exp(-basal_p_1 - (sum(cov_eff1[t,1:c]))) # covariate impacts survival, impact is measured through theta
    p_2[t]  = 1 / 1+ exp(-basal_p_2 - (sum(cov_eff2[t,1:c]))) # covariate impacts survival, impact is measured through theta
    }

# AGE STRUCTURE =========
  Dir_alpha = c(NA)
  p = c(NA) #matrix(nrow=K,ncol=A,NA)
  g = c(NA) #matrix(nrow=K,ncol=A,NA)
  D_scale = 0.3 
  
  # pi = c(0.2148158, 0.1909981, 0.3164682, 0.2777180)
prob = c(NA)

# for (r in 1:3) {
#   prob[r] = rbeta(1,1,1)
# }

prob = c(0.1548598, 0.7782258, 0.40537690)

  pi[1] = prob[1]
  pi[2] = prob[2] * (1 - pi[1])
  pi[3] = prob[3] * (1 - pi[1] - pi[2])
  pi[4] = 1 - pi[1] - pi[2] - pi[3]
  
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
process_error_j = 0.5 # matrix(nrow=K,ncol=1,rep(1, times =K))  #matrix(nrow=nByrs,ncol=1,rep(1, times =nByrs )) #rnorm(nByrs*1,1,0.2))
process_error_sp = 0.5 # matrix(nrow=K,ncol=1,rep(1, times =K)) #matrix(nrow=nRyrs,ncol=1,rep(2, times =nRyrs )) #rnorm(nByrs*1,5, 1))
process_error_r = 0.5 # matrix(nrow=K,ncol=1,rep(1, times =K))  #matrix(nrow=nRyrs,ncol=1,rep(3, times =nRyrs )) #rnorm(nByrs*1,5, 1))

# make pop model matricies and starting values ========= 
kappa_j =  vector( )
kappa_marine = vector( )

N_j =  matrix(nrow=nByrs,ncol=1,NA)
N_e_sum = matrix(nrow=nByrs,ncol=1,NA)
N_e  = matrix(NA, nrow = nRyrs,ncol=A)
N_recruit =  matrix(NA, nrow = nRyrs,ncol=A)
N_ocean = matrix(NA, nrow = nRyrs,ncol=A)
#N_returning = matrix(NA, nrow = nRyrs,ncol=A)
N_sp = matrix(NA, nrow = nRyrs,ncol=A)

## starting values ========
N_j[1,1] = exp(rnorm(1,20,2)) 
N_e_sum[1,1] = exp(rnorm(1,30,2))

for(t in 1:t_start){
  N_recruit[t,] = exp(rnorm(1,14,2))*p 
  N_ocean[t,] = exp(rnorm(1,19.5,2))*p 
  N_sp[t,] = exp(rnorm(1,18,2))*p
  N_e[t,] = exp(rnorm(1,20,2))*p
}
  
catch_q = 1.3 #exp(rnorm(1,0,0.5))
log_catch_q= log(catch_q)

# Harvest =============
# fishing mortality by age 

log_F = rnorm(nRyrs, -0.9, 0.2)
F =  exp(log_F) # 0.4 #,0.4,0.4,0.4) #rnorm(A,0,2) 
hist(log_F)
hist(F)
 
# age specific marine mortality =============== 
M_fill_stan = c(0.06, 0.06, 0.06) # will be cumulative 
M = matrix(ncol = A, nrow = nRyrs, 
           c(NA,0.06, 0.06, 0.06) , byrow = TRUE)
 
# POPULATION MODEL ============ 
     for (t in 2:nByrs){ # loop for each brood year 
        
         kappa_j[t] =  p_1[t]/(1+((p_1[t]*N_e_sum[t-1])/c_1)) # Eq 4.1  - Bev holt transition estimating survival from Egg to Juvenile (plugs into Eq 4.4) 
       
         N_j[t] = kappa_j[t]*N_e_sum[t-1] # Eq 4.4  generated estimate for the amount of fish each year and stock that survive to a juvenile stage
         
         kappa_marine[t] =  p_2[t]/ (1 + ((p_2[t]*N_j[t])/c_2)) # Eq 4.1   - Bev holt transition estimating survival from juvenile to spawner (plugs into Eq 4.4) 
         
         M[t,1] = -log(kappa_marine[t]) # fill in the age 1 survival for the next stage  
       
         for (a in 1:A) { 
           N_ocean[t+a,a] =  N_j[t]*p[a] # add age structure, p is proportion per age class
         
           N_recruit[t+a,a] = N_ocean[t+a,a]*exp(-sum(M[t,1:a])) #add age specific age mortality, kappa marine, survival in first winter gets put into the year 1 slot and then mortality is summer across larger age classes
           #should M be indexed by t or t+a???
           
           #OLD way of doing it: N_recruit[t+a,a] = (kappa_marine[t]*N_ocean[t+a,a])*exp(-M[a]) 
           
           N_sp[t+a,a] = N_recruit[t+a,a]*exp(-F[t+a])#[a]) # fishing occurs before spawning
           
           N_e[t+a,a] = fs[a]*Ps*N_sp[t+a,a] 
         }
        # sum across age classes and transition back to brood years 
         N_e_sum[t] = sum(N_e[t,1:A]) 
   } 
 
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

N_recruit_sim <-  vector() #array(data = NA, dim = c(nRyrs, K))
N_recruit_sim_s <-  vector() #array(data = NA, dim = c(nRyrs, K))

N_recruit[is.na(N_recruit)] <- 0

 # sum together for observation model
N_recruit_sim[1:nRyrs]<- N_recruit[1:nRyrs,1] + N_recruit[1:nRyrs,2] +
N_recruit[1:nRyrs,3] + N_recruit[1:nRyrs,4]

N_sp[is.na(N_sp)] <- 0
N_sp_sim[1:nRyrs]<- N_sp[1:nRyrs,1] + N_sp[1:nRyrs,2] + N_sp[1:nRyrs,3]+N_sp[1:nRyrs,4]

N_j_sim_hat  = (rnorm(nByrs, (N_j[2:nByrs]), process_error_j))
N_recruit_sim_s  = (rnorm(nRyrs, (N_recruit_sim[2:nRyrs]), process_error_r))
N_sp_sim_s  = (rnorm(nRyrs, (N_sp_sim[2:nRyrs]), process_error_sp))
    
    # incorporate Q, to connect different data sources in population model 
N_j_sim_observed=catch_q*N_j_sim_hat

    sd(log(N_j_sim_hat))
    sd(log(N_recruit_sim_s[1:nByrs]))
    sd(log(N_sp_sim_s[1:nByrs]))
    
# STAN STARTING VALUES ==========
  # same starting values used in simulation... 
  N_j_start =  as.vector(NA)
  N_e_sum_start = as.vector(NA)
  
  N_recruit_start = matrix(NA,nrow=t_start, ncol=A)
  N_egg_start = matrix(NA,nrow=t_start, ncol=A)
  N_ocean_start = matrix(NA,nrow=t_start, ncol=A)#vector() # ages # array(data = NA, dim = c(1, A))
  N_sp_start = matrix(NA,nrow=t_start, ncol=A)#vector() # array(data = NA, dim = c(1, A,K))
  
   N_j_start = exp(rnorm(1,20,2))
   N_e_sum_start = exp(rnorm(1,30,2))
 
        for(t in 1:t_start){
         N_recruit_start[t,] = exp(rnorm(1,14,2))*p
         N_ocean_start[t,] = exp(rnorm(1,19.5,2))*p
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
                    data_stage_return = N_recruit_sim_s,
                    data_stage_sp = N_sp_sim_s,
                   N_j_start =  N_j_start,
                   N_recruit_start = N_recruit_start,
                   N_e_sum_start= N_e_sum_start,
                   N_egg_start= N_egg_start,
                   N_sp_start= N_sp_start,
                  # N_returning_start= N_returning_start,
                  catch_q = log_catch_q, # not a data input just for later
                  sigma_y_j=process_error_j,
                  sigma_y_r=process_error_r,
                  sigma_y_sp=process_error_sp,
                  kappa_marine = kappa_marine,
                  kappa_j = kappa_j,
                  kappa_marine_start = basal_p_2, 
                  kappa_j_start = basal_p_1, 
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
                  F=F,
                  Dir_alpha=Dir_alpha,
                  "theta1[1]"=theta1[1],
                  "theta1[2]"=theta1[2],
                  "theta2[1]"=theta2[1],
                  "theta2[2]"=theta2[2],
                  "prob[1]"=prob[1],
                  "prob[2]"=prob[2],
                  "prob[3]"=prob[3]) 

 # STAN data ==========
 data_list_stan <- list(nByrs=nByrs,
                   nRyrs=nRyrs,
                   A=A,
                   t_start = t_start,
                   Ps=Ps,
                   fs=fs,
                   data_stage_j = N_j_sim_observed, # after translated from "simulation" to "basis index observed" using Q multiplier. 
                   data_stage_return = N_recruit_sim_s,
                   data_stage_sp = N_sp_sim_s,
                   N_j_start =  N_j_start,
                   N_recruit_start = N_recruit_start,
                   N_ocean_start = N_ocean_start,
                   N_e_sum_start = N_e_sum_start,
                   N_egg_start = N_egg_start,
                   N_sp_start = N_sp_start,
                   sigma_y_j=process_error_j,
                   sigma_y_r=process_error_r,
                   sigma_y_sp=process_error_sp,
                   kappa_marine_start = basal_p_2,
                   kappa_j_start = basal_p_1,
                   basal_p_1=basal_p_1,
                   cov1=cov1,
                   basal_p_2=basal_p_2,
                   cov2=cov2,
                   ncovars1=ncovars1,
                   ncovars2=ncovars2,
                   pi = pi,
                   M = M_fill_stan,
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



# plot kappa marine =====================
#  plot_survival <- as.data.frame(cbind(kappa_j,kappa_marine)) %>%
#   mutate(time = 1:nrow(.)) %>% 
#   gather(1:2, key = "id", value = "survival")  %>%
#   filter(!time <10)
# 
# ggplot(data = plot_survival) +
#   geom_point(aes(x=time, y =survival, group = id, color = id))+
#   facet_wrap(~id, scales = "free")

# plot n recruit and n ocean ============
# recruit_df <- as.data.frame(N_recruit) %>%
#   mutate(time = 1:nrow(.)) %>% 
#   gather(1:4, key = "age", value = "recruit_abundance")  
#   
# ocean_df <- as.data.frame(N_ocean) %>%
#   mutate(time = 1:nrow(.)) %>% 
#   gather(1:4, key = "age", value = "ocean_abundance")  
# 
# plot_df <- left_join(recruit_df, ocean_df) %>% 
#   gather(3:4, key = "id", value = "abundance") %>%
#   filter(!time <10)
# 
# ggplot(data = plot_df) +
#   geom_line(aes(x=time, y =abundance, group = id, color =id)) +
#   facet_wrap(~age,nrow=1)#,scales = "free")

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