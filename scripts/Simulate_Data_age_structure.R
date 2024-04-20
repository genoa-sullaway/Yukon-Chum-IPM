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

# load salmon data ================================================
# summer_age_comp<-read_csv("data/age_comps/processed_age_comps_summer_yukon.csv")  %>% 
#   filter(!cal_year < 2005 )
# 
# summer_brood <- read_csv("output/yukon_summer_broodyear.csv")%>%
#   filter(!brood_year < 2002) # for now to simplify matching with juveniles
# 
# yukon_summer <- read_excel("data/Yukon_Escapement_ADFG/S Chum RR 2023.xlsx", sheet = 2) %>%
#   dplyr::select(1,11:14) %>% 
#   janitor::row_to_names(row_number = 1) %>%
#   dplyr::rename(cal_year = "Year")  %>%
#   dplyr::mutate(age3=as.numeric(age3),
#                 age4=as.numeric(age4),
#                 age5=as.numeric(age5),
#                 age6=as.numeric(age6)) %>% 
#   filter(!cal_year < 2005)
# 
# ## harvest below weir 
# harvest_escapement <- read_excel("data/Yukon_Escapement_ADFG/S Chum RR 2023.xlsx", sheet = 2) %>%
#   dplyr::select(1:2,4) %>%  
#   janitor::row_to_names(row_number = 1)  %>% 
#   dplyr::rename(cal_year = "Year") %>% 
#   dplyr::mutate(cal_year = as.numeric(cal_year), 
#                 Harvest = as.numeric(Harvest), 
#                 Escapement = as.numeric(Escapement)) %>% 
#   filter(!cal_year < 2005) %>% # from brood year 2002 (first year of juvenile data), the first year that fish could return is 2005 if its a 3yo, the last yera it coudl return is 2007 if its a 6yo. 
#   as.data.frame()  #%>%  
# #as.matrix()
# 
# # juv data =========
# juv<- read_csv("data/tidy_BASIS_AYK_model.csv") %>%
#   dplyr::select(1,2) %>% # yukon summer is column labeled 1, yukon fall is 2, kusko is 3
#   dplyr::rename(abund = `1`) %>%
#   filter(!brood_year>2017) #if a fish is caught in 2023 and is 6 years old, 
# # then its brood  year is 2017. Need to trim so the indexing works... I think... 


# Init ===================
#K = 1 # number of stocks  
A = 4 # age classes 
nByrs= 80 #105 #number of samples per population
nRyrs = nByrs +A+1# 83 #108
t_start = 5
pops = 1 #seq(1,3,1) #population pointer vector
population<- c(rep(1,nByrs)) #population pointer vector
 
#n.pop <-length(unique(population)) #number of population
#year <- rep(seq(1,n), n.pop) #creating a year pointer
 
Ps = 0.5 # proportion of females - assumption, need to lit check
#fs = as.vector(c(1800, 2000, 2200, 2440)) # fecundity - Gilk-Baumer 2009 estimate for Kusko Chum is: 2440. I added extra numbers temporarily just so that younger fish reproduce less, but will have to look up data for this more...
fs = as.vector(c(2440, 2440, 2440, 2440)) # fecundity - Gilk-Baumer 2009 estimate for Kusko Chum is: 2440. I added extra numbers temporarily just so that younger fish reproduce less, but will have to look up data for this more...

#Bev Holt parameters ===================
# p for alpha, and c for carrying capacity 
basal_p_1 = 0.2
basal_p_2 = 0.4

log_c_1 = 18.4
log_c_2 = 17

c_1 = exp(log_c_1) # as.matrix(nrow = 1, ncol =1, exp(log_c_1)) 
c_2 = exp(log_c_2) # as.matrix(nrow = 1, ncol =1, exp(log_c_2)) 

# Covariate data ===================
 # cov1 <-  matrix(nrow = nByrs, ncol =1, rnorm(nByrs*1, 0, 2))  #Cov 1 data
 # cov2 <- matrix(nrow = nByrs, ncol =1, rnorm(nByrs*1, 0, 2))  #Cov2 data
 # 
 #  sigma_coef1 <- as.matrix(nrow = 1, ncol =1, 0.1)
 #  sigma_coef2 <- as.matrix(nrow = 1, ncol =1, 0.1)
 #   
 #  mu_coef1 <- 0.1 #rnorm(0, 10)
 #  mu_coef2 <- -0.2 #rnorm(0, 10)
 #  
 #  theta1 <- rnorm(1,mu_coef1,sigma_coef1[1,1])
 #  theta2 <- rnorm(1,mu_coef2,sigma_coef2[1,1])

# theta1 <- c(0.1) #rep(0.1,n), rep(0.3,n), rep(0.4,n)) #relationship for simulated data
# theta2 <- c(-0.2) #relationship for simulated data
  
# Make p for simulated population model =========
  
  Dir_alpha = c(NA)
  p = c(NA)#matrix(nrow=K,ncol=A,NA)
  g = c(NA)#matrix(nrow=K,ncol=A,NA)
  
  #p = matrix(nrow=nRyrs,ncol=A,NA)
  #g = matrix(nrow=nRyrs,ncol=A,NA)
  
  # prob = c(0.1148158, # rbeta(1,1,1),
  #          0.2157721, # rbeta(1,1,1),
  #          0.5999373)  # rbeta(1,1,1))

  
  #D_scale =0.4 #0.01
  # pi[1] = prob[1]
  # pi[2] = prob[2] * (1 - pi[1])
  # pi[3] = prob[3] * (1 - pi[1] - pi[2])
  # pi[4] = 1 - pi[1] - pi[2] - pi[3]
  
# P as a parameter ========  doesnt vary through time though
  #p = c(0.1148158, 0.1909981, 0.4164682, 0.2777180)
  
  D_scale =0.3 
  
  pi = c(0.2148158, 0.1909981, 0.3164682, 0.2777180)

  D_sum = 1/D_scale^2

  for (a in 1:A) {
   # for (t in 1:nRyrs) {
      Dir_alpha[a] = D_sum * pi[a]
      g[a] = rgamma(n=1,Dir_alpha[a],1)
 #     g[t,a] = rgamma(n=1,Dir_alpha[a],1)
 #  }
  }

    for (a in 1:A) {
   #  for (t in 1:nRyrs) {
       p[a] = g[a]/sum(g[1:A])
    # }
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
N_e  = matrix(NA, nrow = A,ncol=nByrs)
N_recruit =  vector() #= matrix(nrow=nRyrs,ncol=K,NA)
N_returning = matrix(NA, nrow = A,ncol=nByrs)
N_returning_test = matrix(NA, nrow = A,ncol=nByrs)
N_sp = matrix(NA, nrow = A,ncol=nByrs)

## starting values ========
 
N_j[1] = exp(rnorm(1,20,2)) #mean(juv$abund)# rnorm(K,20,10) 
N_recruit[1] = exp(rnorm(1,14,2)) #mean(harvest_escapement$Harvest)
 
for(t in 1:t_start){
  N_e[t,] = exp(rnorm(1,20,2))*p
}

# for(t in 4:8){
#   N_e[t, ,1] = 0 #exp(rnorm(A,20,2))
# }
# 
# for(t in 1:4){
#   N_sp[t,,1]  = rep(exp(rnorm(1,14,2))*p, times =1)
# }
#  
N_e_sum[1] = exp(rnorm(1,30,2))
# 
# N_e[1,1,1]/sum(N_e[1,1:A,1])
# N_e[1,2,1]/sum(N_e[1,1:A,1])
# p
# productivity ============= 
# p_1 =  matrix(nrow=nByrs,ncol=K,NA)
# p_2 =  matrix(nrow=nByrs,ncol=K,NA)

log_p_1 = -0.5 # matrix(nrow=1,ncol=K,-0.5) #rnorm(1,-1, 0.5))
log_p_2 = 0.02 # matrix(nrow=1,ncol=K,0.02) #rnorm(1,-0.5, 0.5))

p_1 = exp(log_p_1)
p_2 = exp(log_p_2)

catch_q = 0.7 #exp(rnorm(1,0,0.5))
log_catch_q= log(catch_q)

# Use covariates - calculate productivity in bev holt transition function =====
# lines below need to be edited if I use more stocks!! theta and cov 1 should be multipled and summed, can get away with just multiplying here because there is only 1 stock and 1 covar
# save for when I try to run with covariates 

# p_1[,1]  = 1 / 1+ exp(-basal_p_1 - (theta1*cov1[,1])) # covariate impacts survival, impact is measured through theta
# p_2[,1]  = 1 / 1+ exp(-basal_p_2  - (theta2 *cov2[,1])) 

# Harvest H_b ========== 
# this is the harvest, going to do a percent of the population instead of whole numbers
# simulating with dirichlet leads to age structure being the same across all ages at 0.25
  ##H_b  <-  array(data = rdirichlet(n=nRyrs, alpha=rep(10,A)), dim = c(nRyrs, K, A)) # higher value for alpha is a more tightly clustered distribution 
#H_b  <-  array(data = rdirichlet(n=nRyrs, alpha=rep(10,A)), dim = c(nRyrs, K, A)) # higher value for alpha is a more tightly clustered distribution 
  ##H_b  <-  array(data = rmultinom(n=nRyrs,size =500, prob = pi), 
#               dim = c(nRyrs, K, A))  
 
 
# POPULATION MODEL ============ 
  # for(k in 1:K){  # loop for each population
     for (t in 2:nByrs){ # loop for each brood year 
        
         kappa_j[t] =  p_1/(1+((p_1*N_e_sum[t-1])/c_1)) # Eq 4.1  - Bev holt transition estimating survival from Egg to Juvenile (plugs into Eq 4.4) 
       
         N_j[t] = kappa_j[t]*N_e_sum[t-1] # Eq 4.4  generated estimate for the amount of fish each year and stock that survive to a juvenile stage
         
         kappa_marine[t] =  p_2/ (1 + ((p_2*N_j[t])/c_2)) # Eq 4.1   - Bev holt transition estimating survival from juvenile to spawner (plugs into Eq 4.4) 
         
         N_recruit[t] = kappa_marine[t]*N_j[t] # Eq 4.5 generated estiamte for the amount of fish each year and stock that survive to a spawning stage
 
         for (a in 1:A) { 
           N_returning[a,t] = N_recruit[t]*p[a] # fix age structure for now
          # N_sp[t,a] = N_returning[t,a] #-100 #* (1-H_b[t+A-a, k, a]) # harvest percent, 1-H_b are the ones that stay
             
           N_e[a,t] = fs[a]*Ps*N_returning[a,t] #  generated estimate for the amount of eggs produced that year for that stock.
           
           # N_sp[t+a,a,k] = N_returning[t,a,k] #-100 #* (1-H_b[t+A-a, k, a]) # harvest percent, 1-H_b are the ones that stay
           # 
           # N_e[t+a,a,k] = fs[a]*Ps*N_sp[t+a,a,k] #  generated estimate for the amount of eggs produced that year for that stock.
         
           }
        # transition back to brood years - plug in ages manually
         N_e_sum[t] = sum(N_e[1:A,t]) #N_e[t+1,k,1] + N_e[t+2,k,2]+N_e[t+3,k,3]+N_e[t+4,k,4]
   #  }
   } 

 
barplot(N_returning[,1:nByrs])
p
# calculate Obs Run Comp  ============
o_run_comp = array(data = NA, dim = c(A,nByrs))
o_run_comp_sp= array(data = NA, dim = c(A,nByrs))
for (a in 1:A) {
  for (t in 1:nByrs) {
    if(t< nByrs+1){
    o_run_comp[a,t] = N_returning[a,t]/sum(N_returning[1:A,t])
    #o_run_comp_sp[t,a] = N_sp[t,a]/sum(N_sp[t,1:A])
    }
  }
}

# fill in the weird gaps
for(t in 1:6){
  for (a in 1:A) {
  #N_recruit[t,1] = exp(rnorm(1,14,2)) #mean(harvest_escapement$Harvest)
  o_run_comp[a,t]  = p[[a]]
  o_run_comp[ a,(t+nByrs-1)] = p[[a]]
 # N_returning[t,1,]  = rep(exp(rnorm(1,15,2))*p, times =1)
  }
}


barplot(o_run_comp_sp)
barplot(o_run_comp)
# ggplot(data = o_run_comp) +
#   geom_bar(aes(x=time, y=proportion, fill = age, group = age), stat = "identity")
 
#
rowMeans(o_run_comp)
#
p

# colMeans(p)
# Fix ESS ============= 
# var_age <- mean(c(var(o_run_comp[1:nByrs,1]),
#                   var(o_run_comp[1:nByrs,2]),
#                   var(o_run_comp[1:nByrs,3]),
#                   var(o_run_comp[1:nByrs,4])))

# nRyrs / (1 + var_age / nRyrs)
# num = nRyrs / (1 + var_age / nRyrs)
# ess_age_comp = rep(num, times = nRyrs)

# try making this bigger 
ess_age_comp = rep(300, times = nByrs)

# make DFs before simulating from pop ============= 
N_j[1] = mean(N_j[2:nByrs]) #mean(N_j[2:n,1:n.pop]) # just so i don't get yelled at about NA's later down the road 
N_j_sim_hat <- vector()# matrix(nrow=nByrs,ncol=K,NA)
 
N_sp_sim <- vector() #array(data = NA, dim = c(nRyrs, K ))
N_sp_sim_s <-  vector() #array(data = NA, dim = c(nRyrs, K))

N_returning_sim <-  vector() #array(data = NA, dim = c(nRyrs, K))
N_returning_sim_s <-  vector() #array(data = NA, dim = c(nRyrs, K))

# N_return and obs age comp ============
N_returning[is.na(N_returning)] <- 0

 # sum together for observation model
N_returning_sim[1:nByrs]<- N_returning[1,1:nByrs] + N_returning[2,1:nByrs] +
                               N_returning[3,1:nByrs] + N_returning[4,1:nByrs]

N_sp[is.na(N_sp)] <- 0
N_sp_sim[1:nByrs]<- N_sp[1,1:nByrs] + N_sp[2,1:nByrs] + N_sp[3,1:nByrs]+N_sp[4,1:nByrs]
 
 
    N_j_sim_hat  = rlnorm(nByrs, log(N_j[2:nByrs]), process_error_j)
    N_returning_sim_s  = rlnorm(nByrs, log(N_returning_sim[2:nByrs]), process_error_r)
    N_sp_sim_s  = rlnorm(nByrs, log(N_sp_sim[2:nByrs]), process_error_sp)
 

hist(N_j_sim_hat)
hist(N_returning_sim_s)
hist(N_sp_sim_s)
plot(N_returning_sim_s)
# translate pop model to observations using the catch Q
N_j_sim_observed = N_j_sim_hat*catch_q
 

# STAN STARTING VALUES ==========
  # same starting values used in simulation... 
  N_j_start =  as.vector(NA)
  N_e_sum_start = as.vector(NA)
  N_recruit_start = as.vector(NA)
  
  N_egg_start = matrix(NA,nrow=A, ncol=t_start)
  N_returning_start = vector() # ages # array(data = NA, dim = c(1, A))
  N_sp_start = vector() # array(data = NA, dim = c(1, A,K))
 
  # for(k in 1:K) { 
   N_j_start = exp(rnorm(1,20,2))
   N_recruit_start = exp(rnorm(1,14,2))
   N_e_sum_start = exp(rnorm(1,30,2))
   N_sp_start = exp(rnorm(1,18,2))*p 
   N_returning_start =exp(rnorm(1,21,2))*p
   # for(a in 1:A){
        for(t in 1:t_start){
         N_egg_start[,t] = exp(rnorm(1,40,2))*p
   # } 
        }
  
  # } 

  
  ## assign data list ==========
data_list <- list(nByrs=nByrs,
                 # nRyrs=nRyrs,
                  A=A,
                #  K=K, 
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
                  catch_q = log(catch_q), # not a data input just for later
                  sigma_y_j=process_error_j,
                  sigma_y_r=process_error_r,
                  sigma_y_sp=process_error_sp,
                  kappa_marine_start = p_2,# matrix(p_2, nrow = 1, ncol = 1), #kappa_marine_start,
                  kappa_j_start = p_1,#matrix(p_1,nrow = 1, ncol = 1),
                  # cov1 = cov1,
                  # cov2 = cov2,
                  # ncovars1 = 1,
                  # ncovars2 = 1, 
                  p_1=p_1,
                  p_2=p_2,
                  log_p_1=log_p_1,
                  log_p_2=log_p_2,
                  # sigma_coef1 = sigma_coef1,
                  # sigma_coef2=sigma_coef2,
                  basal_p_1=basal_p_1,
                  basal_p_2=basal_p_2,
                  #H_b=H_b,
                  #prob=prob,
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
                  Dir_alpha=Dir_alpha) 
 p
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
  file = here::here("scripts", "stan_mod_BH_SIM.stan"), #different than data model so I can move priors around 
  data = data_list,
  chains = n_chains,
  warmup = warmups,
  iter = total_iterations,
  cores = n_cores) # init=init_ll)

write_rds(bh_fit, "output/stan_fit_SIMULATED_OUTPUT.RDS")

