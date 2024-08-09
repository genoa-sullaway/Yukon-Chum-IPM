library(actuaryr)
library(readxl)
library(tidyverse)
library(dplyr)
library(rstan)
library(dirmult)
library(here)
library(Rlab)

# this pairs with the "stan_mod_BH_SIM.stan" script. 
  
# load salmon data for starting values==============================================

year_min = 2002
year_max_cal = 2022
year_max_brood = 2021
 
## Spawners, Recruits, Harvest ==================================== 
yukon_fall_spawners <-read_csv("data/processed_data/yukon_fall_spawners.csv") %>%
  filter(cal_year >= year_min #, 
         #cal_year <= year_max_cal
  )%>%
  dplyr::select(2) %>%
  summarise(mean = log(mean(Spawners)))

yukon_fall_harvest<-read_csv("data/processed_data/yukon_fall_harvest.csv") %>%
  filter(cal_year >= year_min#, 
         #cal_year <= year_max_cal
  )%>%
  dplyr::select(2) %>% 
  summarise(mean = log(mean(harvest)))

yukon_fall_recruits<-read_csv("data/processed_data/yukon_fall_recruits.csv") %>%
  filter(cal_year >= year_min) %>%
  dplyr::select(2) %>%
  dplyr::summarise(mean = log(mean(total_run)))

## Fall Juveniles ================================================
fall_juv <- read_csv("data/processed_data/tidy_juv_fall_yukon.csv")  %>%
  filter(Year <= 2021) %>% # 1 year less than all spawners 
  dplyr::select(2) %>% 
  dplyr::summarise(mean = log(mean(fall_abundance)))

#### Simulating data 
# Load data for baseline ============== 
warmups <- 2000
total_iterations <- 4000
max_treedepth <-  12
n_chains <- 4
n_cores <- 4
adapt_delta <- 0.95

# Init =================== 
A = 4 # age classes 
nByrs= 20 
nRyrs = 21  
nRyrs_T = nRyrs + A -1
A = 4 # number of age classes, 3,4,5,6
K = 1 # number of stocks 
Ps = 0.5 # proportion of females - assumption, need to lit check
fs = as.vector(c(2000,2000,2000,2000)) #1800, 2000, 2200, 2400)) # fecundity - Gilk-Baumer 2009 estimate for Kusko Chum is: 2440. I added extra numbers temporarily just so that younger fish reproduce less, but will have to look up data for this more...
t_start = A  # to fill starting values 
 
#Bev Holt parameters ===================
# p for alpha, and c for carrying capacity 

log_c_1 = 16
log_c_2 = 18

c_1 = exp(log_c_1) # as.matrix(nrow = 1, ncol =1, exp(log_c_1)) 
c_2 = exp(log_c_2) # as.matrix(nrow = 1, ncol =1, exp(log_c_2)) 

# SURVIVAL/COVARIATE ===================
ncovars1 = 1
ncovars2 = 1

basal_p_1 = 1#0.2#-1.820463 # (0.1) #base survival 
basal_p_2 = 1# 0.2 #-0.2369558 # (0.4)

 # p_1 = 0.08 #-1.820463 # (0.1) #base survival 
 # p_2 = 0.2 #-0.2369558 # (0.4)

cov1 <- matrix(nrow = nByrs, ncol = ncovars1, rep(rnorm(nByrs, 0, 2), times = ncovars1))   
cov2 <- matrix(nrow = nByrs, ncol = ncovars2, rep(rnorm(nByrs, 0, 2), times = ncovars2))

theta1 <- c(0.5)    
theta2 <- c(-0.5)  

 p_1 = as.vector(NA) # matrix(nrow=nByrs,ncol=1,NA)
 p_2 = as.vector(NA) # matrix(nrow=nByrs,ncol=K,NA)
 
 cov_eff1 = matrix(NA, nrow = nByrs, ncol = ncovars1)
 cov_eff2 = matrix(NA, nrow = nByrs, ncol = ncovars2)
 
 for(t in 1:nByrs){
   for (c in 1:ncovars1) {
     cov_eff1[t,c] =  theta1[c]*cov1[t,c]
   }
   for (c in 1:ncovars2) {
     cov_eff2[t,c] =  theta2[c]*cov2[t,c]
   }
   p_1[t]  = 1 / (1 + exp(-basal_p_1-sum(cov_eff1[t,1:ncovars1])))
   p_2[t]  = 1 / (1 + exp(-basal_p_2-sum(cov_eff2[t,1:ncovars2])))
 } 
 # AGE STRUCTURE =========
# Dirichlet version
  Dir_alpha = c(NA)
   # p = c(NA) #
  p = matrix(nrow=nByrs,ncol=A,NA)
  # g = c(NA)
   g = matrix(nrow=nByrs,ncol=A,NA)
  D_scale = 0.5
# pi = c(0.2148158, 0.1909981, 0.3164682, 0.2777180)
prob = c(NA)
prob = c(0.1548598, 0.7782258, 0.40537690)

  pi[1] = prob[1]
  pi[2] = prob[2] * (1 - pi[1])
  pi[3] = prob[3] * (1 - pi[1] - pi[2])
  pi[4] = 1 - pi[1] - pi[2] - pi[3]


  D_sum = 1/(D_scale^2)

  for (a in 1:A) {
      Dir_alpha[a] = D_sum * pi[a]
       for(t in 1:nByrs) {
        g[t,a] = rgamma(n=1,Dir_alpha[a],1)
       }
  }

  # for (a in 1:A) {
  # g[a] = rgamma(n=1,Dir_alpha[a],5)
  # }

  # for (a in 1:A) {
  #     # p[1:nByrs, a] = g[a]/sum(g[1:A])
  #       p[a] = g[a]/sum(g[1:A])
  # }

  for (a in 1:A) {
    for(t in 1:(nByrs)) {
  p[t,a] = g[t,a]/sum(g[t,1:A])
    }
  }
 
# Process error  ===================
# error is fixed in model right now so fix it here. 
process_error_j = 1 # matrix(nrow=K,ncol=1,rep(1, times =K))  #matrix(nrow=nByrs,ncol=1,rep(1, times =nByrs )) #rnorm(nByrs*1,1,0.2))
# process_error_sp = 0.01#exp(rnorm(n = nRyrs, -3, 0.5))   # matrix(nrow=K,ncol=1,rep(1, times =K)) #matrix(nrow=nRyrs,ncol=1,rep(2, times =nRyrs )) #rnorm(nByrs*1,5, 1))
# process_error_r = 1# matrix(nrow=K,ncol=1,rep(1, times =K))  #matrix(nrow=nRyrs,ncol=1,rep(3, times =nRyrs )) #rnorm(nByrs*1,5, 1))
# process_error_c = 1

# make pop model matricies  ========= 
kappa_j =  vector( )
kappa_marine = vector( )
kappa_marine_mortality = vector( )

N_j =  matrix(nrow=nByrs,ncol=1,NA)
N_brood_year_return = matrix(nrow=nByrs,ncol=1,NA)

#N_first_winter = matrix(nrow=nRyrs_T,ncol=A,NA)
N_e_sum = matrix(nrow=nByrs,ncol=1,NA)
N_e  = matrix(NA, nrow = nRyrs_T,ncol=A)
N_recruit =  matrix(NA, nrow = nRyrs_T,ncol=A)
N_catch =  matrix(NA, nrow = nRyrs_T,ncol=A) 
N_sp = matrix(NA, nrow = nRyrs_T,ncol=A) 
 
# starting values ========
 # N_e_sum_start = as.vector(0)
 N_recruit_start = matrix(NA,nrow=t_start, ncol=A)
 N_catch_start = matrix(NA,nrow=t_start, ncol=A)
 N_egg_start = matrix(0,nrow=t_start, ncol=A)
 N_sp_start = matrix(NA,nrow=t_start, ncol=A)

 N_j_start = exp(rnorm(1,17,1)) 
 N_brood_year_return_start = exp(rnorm(1,16.5,1))

 for (a in 1:A) {
  for(t in 1:t_start){
   N_recruit_start[,a] = exp(yukon_fall_recruits$mean)*p[a] #exp(rnorm(1,yukon_fall_recruits$mean,1))*p[t,]
   # N_ocean_start[t,a] = exp(rnorm(1,13.6,1))*p[a]#*p[t,a]
   N_sp_start[,a] = exp( yukon_fall_spawners$mean )*p[a]   #exp(rnorm(1,yukon_fall_spawners$mean,1))*p[t,]
   # N_first_winter_start[t,a] = exp(rnorm(1,13.6))*p[a]#*p[t,a]
   N_catch_start[,a] = exp( yukon_fall_harvest$mean )*p[a]   #exp(rnorm(1,yukon_fall_harvest$mean,1))*p[t,]
   N_egg_start[,a] = exp(17.5)*p[a]  
  }
 }
 
 # log for model ======
 N_j_start_log = log(N_j_start)
 N_brood_year_return_start_log =  log(N_brood_year_return_start)
 
 N_recruit_start_log = log(N_recruit_start)
 N_sp_start_log = log(N_sp_start)
 N_catch_start_log = log(N_catch_start) 
 N_egg_start_log  = log(N_egg_start)
 
 # fill starting values 
  N_j[1,1] = N_j_start
# N_first_winter[1,1] = N_first_winter_start  
  N_brood_year_return[1,1] = N_brood_year_return_start
  
  N_recruit[1:t_start,] = N_recruit_start
  N_sp[1:t_start,] = N_sp_start
  N_catch[1:t_start,] = N_catch_start
  N_e[1:t_start,] = N_egg_start

  # catch Q ===========
  # translates from predicted juveniles to observed juveniles 
  # catch_q = -2  #exp(rnorm(1,0,0.5))
  
 # log_catch_q = rnorm(nByrs, -4,0.5)  
  log_catch_q = -4
# Harvest =============
  sigma_catch = 5
## fishing mortality by age 
  log_F = rnorm(nRyrs_T, 0,0.5)   
  F = exp(log_F)
  
  # log_F_dev_y = rnorm(nByrs, 0,0.5)  
  # log_F_mean = -1.1
  # F  = exp(log_F_mean +log_F_dev_y) 
 
## selectivity =====
# log_S = c(-1.1534648,  1.0728142, -0.4745670, -0.4882135)
# S = exp(log_S)
# S

# age specific marine mortality =============== 
M_fill_stan = c(0.06,0.06, 0.06, 0.06) # will be cumulative 
# M = matrix(ncol = A, nrow = 1, 
#            c(0.06,0.06, 0.06, 0.06) , byrow = TRUE)

# fix productivity ======
# p_1 =  rbeta(nByrs,0.2,1) #abs(rnorm(nByrs, 0.3,1))
# p_2 =  rbeta(nByrs,0.5,1)#abs(rnorm(nByrs, 0.5,1)  )

# POPULATION MODEL ============ 
     for (t in 1:nByrs){ # loop for each brood year 
         N_e_sum[t] = sum(N_e[t,1:A]); 
       
         kappa_j[t] =  p_1[t]/(1+((p_1[t]*N_e_sum[t])/c_1)) # Eq 4.1  - Bev holt transition estimating survival from Egg to Juvenile (plugs into Eq 4.4) 
       
         N_j[t] = kappa_j[t]*N_e_sum[t] # Eq 4.4  generated estimate for the amount of fish each year and stock that survive to a juvenile stage
        
         kappa_marine[t] =  p_2[t]/(1 + (( p_2[t]*N_j[t])/c_2)) # Eq 4.1  - Bev holt transition estimating survival from juvenile to spawner (plugs into Eq 4.4) 
         
         # kappa_marine_mortality[t] = -log(kappa_marine[t])
         
         N_brood_year_return[t] = N_j[t]*kappa_marine[t] #)*exp(-(kappa_marine_mortality[t])) #add age specific mortality, 
          
         
         for (a in 1:A) { 
           # N_first_winter[t+a+1,a] =  N_j[t]*p[t+a+1,a]; #add age structure, p is proportion per age class by BROOD YEAR 
           
           N_recruit[t+a,a] = (N_brood_year_return[t]*p[t,a])#*exp(-(sum(M_fill_stan[1:a]))) #exp(-(kappa_marine_mortality[t])) #add age specific mortality, 
           
            # N_recruit[t+a+1,a] = (N_j[t]*p[t,a])*exp(-(sum(M_fill_stan[1:a]) + kappa_marine_mortality[t])) #add age specific mortality, 
           
           # N_recruit[t+a+1,a] = N_first_winter[t+a+1,a]*exp(-(sum(M[1:a])+kappa_marine_mortality[t]));  
           
          N_catch[t+a,a] = N_recruit[t+a,a]*(1-exp(-(F[t+a]))) 
           
           #N_catch[t+a,a] = N_recruit[t+a,a]*(1-exp(-(F[t+a]*S[a])))
           
           N_sp[t+a,a] = N_recruit[t+a,a]-N_catch[t+a,a] # fishing occurs before spawning -- 
             
           N_e[t+a,a] = fs[a]*Ps*N_sp[t+a,a] 
         }
     }  

# calculate Obs Run Comp  ============
o_run_comp = array(data = NA, dim = c(nRyrs,A)) 

  for (t in 1:nRyrs) {
    for (a in 1:A) {
        # if(t< nByrs+1){
          o_run_comp[t,a] = N_recruit[t,a]/sum(N_recruit[t,1:A])
    # }
  }
}
 
# fill in the weird gaps
# for(t in 1:6){
#   for (a in 1:A) {
#   # o_run_comp[t,a]  = p[[a]]
#    o_run_comp[21,a] = p[[a]]
#   # }
# }
 
# barplots =====
barplot(t(p_1))
barplot(t(p_2))

barplot(t(kappa_j))

barplot(t(F))

barplot(t(kappa_marine))

barplot(t(o_run_comp))
barplot(t(N_j))
barplot(t(N_e_sum))
barplot(t(N_e))
barplot(t(N_brood_year_return))
barplot(t(N_recruit))

barplot(t(N_sp))
barplot(t(N_catch))
 
colMeans(o_run_comp)
p
 
#colMeans(p)
 
# Fix ESS ============= 
ess_age_comp = 400 #rep(300, times = nByrs)

# PROCESS MODEL ============= 
N_j_sim_hat <- vector()# matrix(nrow=nByrs,ncol=K,NA)
 
N_sp_sim <- vector() #array(data = NA, dim = c(nRyrs, K ))
N_sp_sim_s <-  vector() #array(data = NA, dim = c(nRyrs, K))

N_catch_sim <- vector()
N_catch_sim_s <- vector()

N_brood_year_return_sim <-  vector() #array(data = NA, dim = c(nRyrs, K))
 
N_recruit_sim <-  vector() #array(data = NA, dim = c(nRyrs, K))
N_recruit_sim_s <-  vector() #array(data = NA, dim = c(nRyrs, K))

N_recruit[is.na(N_recruit)] <- 0

 # sum together for observation model
N_recruit_sim[1:nRyrs_T]<- N_recruit[1:nRyrs_T,1] + N_recruit[1:nRyrs_T,2] +
N_recruit[1:nRyrs_T,3] + N_recruit[1:nRyrs_T,4]

N_catch[is.na(N_catch)] <- 0
N_catch_sim[1:nRyrs_T]<- N_catch[1:nRyrs_T,1] + N_catch[1:nRyrs_T,2] + N_catch[1:nRyrs_T,3]+N_catch[1:nRyrs_T,4]

N_sp[is.na(N_sp)] <- 0
N_sp_sim[1:nRyrs_T]<- N_sp[1:nRyrs_T,1] + N_sp[1:nRyrs_T,2] + N_sp[1:nRyrs_T,3]+N_sp[1:nRyrs_T,4]

# do this so i remove the burn in period until the model stabilizes
nByrs_stan = nByrs#-8
nRyrs_stan = nRyrs#-8
nRyrs_T_stan = nRyrs_T#-8 

N_j_sim_observed= N_j*(exp(log_catch_q))

N_j_sim_hat  =  rlnorm(nByrs_stan,  log(N_j_sim_observed), sqrt(log((0.01^2) + 1)))
N_brood_year_return_sim <- rlnorm(nByrs_stan, log(N_brood_year_return), sqrt(log((0.01^2) + 1)))  
N_catch_sim_s = rlnorm(nRyrs_stan, log(N_catch_sim ), sigma_catch)  
N_sp_sim_s  = rlnorm(nRyrs_stan, log(N_sp_sim ), sqrt(log((0.01^2) + 1)))
       
    # STAN STARTING VALUES ==========
    kappa_j_start =  basal_p_1 # runif(1, 0.2, 0.2)
    kappa_marine_start = basal_p_2 #runif(1, 0.4, 0.4)

     # PLOT all simulated data together  ==========
        dat <- data.frame(#data_stage_return = N_brood_year_return_sim,#[6:(nRyrs-1)],
                          data_stage_sp = N_sp_sim_s,#[6:(nRyrs-1)],
                          data_stage_harvest = N_catch_sim_s) %>% #[6:(nRyrs-1)])%>%
          mutate(time = 1:nrow(.)) %>% 
          gather(1:2, key = "id", value = "value")
        
    ggplot(data = dat, aes(x=time , y = value)) +
      geom_line(aes(group = id, color = id))+
      facet_wrap(~id, scales = "free")
    
    dat <- data.frame(#data_stage_return = N_brood_year_return_sim,#[6:(nRyrs-1)],
      data_stage_return = N_brood_year_return_sim,#[6:(nRyrs-1)],
      data_stage_j = N_j_sim_hat) %>% #[6:(nRyrs-1)])%>%
      mutate(time = 1:nrow(.)) %>% 
      gather(1:2, key = "id", value = "value")  
    
    ggplot(data = dat, aes(x=time , y = value)) +
      geom_line(aes(group = id, color = id)) +
      facet_wrap(~id,scales = "free")
     
        # STAN data ==========
   data_list_stan <- list(nByrs=nByrs_stan,
                          nRyrs=nRyrs_stan,
                          nRyrs_T = nRyrs_T_stan, 
                          A=A,
                          t_start = t_start, 
                          
                          # N_j_start_log =N_j_start_log,
                          # N_brood_year_return_start_log =  N_brood_year_return_start_log,
                          # N_recruit_start_log = N_recruit_start_log,
                          # N_sp_start_log =N_sp_start_log,
                          # N_catch_start_log = N_catch_start_log,
                          # N_egg_start_log=N_egg_start_log,
                          
                         sigma_y_j=process_error_j, 
                          
                          Ps=Ps,
                          fs=fs,
                          M = M_fill_stan, 
                          
                          # basal_p_1 = basal_p_1,
                          # basal_p_2=basal_p_2,
                         
                          # log_c_1 = log_c_1,
                          # log_c_2=log_c_2,
                    
                          data_stage_j = N_j_sim_hat,#[6:nByrs+1],
                          data_stage_return = N_brood_year_return_sim, 
                          # data_stage_return = N_recruit_sim_s,#[6:nRyrs+2],
                          data_stage_sp = N_sp_sim_s,#[6:nRyrs+2],
                          data_stage_harvest = N_catch_sim_s,#[6:nRyrs+2], 
                          
                          kappa_marine_mort_start = c(-log(basal_p_2), -log(basal_p_2)),                
                          kappa_marine_start = c(basal_p_2, basal_p_2), 
                          
                           p_1 = p_1,
                           p_2=p_2,
                          
                          g=g,
                          kappa_j_start = basal_p_1,
               
                          # basal_p_1 = basal_p_1,
                          # basal_p_2 = basal_p_2,
                          
                          ncovars1=ncovars1,
                          ncovars2=ncovars2,
                          # log_S = log_S,
                          # F = F,
                          # data_sp_cv = process_error_sp,
                          cov1 = cov1, #matrix(nrow = nByrs_stan, ncol = ncovars1, rep(rnorm(nByrs_stan, 0, 1), times = ncovars1)),   
                          cov2 = cov2, #matrix(nrow = nByrs_stan, ncol = ncovars2, rep(rnorm(nByrs_stan, 0, 1), times = ncovars2)),
                          # theta1=theta1,
                          # theta2=theta2,
                          o_run_comp=o_run_comp,#[8:nByrs,],
                          ess_age_comp=ess_age_comp)#[8:(nByrs-1)] )
        
    
# call mod  ===========================
bh_fit <- stan(
  file = here::here("scripts", "stan_mod_BH_SIM.stan"), # different than data model so I can move priors around 
  data = data_list_stan,
  chains = 4, #n_chains,
  warmup = warmups,
  iter = total_iterations,
  cores = n_cores,
  control = list(adapt_delta = 0.99))  
        
write_rds(bh_fit, "output/stan_fit_SIMULATED_OUTPUT.RDS")

 
 