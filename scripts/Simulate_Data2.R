
library(actuaryr)
# library(rjags)
library(readxl)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(MCMCvis)
library(HDInterval)
library(reshape2)
library(tidyverse)
library(dplyr)
library(rstan)
set.seed(20)

#### Simulating data 
# Load data for baseline ============== 
#yukon_fall<- read_csv("data/Yukon_Escapement_ADFG/Yukon_Fall_Chum_RR_JTC.csv")  
yukon_spring <- read_excel("data/Yukon_Escapement_ADFG/Yukon Summer Chum Total Run 1978-2022 Run Rec.xlsx")

#kusko_estimated_parameters<- readRDS("output/optim_output_par_data2021.RDS") 
#kusko<-data.frame(Year = c(1988:(2022-1)),   
 #                 pred_N_est= as.vector(c(kusko_estimated_parameters[2:35])))  

# Init ===================
n<-105 #number of samples per population
pops<- 1 #seq(1,3,1) #population pointer vector
population<- c(rep(1,n)) #, rep(2,n), rep(3,n)) #population pointer vector
n.pop <-length(unique(population)) #number of population
year <- rep(seq(1,n), n.pop) #creating a year pointer

fs = 2440 # fecundity, from Gilk Baumer paper about Kusko (not sig different from spring fall). Check lit for yukon. 
Ps = 0.5 # proportion of females. supported by Gilk Baumer 

#Bev Holt parameters ===================
# p for alpha, and c for carrying capacity 
p_1 <- c(rnorm(n, 0.05, 0)) #simulating a unique beta for each population
      # rnorm(n, 0.00013, 0), 
       #rnorm(n, 0.000047, 0))
p_2 <-c(rnorm(n, 0.15, 0)) #simulating a unique beta for each population
# rnorm(n, 0.00013, 0), 
#rnorm(n, 0.000047, 0))
c_1 <- c(rnorm(n, 10000000, 0)) #16.11, #simulating a unique alpha for each population
       # rnorm(n, 2.5, 0),
       # rnorm(n, 1.77, 0))
c_2 <- c(rnorm(n, 1000000, 0)) #13.8 , #simulating a unique alpha for each population
# rnorm(n, 2.5, 0),
# rnorm(n, 1.77, 0))
 
#covariate data ===================
# not worrying about this yet
# cov1 <- rnorm(n*n.pop, 0, 2) #Cov 1 data
# cov2 <- rnorm(n*n.pop, 0, 2) #Cov2 data
# theta1 <- c(rep(0.1,n), rep(0.3,n), rep(0.4,n)) #relationship for simulated data
# theta2 <- c(rep(-0.2,n), rep(0.1,n), rep(-0.1,n)) #relationship for simulated data

# simulate populations  ===================

# N_sp = matrix(nrow=n, ncol=1, NA)
# N_sp[1,] <- mean.spawn # seed inital 

# spawn.sim <- c(rnorm(n, mean.spawn[1],mean(error.spawn)), #simulating a unique number of spawners for each population
#                rnorm(n, mean.spawn[2],mean(error.spawn)),
#                rnorm(n, mean.spawn[3],mean(error.spawn)))

# obs_error_j  <- rnorm(n*n.pop, 50, 5)
# obs_error_sp  <- rnorm(n*n.pop, 50, 5)

N_eggs = matrix(nrow=n,ncol=1,NA)
kappa_fw =  matrix(nrow=n,ncol=1,NA)
N_j =  matrix(nrow=n,ncol=1,NA)
N_j[1] = 10000 # just so i dont get yelled at about NAs later down the road 
kappa_sp =  matrix(nrow=n,ncol=1,NA)

N_sp = matrix(nrow=n, ncol=1, NA)
mean.spawn <- c(mean(yukon_spring$Escapement)) #c(1200 , 2000, 2500) #mean spawners for each population
N_sp[1] <-mean.spawn

#N_sp<-c(rnorm(n, mean.spawn, mean(obs_error_sp)))
process_error_j <- rnorm(n*n.pop,2 , 0.3)
process_error_sp <- rnorm(n*n.pop,2, 0.3)

for (i in 2:n) {
  N_eggs[i,] = fs*Ps*N_sp[i-1]
  kappa_fw[i,] <-  (p_1[i])/(1 + ((p_1[i]*N_eggs[i,])/c_1[i])) # + obs_error_j[[i]] # SR formula from cunnigham 2018
  N_j[i,] = (N_eggs[i,]*kappa_fw[i,]) #+ obs_error_j[i]
  
  kappa_sp[i,] <- (p_2[i])/(1 + ((p_2[i]*N_j[i,])/c_2[i]))  
  N_sp[i,] = (N_j[i,]*kappa_sp[i,])# + obs_error_sp[i]  
}

N_j_sim = rlnorm(n, log(N_j), process_error_j)
N_sp_sim = rlnorm(n, log(N_sp), process_error_sp)

hist(log(N_j_sim[6:n]))
hist(log(N_sp_sim[6:n]))
 
sd(log(N_j_sim[6:n])) #[6:n])
sd(log(N_sp_sim[6:n]))

plot(N_j) 
plot(N_sp) 

dat_sim <- cbind(Population = population, Year = year, 
                 N_eggs = N_eggs[1:n,], N_j = N_j_sim, #[1:n,] , 
                 N_sp = N_sp_sim,#[1:n,] ,
                 obs_error_sp = obs_error_sp, obs_error_j=obs_error_j,#cov1=cov1, cov2=cov2,
                 p_1 = p_1, p_2=p_1, 
                 c_1=c_1, c_2=c_2,
                 process_error_j=process_error_j,
                 process_error_sp=process_error_sp,
                 kappa_sp=kappa_sp[1:n,],
                 kappa_fw=kappa_fw[1:n,]) #setting up single data file so that you can replace it with the real data

dat_sim <- data.frame(dat_sim)[6:n,]  
dat_sim_plot <- dat_sim %>% 
  gather(c(3:13), key = "id", value = "value") 
  
plota <- ggplot(data = dat_sim_plot, aes(x=Year, y = log(value))) +
  geom_point() +
  facet_wrap(~id, scales = "free")
 
plota

plotb <- ggplot(data = dat_sim_plot, aes(x=Year, y = value)) +
  geom_point() +
  facet_wrap(~id, scales = "free")

plotb

write_csv(dat_sim , "data/Simulated_DatBH.csv")

sd(dat_sim$N_j)
sd(dat_sim$N_sp)

log(mean(dat_sim$N_j))
log(mean(dat_sim$N_sp))
log(mean(dat_sim$N_eggs))



# 
# #### Setting up structure for using the data file ####
# #creating references for number of years of data for each population
# n.years <- vector(length=n.pop)
# years <- matrix(nrow=n.pop,ncol=n)
# p <- 1
# for(p in 1:n.pop) {
#   n.years[p] <- length(unique(dat.sim$Year[dat.sim$Population==pops[p]]))
#   years[p,1:n.years[p]] <- sort(unique(dat.sim$Year[dat.sim$Population==pops[p]]))
# }#next p
# 
# 
# # Spawners and Recruits data
# spawn <- matrix(nrow=n.pop,ncol=max(n.years))
# rec <- matrix(nrow=n.pop,ncol=max(n.years))
# ln.rec <- matrix(nrow=n.pop,ncol=max(n.years))
# 
# error.spawn <-  matrix(nrow=n.pop,ncol=max(n.years))
# error.rec <- matrix(nrow=n.pop,ncol=max(n.years))
# 
# p <- 1
# for(p in 1:n.pop) {
#   y <- 1
#   for(y in 1:n.years[p]) {
#     year <- years[p,y]
#     
#     #Spawners
#     spawn[p,y] <- dat.sim$Spawners[dat.sim$Population==pops[p] & dat.sim$Year==year]
#     error.spawn[p,y] <- dat.sim$error.spawn[dat.sim$Population==pops[p] & dat.sim$Year==year]
#     #Recruits
#     rec[p,y] <- dat.sim$ObsRecruits[dat.sim$Population==pops[p] & dat.sim$Year==year]
#     ln.rec[p,y] <- log(dat.sim$ObsRecruits[dat.sim$Population==pops[p] & dat.sim$Year==year])
#     error.rec[p,y] <- dat.sim$error.rec[dat.sim$Population==pops[p] & dat.sim$Year==year]
#   }#next y
# }#next p
# 
# ##### Setting up Covars data ####
# n.covars <- 2
# covars <- array(data=NA,dim=c(n.pop, max(n.years), n.covars))
# 
# p <- 1
# for(p in 1:n.pop) {
#   y <- 1
#   for(y in 1:n.years[p]) {
#     year <- years[p,y]
#     c <- 1
#     #COVARIATE 1 ========
#     covar1 <- dat.sim$cov1[dat.sim$Population==pops[p] & dat.sim$Year==year]
#     covars[p,y,c] <- ifelse(is.na(covar1),0,covar1) # Fill in with zero if unavailable
#     c <- c+1
#     
#     #COVARIATE 2 ========
#     covar2 <- dat.sim$cov2[dat.sim$Population==pops[p] & dat.sim$Year==year]
#     covars[p,y,c] <- ifelse(is.na(covar2),0,covar2) # Fill in with zero if unavailable
#     c <- c+1
#     
#   }#next y
# }#next p
# 
# #### Running STAN Model ####
# 
# ##### Assigning data to the STAN data list ####
# ##### Setting up Covars data ####
# 
# years[is.na(years)] <- -999 # years without data are assined -999 (Stan can't use NAs) - but NAs are important for indexing covars so needs to be after
# data <- list(S = n.pop, #number of populations
#              N = n.years,# number of years
#              ncovars=n.covars, #number of covariates
#              spawn = spawn, #spawner data
#              ln_rec = ln.rec,#recruit data
#              covars = covars, # covariate data
#              error_rec = error.rec, # error for recruits
#              error_spawn = error.spawn, #error for spawenrs
#              maxN = 100,
#              n_iter=2000
# )
# 
# 
# #####Assinging Stan Conditions ####
# 
# warmups <- 1000
# total_iterations <- 3000
# max_treedepth <-  12
# n_chains <-  3
# n_cores <- 4
# adapt_delta <- 0.95
# 
# ####Fitting the Model ####
# bh_fit <- stan(
#   file = here::here("Chinook/Src/DRAFTModelSimSTAN.stan"),
#   data = data,
#   chains = n_chains,
#   warmup = warmups,
#   iter = total_iterations,
#   cores = n_cores,
#   refresh = 250,
#   control = list(max_treedepth = max_treedepth,
#                  adapt_delta = adapt_delta)
# )
# 
# MCMCsummary(bh_fit,params = c("alpha", "beta",
#                               "mu_coef",
#                               "sigma_coef", "theta"))
# 
# 
# MCMCtrace(bh_fit, params = c("alpha"), pdf = FALSE)
# MCMCtrace(bh_fit, params = c("beta"), pdf = FALSE)
# MCMCtrace(bh_fit, params = c("theta"), pdf = FALSE)
#  