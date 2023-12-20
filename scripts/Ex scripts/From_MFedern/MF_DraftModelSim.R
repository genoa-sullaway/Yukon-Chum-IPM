
library(actuaryr)
# library(rjags)
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

#### Simulating data ####
n<-100 #number of samples per population
pops<- seq(1,3,1) #population pointer vector
population<- c(rep(1,n), rep(2,n), rep(3,n)) #population pointer vector
n.pop <-length(unique(population)) #number of population
year <- rep(seq(1,n), n.pop) #creating a year pointer

#Ricker parameters
b <- c(rnorm(n, 0.00012, 0), #simulating a unique beta for each population
       rnorm(n, 0.00013, 0), 
       rnorm(n, 0.000047, 0))
a <- c(rnorm(n, 2.1, 0), #simulating a unique alpha for each population
       rnorm(n, 2.5, 0),
       rnorm(n, 1.77, 0))

#covariate data
cov1 <- rnorm(n*n.pop, 0, 2) #Cov 1 data
cov2 <- rnorm(n*n.pop, 0, 2) #Cov2 data
theta1 <- c(rep(0.1,n), rep(0.3,n), rep(0.4,n)) #relationship for simulated data
theta2 <- c(rep(-0.2,n), rep(0.1,n), rep(-0.1,n)) #relationship for simulated data

##### simulating the data ####
error.spawn<- rnorm(n*n.pop, 50, 5)
error.rec <- rnorm(n*n.pop, 1, 0.2)

mean.spawn <- c(1200, 2000, 2500) #mean spawners for each population
spawn.sim <- c(rnorm(n, mean.spawn[1],mean(error.spawn)), #simulating a unique number of spawners for each population
               rnorm(n, mean.spawn[2],mean(error.spawn)),
               rnorm(n, mean.spawn[3],mean(error.spawn)))

mu.rec.sim <- spawn.sim * exp(a - spawn.sim * b + theta1*cov1 + theta2*cov2) #calculating mean recruits with covariates from spawner, a, and b

rec.sim <- rlnorm(length(spawn.sim), log(mu.rec.sim),mean(error.rec)) #simulating recruits from log normal distribution

sd(rec.sim) # 49158.38 sd of recruits with error propagated across pops
sd(spawn.sim) # 540
plot(spawn.sim,rec.sim) #plotting spawener recruit relationship for the simulated data
plot(spawn.sim) #plotting spawener recruit relationship for the simulated data
plot(rec.sim)


dat.sim <- cbind(Population = population, Year = year, ObsRecruits = rec.sim,
                 Spawners = spawn.sim, error.spawn = error.spawn, 
                 error.rec=error.rec,cov1=cov1, cov2=cov2,
                 a = a, b=b, theta1=theta1, theta2=theta2) #setting up single data file so that you can replace it with the real data
dat.sim <- data.frame(dat.sim)

#### Setting up structure for using the data file ####
#creating references for number of years of data for each population
n.years <- vector(length=n.pop)
years <- matrix(nrow=n.pop,ncol=n)
p <- 1
for(p in 1:n.pop) {
  n.years[p] <- length(unique(dat.sim$Year[dat.sim$Population==pops[p]]))
  years[p,1:n.years[p]] <- sort(unique(dat.sim$Year[dat.sim$Population==pops[p]]))
}#next p


# Spawners and Recruits data
spawn <- matrix(nrow=n.pop,ncol=max(n.years))
rec <- matrix(nrow=n.pop,ncol=max(n.years))
ln.rec <- matrix(nrow=n.pop,ncol=max(n.years))

error.spawn <-  matrix(nrow=n.pop,ncol=max(n.years))
error.rec <- matrix(nrow=n.pop,ncol=max(n.years))

p <- 1
for(p in 1:n.pop) {
  y <- 1
  for(y in 1:n.years[p]) {
    year <- years[p,y]
    
    #Spawners
    spawn[p,y] <- dat.sim$Spawners[dat.sim$Population==pops[p] & dat.sim$Year==year]
    error.spawn[p,y] <- dat.sim$error.spawn[dat.sim$Population==pops[p] & dat.sim$Year==year]
    #Recruits
    rec[p,y] <- dat.sim$ObsRecruits[dat.sim$Population==pops[p] & dat.sim$Year==year]
    ln.rec[p,y] <- log(dat.sim$ObsRecruits[dat.sim$Population==pops[p] & dat.sim$Year==year])
    error.rec[p,y] <- dat.sim$error.rec[dat.sim$Population==pops[p] & dat.sim$Year==year]
    }#next y
}#next p

##### Setting up Covars data ####
n.covars <- 2
covars <- array(data=NA,dim=c(n.pop, max(n.years), n.covars))

p <- 1
for(p in 1:n.pop) {
  y <- 1
  for(y in 1:n.years[p]) {
    year <- years[p,y]
    c <- 1
    #COVARIATE 1 ========
    covar1 <- dat.sim$cov1[dat.sim$Population==pops[p] & dat.sim$Year==year]
    covars[p,y,c] <- ifelse(is.na(covar1),0,covar1) # Fill in with zero if unavailable
    c <- c+1
    
    #COVARIATE 2 ========
    covar2 <- dat.sim$cov2[dat.sim$Population==pops[p] & dat.sim$Year==year]
    covars[p,y,c] <- ifelse(is.na(covar2),0,covar2) # Fill in with zero if unavailable
    c <- c+1

  }#next y
}#next p

#### Running STAN Model ####

##### Assigning data to the STAN data list ####
##### Setting up Covars data ####

years[is.na(years)] <- -999 # years without data are assined -999 (Stan can't use NAs) - but NAs are important for indexing covars so needs to be after
data <- list(S = n.pop, #number of populations
             N = n.years,# number of years
             ncovars=n.covars, #number of covariates
             spawn = spawn, #spawner data
             ln_rec = ln.rec,#recruit data
             covars = covars, # covariate data
             error_rec = error.rec, # error for recruits
             error_spawn = error.spawn, #error for spawenrs
             maxN = 100,
             n_iter=2000
)


#####Assinging Stan Conditions ####

warmups <- 1000
total_iterations <- 3000
max_treedepth <-  12
n_chains <-  3
n_cores <- 4
adapt_delta <- 0.95

####Fitting the Model ####
bh_fit <- stan(
  file = here::here("Chinook/Src/DRAFTModelSimSTAN.stan"),
  data = data,
  chains = n_chains,
  warmup = warmups,
  iter = total_iterations,
  cores = n_cores,
  refresh = 250,
  control = list(max_treedepth = max_treedepth,
                 adapt_delta = adapt_delta)
)

MCMCsummary(bh_fit,params = c("alpha", "beta",
                                "mu_coef",
                                 "sigma_coef", "theta"))


MCMCtrace(bh_fit, params = c("alpha"), pdf = FALSE)
MCMCtrace(bh_fit, params = c("beta"), pdf = FALSE)
MCMCtrace(bh_fit, params = c("theta"), pdf = FALSE)
# 
# #### Setting up JAGS conditions ####
# 
# parameters.to.save <- c('alpha',
#                         'exp.alpha',
#                         'beta',
#                         'mu.coef',
#                         'sigma.coef',
#                         'coef',
#                         'sigma.oe',
#                         'cov.eff',
#                         'pred.rec',
#                         'dist.coef')
# data = list(
#   n.pop = n.pop, #number of populations
#   n.years = n.years,# number of years
#   n.covars=n.covars, #number of covariates
#   spawn = spawn, #spawner data
#   ln.rec = ln.rec, #recruit data
#   covars = covars, #covariate data
#   error.spawn = error.spawn, #spawner error estimates
#   error.rec = error.rec # recruits error estimates
# )
# 
# n.adapt = 3000
# n.update = 50000
# n.iter = 50000
# 
# #### running the JAGS model ####
# jm = jags.model("Chinook/Src/DraftModelSimJAG.R", data = data,  
#                 n.chains = 3, n.adapt = n.adapt)
# update(jm, n.iter = n.update)
# zc.pooled = coda.samples(jm, variable.names = parameters.to.save, n.iter = n.iter)
# 
# 
# 
# #### summarizing the JAGS results #####
# MCMCsummary(zc.pooled,params = c("alpha", "beta",
#                                  "coef", "mu.coef",
#                                  "sigma.coef"))
# MCMCtrace(zc.pooled, params = c("alpha"), pdf = FALSE)
# MCMCtrace(zc.pooled, params = c("beta"), pdf = FALSE)
# MCMCtrace(zc.pooled, params = c("coef"), pdf = FALSE)
# MCMCtrace(zc.pooled, params = c("sigma"), pdf = FALSE)
# MCMCtrace(zc.pooled, params = c("mu.c"), pdf = FALSE)
# dev.off()
