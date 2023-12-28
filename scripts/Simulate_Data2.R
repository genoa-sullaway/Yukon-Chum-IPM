
library(actuaryr)
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
yukon_fall<- read_csv("data/Yukon_Escapement_ADFG/Yukon_Fall_Chum_RR_JTC.csv")  
yukon_spring <- read_excel("data/Yukon_Escapement_ADFG/Yukon Summer Chum Total Run 1978-2022 Run Rec.xlsx")
kusko_estimated_parameters<- readRDS("output/optim_output_par_data2021.RDS") 
 kusko<-data.frame(Year = c(1988:(2022-1)),   
                 pred_N_est= as.vector(c(kusko_estimated_parameters[2:35])))  

# Init ===================
n<-105 #number of samples per population
pops <- 3 #seq(1,3,1) #population pointer vector
population<- c(rep(1,n), rep(2,n), rep(3,n)) #population pointer vector
n.pop <-length(unique(population)) #number of population
year <- rep(seq(1,n), n.pop) #creating a year pointer

fs = 2440 # fecundity, from Gilk Baumer paper about Kusko (not sig different from spring fall). Check lit for yukon. 
Ps = 0.5 # proportion of females. supported by Gilk Baumer 

#Bev Holt parameters ===================
# p for alpha, and c for carrying capacity 
basal_p_1 <- matrix(nrow=n,ncol=n.pop,
                    c(rnorm(n, 0.05, 0), #simulating a unique beta for each population
                       rnorm(n, 0.05, 0), 
                       rnorm(n, 0.05, 0)))

basal_p_2 <-matrix(nrow=n,ncol=n.pop,
                   c(rnorm(n, 0.15, 0),
                     rnorm(n, 0.15, 0), 
                     rnorm(n, 0.15, 0)))

c_1 <- matrix(nrow=1, ncol=n.pop,
       c(rnorm(1,1e8, 0), #13.8 , #simulating a unique alpha for each population
         rnorm(1,1e8, 0),
         rnorm(1,1e8, 0)))

c_2 <- matrix(nrow=1,ncol=n.pop,
              c(rnorm(n, 750000, 0), #13.8 , #simulating a unique alpha for each population
                rnorm(n, 250000, 0),
                rnorm(n, 177000, 0)))
 
# Covariate data ===================

cov1 <-  c(rnorm(n*1, 0, 2))  #Cov 1 data
cov2 <- c(rnorm(n*1, 0, 2)) #Cov2 data

theta1 <- c(0.1,0.3,0.4) #rep(0.1,n), rep(0.3,n), rep(0.4,n)) #relationship for simulated data
theta2 <- c(-0.2,0.1,-0.1) #relationship for simulated data

# Simulate populations  ===================
process_error_j <- matrix(nrow=n,ncol=1,rnorm(n*1,1,0.2))
process_error_sp <- matrix(nrow=n,ncol=1,rnorm(n*1,50, 5))
process_error_r <- matrix(nrow=n,ncol=1,rnorm(n*1,1,0.2))

N_eggs = matrix(nrow=n,ncol=n.pop,NA)
kappa_fw =  matrix(nrow=n,ncol=n.pop,NA)
N_j =  matrix(nrow=n,ncol=n.pop,NA)
kappa_sp =  matrix(nrow=n,ncol=n.pop,NA)

N_r <-matrix(nrow=n, ncol=pops, NA)

mean.spawn <- c(mean(yukon_spring$Escapement), 
                mean(yukon_fall$Estimated_Run), 
                mean(kusko$pred_N_est)) #mean spawners for each population
 
N_sp <- matrix(nrow=n, ncol=pops, 
        data= c(rnorm(n, mean.spawn[1], process_error_sp), #simulating a unique number of spawners for each population
                rnorm(n, mean.spawn[2], mean(process_error_sp)),
                rnorm(n, mean.spawn[3], mean(process_error_sp))))
plot(N_sp[,1])
plot(N_sp[,2])
plot(N_sp[,3])

p_1 =  matrix(nrow=n,ncol=n.pop,NA)
p_2 =  matrix(nrow=n,ncol=n.pop,NA)

# simulate p first, 
for (p in 1:n.pop) {
p_1[,p]  = 1 / exp(-basal_p_1[p] - (theta1[p]*cov1)) # covariate impacts survival, impact is measured through theta
p_2[,p]  = 1 / exp(-basal_p_2[p] - (theta2[p]*cov2)) 
 } 

for(p in 1:n.pop) {
 for (i in 2:n) {
  N_eggs[i,p] = fs*Ps*N_sp[i-1,p]
  kappa_fw[i,p] <-  (p_1[i,p])/(1 + ((p_1[i,p]*N_eggs[i,p])/c_1[p])) # + obs_error_j[[i]] # SR formula from cunnigham 2018
  N_j[i,p] = (N_eggs[i,p]*kappa_fw[i,p]) #+ obs_error_j[i]
  
  kappa_sp[i,p] <- (p_2[i,p])/(1 + ((p_2[i,p]*N_j[i,p])/c_2[p]))  
  N_r[i,p] = (N_j[i,p]*kappa_sp[i,p])# + obs_error_sp[i]  
    }
  }

N_j[1,] = colMeans(N_j[2:n,]) #mean(N_j[2:n,1:n.pop]) # just so i don't get yelled at about NA's later down the road 

N_j_sim <-  matrix(nrow=n,ncol=n.pop,NA)
N_r_sim <-  matrix(nrow=n,ncol=n.pop,NA)

for (p in 1:n.pop) {
N_j_sim[,p] = rlnorm(n, log(N_j[2:n,p]), process_error_j[2:n])
N_r_sim[,p] = rlnorm(n, log(N_r[2:n,p]), process_error_r[2:n])
} 

sigma_j_obs<-c(sd(log(N_j_sim)[6:n,1]),sd(log(N_j_sim)[6:n,2]),sd(log(N_j_sim)[6:n,3]))  #[6:n])
sigma_sp_obs<-c(sd(log(N_sp)[6:n,1]),sd(log(N_sp)[6:n,2]),sd(log(N_sp)[6:n,3]))
sigma_r_obs<-c(sd(log(N_r_sim)[6:n,1]),sd(log(N_r_sim)[6:n,2]),sd(log(N_r_sim)[6:n,3]))

dat_sim <- list(Population = population, Year = year,
                 N_sp = N_sp,#[1:n,] ,
                 N_eggs = N_eggs[1:n,], 
                 N_j = N_j_sim, #[1:n,] , 
                 N_r =  N_r_sim,
                 p_1 = p_1, p_2=p_2, 
                 c_1=c_1, c_2=c_2,
                 kappa_sp=kappa_sp[1:n,],
                 kappa_fw=kappa_fw[1:n,],
                 cov1=cov1,cov2=cov2) 

saveRDS(dat_sim , "data/Simulated_DatBH.RDS")
 
plot(log(N_r[,1]))
plot(N_j[,1])

plot(N_sp[,1])
plot(N_sp[,2])
plot(N_sp[,3])
# plot(N_j_sim) 
# plot(N_sp_sim) 
# 
# hist(log(N_j_sim[6:n]))
# hist(log(N_sp_sim[6:n]))
# 
# hist( N_j_sim[6:n])
# hist( N_sp_sim[6:n])

##### Below doesnt work since I added more populations ========== 
# dat_sim <- cbind(Population = population, Year = year, 
#                 N_eggs = N_eggs[1:n,], N_j = N_j_sim, #[1:n,] , 
#                 N_sp = N_sp_sim,#[1:n,] ,
#                 #obs_error_sp = obs_error_sp, obs_error_j=obs_error_j,#cov1=cov1, cov2=cov2,
#                 p_1 = p_1, p_2=p_2, 
#                 c_1=c_1, c_2=c_2,
#                 process_error_j=process_error_j,
#                 process_error_sp=process_error_sp,
#                 kappa_sp=kappa_sp[1:n,],
#                 kappa_fw=kappa_fw[1:n,],
#                 cov1=cov1,cov2=cov2) 
# dat_sim <- data.frame(dat_sim)[6:n,]  
# dat_sim_plot <- dat_sim %>% 
#   gather(c(3:14), key = "id", value = "value") 
#   
# plota <- ggplot(data = dat_sim_plot, aes(x=Year, y = log(value))) +
#   geom_point() +
#   facet_wrap(~id, scales = "free")
#  
# plota
# 
# plotb <- ggplot(data = dat_sim_plot, aes(x=Year, y = value)) +
#   geom_point() +
#   facet_wrap(~id, scales = "free")
# 
# plotb
# 
# write_csv(dat_sim , "data/Simulated_DatBH.csv")
# 
# sd(dat_sim$N_j)
# sd(dat_sim$N_sp)
# sd(dat_sim$N_eggs)
# 
# log(mean(dat_sim$N_j))
# log(mean(dat_sim$N_sp))
# log(mean(dat_sim$N_eggs))
# 

