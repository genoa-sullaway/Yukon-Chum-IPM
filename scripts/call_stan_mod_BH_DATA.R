library(rstan)
library(tidyverse)
library(here)
library(bayesplot)
library(rstanarm) 
 
library(RNetCDF)
library(tidync)
library(lubridate) 

# Load data =======================================================
# Spawners =======
yukon_summer <- read_excel("data/Yukon_Escapement_ADFG/Yukon Summer Chum Total Run 1978-2022 Run Rec.xlsx") %>%
  dplyr::select(Year, `Total Run`) %>%
  dplyr::rename(Estimated_Run = `Total Run`) %>%
  dplyr::mutate(id = 1)
yukon_fall<- read_csv("data/Yukon_Escapement_ADFG/Yukon_Fall_Chum_RR_JTC.csv")  %>%
  select(Year, Estimated_Run) %>%
  dplyr::mutate(id = 2)

kusko_estimated_parameters<- readRDS("output/optim_output_par_data2021.RDS")
kusko<-data.frame(Year = c(1988:(2022-1)),
                  Estimated_Run= as.vector(c(kusko_estimated_parameters[2:35])),
                  id =3) %>% 
  filter(Year > 2001) 

mean<-mean(kusko$Estimated_Run)
kusko<-kusko %>%
  rbind(df=data.frame(Year = c(2022), Estimated_Run = c(mean), id=c(3)))

sp <- rbind(yukon_summer, yukon_fall, kusko) %>%
  filter(Year > 2001) %>%
  spread(id, Estimated_Run)
 
# Juveniles ========================================================
juv <- read_csv("data/Juv_Index_CC_aug2023/Index2.csv") %>%
      dplyr::select(Time, Estimate) %>%
  rename(Year = "Time") 

# Make proportion of juveniles per run ======================================
mean_prop<- read_csv("data/mean_prop_basis.csv") # see "script/explore_basis_proportions.R"
 
juv_prop_ayk <- expand_grid(juv, mean_prop) %>%
                dplyr::mutate(juv_index = mean*Estimate) %>%
                dplyr::select(Year, reporting_group, juv_index) %>%
                #filter(!reporting_group == "Kusko_Bristol") %>%
                dplyr::mutate(id = case_when(reporting_group == "Yukon_Summer" ~ 1,
                                             reporting_group == "Yukon_Fall" ~ 2,
                                             TRUE ~ 3)) %>% 
                dplyr::select(-reporting_group) %>% 
                spread(id, juv_index)

juv_prop_ayk[19,2]<- colMeans(juv_prop_ayk[-19,])[2] # get means of all columns except 2020, fill that in for 2020
juv_prop_ayk[19,3]<- colMeans(juv_prop_ayk[-19,])[3] # get means of all columns except 2020, fill that in for 2020
juv_prop_ayk[19,4]<- colMeans(juv_prop_ayk[-19,])[4]
# load Covariates  ==========================================================
# temporary... use M2
m2_cov<-read_csv("data/M2_df.csv") %>% # this was created in "MAPP/sceripts_02/process_M2_degreedays.R" and the M2 file was copied to this datafile
  dplyr::mutate(DOY = lubridate::yday(dates),
                Year = lubridate::year(dates)) %>% 
 # filter(!DOY>300) %>% 
  group_by(Year) %>%
  dplyr::summarise(degree_days = sum(temperature)) %>%
  filter(Year>2001) %>%
  dplyr::select(degree_days) %>%
  mutate(degree_days = scale(degree_days)) # mean scale 


# setup inputs ==============================================================
warmups <- 2000
total_iterations <- 4000
max_treedepth <-  15
n_chains <- 1
n_cores <- 4
adapt_delta <- 0.95

Ps <- 0.5 # proportion of females - assumption, need to lit check
fs <- 2440 # fecundity - Gilk and Baumer 2009 estimate for Kusko Chum

# Organize data call inputs ================================================
K = 3 # number of stocks involved
N <- c(nrow(sp)) #, nrow(sim_dat$N_sp), nrow(sim_dat$N_sp))  
#N = N_stock 
# g = c(rep(1, times = N_stock[1]), # Vector of group assignments.
#       rep(2, times = N_stock[2]),
#       rep(3, times = N_stock[3]))
#ncovars = 1 # right now just trying with temperature for stage 1      
 
data_stage_j <- as.matrix(juv_prop_ayk[,2:4]) #c(as.integer(sim_dat$N_j))#, 
                 # as.integer(sim_yukon_fall_df$N_j),
                 # as.integer(sim_kusko_df$N_j))

#data_stage_r <- sp[,2:3]
 
data_stage_sp <-as.matrix(sp[,2:4])#c(as.integer(sim_dat$N_sp))#, 
                # as.integer(sim_yukon_fall_df$N_sp),
                # as.integer(sim_kusko_df$N_sp))

kappa_j_start =  c(runif(1, 0.03, 0.07),
                   runif(1, 0.03, 0.07),
                   runif(1, 0.03, 0.07))
kappa_sp_start =  c(runif(1, 0.12, 0.2),
                   runif(1, 0.12, 0.2),
                    runif(1, 0.12, 0.2))
#  
# basal_p_1 = c(0.05,#0.05,
#               0.05) # straight from simulation 
basal_p_2 = c(0.15,
              0.15,
              0.15) # straight from simulation
#   
# cov1 = sim_dat$cov1
  cov2 =  as.matrix(m2_cov)

data_list <- list(Ps = Ps,
                  fs=fs,
                  N = N, 
                  K = K,  
                  data_stage_j = data_stage_j, 
                  data_stage_sp = data_stage_sp,
                  kappa_sp_start = kappa_sp_start,
                  #data_stage_r =data_stage_r,
                  kappa_j_start = kappa_j_start,
                  # basal_p_1=basal_p_1,
                   basal_p_2=basal_p_2 ,
                  #cov1 = cov1,
                  cov2 = cov2
                  ) 

bh_fit <- stan(
  file = here::here("scripts", "stan_mod_BH_DATA.stan"),
  data = data_list,
  chains = n_chains,
  warmup = warmups,
  iter = total_iterations,
  cores = n_cores)

mcmc_trace(bh_fit, pars = c("c_1[1]","c_1[2]","c_1[3]"))
mcmc_trace(bh_fit, pars = c("p_1[1]","p_1[2]","p_1[3]"))
mcmc_trace(bh_fit, pars = c("c_2[1]","c_2[2]","c_2[3]"))
mcmc_trace(bh_fit, pars = c("sigma_y_j[1]","sigma_y_j[2]","sigma_y_j[3]"))
mcmc_trace(bh_fit, pars = c("sigma_y_sp[1]","sigma_y_sp[2]","sigma_y_sp[3]"))

 
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
  facet_wrap(~variable, scales = 'free') # +
 # geom_point(data = obs_df, aes(variable, mean), color = "red" ) #observed

 pairs(bh_fit)
 
 