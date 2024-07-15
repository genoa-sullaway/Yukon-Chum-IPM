library(tidyverse)
library(tidybayes)
library(here)
library(rstan)
library(bayesplot)
library(rstanarm)

# load model ==============
bh_fit<- read_rds("output/stan_fit_DATA.RDS")

years <-read_csv("data/processed_data/yukon_fall_spawners.csv") %>%
  filter(cal_year >= year_min) %>%
  dplyr::select(cal_year) %>%
  dplyr::mutate(time = c(1:nrow(.)))

# Plot Observed vs Predicted ========
## Spawners ==========
pred_N_SP <- summary(bh_fit, pars = c("N_sp"), 
                     probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:28, each=4),
                age = rep(3:6, length.out = nrow(.))) %>%
  filter(!time>21) %>% # remove years without full return estimates 
  left_join(years) 
 
# plot proportions 
# sum to compare with data 
summ_n_sp <- pred_N_SP %>%
  group_by(cal_year) %>%
  summarise(mean = sum(mean),
           # se_mean = mean(se_mean),
            CI_10 = sum(X10.),
            CI_90 = sum(X90.)) %>%
  cbind(obs = data_list_stan$data_stage_sp) %>%
  mutate(rowname = "sp")  

ggplot(data = summ_n_sp) +
  geom_point(aes(x=cal_year, y = obs)) +
  geom_line(aes(x=cal_year, y = mean)) +
  geom_ribbon(aes(x=cal_year, ymin = CI_10,
                  ymax = CI_90)) +
  ggtitle("spawners: obs and predicted")+
  scale_x_continuous(breaks = c(2002, 2006,2010, 2015,2020)) +
  theme_classic()


# Spawners PP ====== 
mcmc_areas(bh_fit, pars=c("N_sp_sim"))

pred_N_SP_sim <- summary(bh_fit, pars = c("N_sp_sim"), 
                     probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = 1:nrow(.),
                 mean= exp(mean),
                 # ci_10 =  (X10.),
                 # ci_90 =  (X90.)
                 ci_10 = exp(X10.),
                 ci_90 = exp(X90.)
                ) %>%
  left_join(years) %>%
  cbind(obs = data_list_stan$data_stage_sp) %>%
  dplyr::mutate(rowname = "sp")  

ggplot(data = pred_N_SP_sim) +
  geom_point(aes(x=cal_year, y = obs)) +
  geom_line(aes(x=cal_year, y = obs)) +
  geom_line(aes(x=cal_year, y = mean)) +
  geom_ribbon(aes(x=cal_year, ymin = ci_10,
                  ymax = ci_90)) +
  ggtitle("spawners: obs and predicted")+
  scale_x_continuous(breaks = c(2002, 2005,2010, 2015,2020)) +
  theme_classic()

## recruits ====== 
pred_N_recruit <- summary(bh_fit, pars = c("N_recruit"), 
                          probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:28, each=4),
                age = rep(3:6, length.out = nrow(.))) %>%
  filter(!time>21) %>%
  left_join(years) 

summ_n_rec <- pred_N_recruit %>%
  group_by(cal_year) %>%
  summarise(mean = sum(mean),
            se_mean = mean(se_mean)) %>% 
  cbind(obs = data_list_stan$data_stage_return)  %>%
  mutate(rowname = "recruit")  

ggplot(data = summ_n_rec) +
  geom_point(aes(x=cal_year, y = obs)) +
  geom_line(aes(x=cal_year, y = mean)) +
  geom_ribbon(aes(x=cal_year, ymin = mean-se_mean,
                  ymax = mean+se_mean))+
  ggtitle(("recruits: obs and predicted"))+
  scale_x_continuous(breaks = c(2002, 2005,2010, 2015,2020))

## harvest ====== 
pred_N_harvest <- summary(bh_fit, pars = c("N_catch"), 
                          probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:28, each=4),
                age = rep(3:6, length.out = nrow(.))) %>% 
  filter(!time>21) 

# plt proportions 
# sum to compare with data 
summ_n_harvest <- pred_N_harvest %>%
  group_by(time) %>%
  summarise(mean = sum(mean),
            se_mean = mean(se_mean)) %>% 
  cbind(obs = data_list_stan$data_stage_harvest)  %>%
  mutate(rowname = "harvest")  

ggplot(data = summ_n_harvest) +
  geom_point(aes(x=time, y = obs)) +
  geom_line(aes(x=time, y = mean)) +
  geom_ribbon(aes(x=time, ymin = mean-se_mean,
                  ymax = mean+se_mean)) +
  theme_classic() +
  scale_x_continuous(breaks = c(2002, 2005,2010, 2015,2020))

## Juveniles PP ====== 
# multiply by catch q to fit observations

pred_N_jpp <- summary(bh_fit, pars = c("N_j_pp"), 
                    probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = 1:nrow(.)) %>%
  #filter(!time==22) %>%
  left_join(years) %>%
  cbind(obs = data_list_stan$data_stage_j) %>% 
  dplyr::mutate(mean = exp(mean),
         ci_10=exp(X10.),
         ci_90=exp(X90.))

ggplot(data = pred_N_jpp) +
  geom_line(aes(x=cal_year, y = mean)) + 
  geom_point(aes(x=cal_year, y = obs)) +
  # geom_line(aes(x=time, y = mean), color = "green") +
   geom_ribbon(aes(x=cal_year, ymin = ci_10,
                   ymax = ci_90), alpha = 0.5)+
  ggtitle(("Juveniles, est "))


## eggs =============================
pred_N_eggs_sum <- summary(bh_fit, pars = c("N_e_sum"), 
                           probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:22, each=1)) %>%
  filter(!time == 22) %>% 
  cbind(obs_j = data_list_stan$data_stage_j,
        pred_j = pred_N_j$mean) %>% 
  # filter(!time<7)  %>% 
  mutate(rowname = "eggs") %>%
  select(rowname,time,mean,obs_j,pred_j)  

ggplot(data = pred_N_eggs_sum) + 
  geom_line(aes(x=time, y = mean)) +
  # geom_line(aes(x=time, y = mean*10), color = "green") +  
  geom_line(aes(x=time, y = pred_j), color = "blue")  

ggplot(data = pred_N_eggs_sum) + 
  geom_line(aes(x=time, y = mean)) 
  

# Productivity =======
productivity1 <- summary(bh_fit, pars = c("p_1"), 
                         probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>% 
  dplyr::mutate( variable = "p_1",
                 time =  1:nrow(.)) 

productivity2 <- summary(bh_fit, pars = c("p_2"), 
                         probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>% 
  dplyr::mutate( variable = "p_2",
                 time =  1:nrow(.)) 

productivity <- rbind(productivity1,productivity2)

ggplot(data = productivity, aes(x=time, y = mean, group = variable ,color = variable)) + 
  geom_line( ) +
  geom_ribbon(aes(x=time, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5) +
  facet_wrap(~variable, scales = "free")

ggplot(data = productivity, aes(x=time, y = mean, group = variable ,color = variable)) + 
  geom_line( ) +
  geom_ribbon(aes(x=time, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5)





# estimated fishing mortality ======
fishing <- summary(bh_fit, pars = c("F"), 
                   probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>% 
  mutate(mean =  (mean),
         time = 1:nrow(.)) %>% 
  filter(!time> 21 & !time<5)

ggplot(data = fishing) + 
  geom_line(aes(x=time, y = mean)) + 
  ylab("Instantaneous fishing mortality")

# plot  estimated kappas survival ======
kappasurvival <- summary(bh_fit, pars = c("kappa_marine_survival", "kappa_j_survival"), 
                         probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>% 
  dplyr::mutate(time = rep(1:21, length.out = nrow(.)), 
                variable = case_when(grepl("kappa_marine_survival",rowname) ~ "kappa_marine_survival",
                                     TRUE ~ "kappa_j_survival")) %>% 
  left_join(years)# %>% 
#filter(!time<5 & !time>20)

ggplot(data = kappasurvival%>%   filter(!time<2),aes(x=cal_year, y = mean, group = variable ,color = variable)) + 
  geom_line( ) +
  geom_ribbon(aes(x=cal_year, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5) + 
  scale_x_continuous(breaks = c(2002,2006,2010, 2015,2020, 2022))

ggplot(data = kappasurvival %>%   filter(!time<2),
       aes(x=cal_year, y = mean, group = variable ,color = variable)) + 
  geom_line( ) +
  geom_ribbon(aes(x=cal_year, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5) +
  facet_wrap(~variable, scales = "free") + 
  scale_x_continuous(breaks = c(2002,2006,2010, 2015,2020, 2022)) +
  theme_classic() + 
  xlab("Calendar Year") + 
  ylab("Survival Rate")

ggplot(data = kappasurvival %>%
         filter(!time<5 & !time>20), 
       aes(x=cal_year, y = mean, group = variable ,color = variable)) + 
  geom_line( ) +
  geom_ribbon(aes(x=cal_year, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5) +
  facet_wrap(~variable, scales = "free") + 
  scale_x_continuous(breaks = c(2002,2006,2010, 2015,2020, 2022)) +
  theme_classic() + 
  xlab("Calendar Year") + 
  ylab("Survival Rate")

# plot estimated productivity ======
productivity <- summary(bh_fit, pars = c("p_1", "p_2"), 
                        probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>% 
  dplyr::mutate(time = rep(1:21, length.out = nrow(.)), 
                variable = case_when(grepl("p_1",rowname) ~ "p_1",
                                     TRUE ~ "p_2"))  

ggplot(data = productivity, aes(x=time, y = mean, group = variable ,color = variable)) + 
  geom_line( ) +
  geom_ribbon(aes(x=time, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5) +
  facet_wrap(~variable, scales = "free")

ggplot(data = productivity, aes(x=time, y = mean, group = variable ,color = variable)) + 
  geom_line( ) +
  geom_ribbon(aes(x=time, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5)

# plot sigma  ======
# sigma <- summary(bh_fit, pars = c("sigma_y_h", 
#                                   "sigma_y_r", 
#                                   "sigma_y_j",
#                                   "sigma_y_sp"), 
#                     probs = c(0.1, 0.9))$summary %>%
#   data.frame() %>%
#   rownames_to_column()   
# rename(pred = "mean") %>% 
# cbind(obs = data_list_stan$p_obs) %>% 
# dplyr::select(1,2,9) %>% 
# gather(2:3, key = "key", value = "value")

# ggplot(data = sigma) +
#   geom_point(aes(x= rowname, y = mean)) + 
#   theme_classic()

# plot age comp  ======
age_comp <- summary(bh_fit, pars = c("p"), 
                    probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column() %>% 
  rename(pred = "mean") %>% 
  cbind(obs = data_list_stan$p_obs) %>% 
  dplyr::select(1,2,9) %>% 
  gather(2:3, key = "key", value = "value")

ggplot(data = age_comp) +
  geom_point(aes(x= rowname, y = value, group = key, color = key)) + 
  theme_classic()

# plot theta ========
theta <- summary(bh_fit, pars = c("theta_1_1_pp","theta_1_2_pp",
                                  "theta_2_1_pp","theta_2_2_pp"), 
                 probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column() 

ggplot(data = theta,aes(x= mean, y = rowname, group = rowname, color = rowname)) +
  geom_point() + 
  theme_classic() +
  geom_errorbar(aes(xmin =X10., xmax = X90.),width = 0.1) + 
  facet_wrap(~rowname,scales = "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_vline(xintercept=0)



# PLOT PARAMS  ======================  
# data_list - holds simulated values, this is from: simulate_data_age_structure.R
params <- summary(bh_fit, pars = c("log_c_1","log_c_2","log_catch_q", 
                                   "D_scale", "theta1", "theta2",
                                   "basal_p_1", "basal_p_2"), 
                  probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column() %>%
  dplyr::mutate(rowname = case_when(rowname == "theta1[1]"~ "theta1_1",
                                    rowname == "theta1[2]"~ "theta1_2",
                                    rowname == "theta2[1]"~ "theta2_1",
                                    TRUE ~ rowname))

params %>% 
  ggplot() + 
  geom_linerange(aes(rowname, ymin = X10.,ymax = X90.)) + 
  geom_crossbar(aes(rowname, mean, ymin = X10.,ymax = X90.),  fill= 'grey') + 
  #geom_point(aes(x=rowname, y = mean_obs), color = "red") + 
  facet_wrap(~rowname, scales = 'free')  

# estimated kappa marine mortality ======
kappa_marine_mortality <- summary(bh_fit, pars = c("kappa_marine_mortality"), 
                                  probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>% 
  mutate(mean = exp(mean),
         time = 1:nrow(.)) 

kappa_marine_survival <- summary(bh_fit, pars = c("kappa_marine_survival"), 
                                 probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>% 
  mutate(mean = exp(mean),
         time = 1:nrow(.),
         mort = -log(mean))


# calculate SR based one stiamted values========
test <- summary(bh_fit, pars = c("kappa_j_survival", "p_1", "N_egg_start_log"), 
                probs = c(0.1, 0.9))$summary  







