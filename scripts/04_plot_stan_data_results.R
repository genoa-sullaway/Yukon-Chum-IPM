library(tidyverse)
library(tidybayes)
library(here)
library(rstan)
library(bayesplot)
library(rstanarm)
 
# load model ==============
bh_fit<- read_rds("output/stan_fit_DATA.RDS")

# traceplot ========
traceplot(bh_fit,pars=  c( "D_scale", "theta1[1]","theta1[2]"))#,"theta1[3]","theta1[4]",
                           #"theta2[1]","theta2[2]","theta2[3]"))

traceplot(bh_fit,pars=  c("prob[1]", "prob[2]","prob[3]", "basal_p_1", "basal_p_2"))

traceplot(bh_fit,pars=  c("log_catch_q","g"))

traceplot(bh_fit,pars=  c("N_sp_start_log"))

traceplot(bh_fit,pars=  c("N_sp"))

traceplot(bh_fit,pars=  c("N_j"))

traceplot(bh_fit,pars=  c("N_j_start_log"))

# parameter plots ======== 
plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "D_scale", #"theta1[1]","theta1[2]",#"theta1[3]","theta1[4]",
               "theta2[1]","theta2[2]","theta2[3]" ),
     fill_color = "blue")
 
plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
     pars=  c( "kappa_marine_survival"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
     pars=  c( "kappa_j_survival"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "log_F_mean"),
     fill_color = "blue")
 
plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "log_F_dev_y"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
     pars=  c( "prob[1]", "prob[2]","prob[3]", "basal_p_1", "basal_p_2"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,
     pars=  c(  "log_c_1", "log_c_2" ),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
     pars=  c("log_catch_q"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
     pars=  c( "g"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
     pars=  c( "N_sp_start_log", #"N_ocean_start_log",
               "N_catch_start_log", "N_recruit_start_log"),
     fill_color = "blue")
# 
# plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
#      pars=  c( "cov_eff1"),
#      fill_color = "blue")
# 
# plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
#      pars=  c( "cov_eff2"),
#      fill_color = "blue") 
plot(bh_fit, show_density = FALSE, ci_level = 0.95,
     pars=  c( "sigma_y_j"),
     fill_color = "blue")


plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "p_1"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "p_2"),
     fill_color = "blue")

# Plot Observed vs Predicted ========
## Spawners ==========
pred_N_SP <- summary(bh_fit, pars = c("N_sp"), 
              probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:25, each=4),
                age = rep(1:4, length.out = nrow(.))) %>%
  filter(!time>21) # remove years without full return estimates 

# ggplot(data = pred_N_SP %>% mutate(age = factor(age))) +
#   geom_line(aes(x=time, y= mean, group = age, color = age))+
#   facet_wrap(~age, scales = "free")

# plot proportions 
# sum to compare with data 
summ_n_sp <- pred_N_SP %>%
  group_by(time) %>%
  summarise(pred_n_sp = sum(mean),
            pred_se = mean(se_mean)) %>%
  cbind(obs = data_list_stan$data_stage_sp)
  
ggplot(data = summ_n_sp) +
  geom_point(aes(x=time, y = obs)) +
  geom_line(aes(x=time, y = pred_n_sp)) +
  geom_ribbon(aes(x=time, ymin = pred_n_sp-pred_se,
                  ymax = pred_n_sp+pred_se)) +
  ggtitle("spawners: obs and predicted")

## recruits ====== 
pred_N_recruit <- summary(bh_fit, pars = c("N_recruit"), 
                     probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:25, each=4),
                age = rep(1:4, length.out = nrow(.))) %>%
  filter(!time>21) 

# plt proportions 

# sum to compare with data 
summ_n_rec <- pred_N_recruit %>%
  group_by(time) %>%
  summarise(pred_n_rec = sum(mean),
            pred_se = mean(se_mean)) %>% 
  cbind(obs = data_list_stan$data_stage_return) 

ggplot(data = summ_n_rec) +
  geom_point(aes(x=time, y = obs)) +
  geom_line(aes(x=time, y = pred_n_rec)) +
  geom_ribbon(aes(x=time, ymin = pred_n_rec-pred_se,
                  ymax = pred_n_rec+pred_se))+
  ggtitle(("recruits: obs and predicted"))

## harvest ====== 
pred_N_harvest <- summary(bh_fit, pars = c("N_catch"), 
                          probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:25, each=4),
                age = rep(1:4, length.out = nrow(.))) %>% 
  filter(!time>21) 

# plt proportions 

# sum to compare with data 
summ_n_harvest <- pred_N_harvest %>%
  group_by(time) %>%
  summarise(pred_n_harvest = sum(mean),
            pred_se = mean(se_mean)) %>% 
  cbind(obs = data_list_stan$data_stage_harvest)  

ggplot(data = summ_n_harvest) +
  geom_point(aes(x=time, y = obs)) +
  geom_line(aes(x=time, y = pred_n_harvest)) +
  geom_ribbon(aes(x=time, ymin = pred_n_harvest-pred_se,
                  ymax = pred_n_harvest+pred_se))+
  ggtitle(("Harvest, est and observed"))

## juveniles ====== 
# multiply by catch q to fit observations
catch_q <- summary(bh_fit, pars = c("log_catch_q"), 
                   probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>% 
  mutate(mean = exp(mean))

pred_N_j <- summary(bh_fit, pars = c("N_j"), 
                          probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = 1:nrow(.))
                
# plot proportions 
# sum to compare with data 
summ_n_j <- pred_N_j %>%
 dplyr::mutate(mean_J_Q = mean*catch_q$mean,
               se_mean = se_mean*catch_q$mean) %>% 
  cbind(obs = data_list_stan$data_stage_j) #%>%
#  filter(!time<5)
#  dplyr::mutate(obs = obs*catch_q$mean) 

ggplot(data = summ_n_j) +
  geom_point(aes(x=time, y = obs)) +
  geom_line(aes(x=time, y = mean_J_Q)) +
 # geom_line(aes(x=time, y = mean), color = "green") +
  geom_ribbon(aes(x=time, ymin = mean_J_Q-se_mean,
                  ymax = mean_J_Q+se_mean), alpha = 0.5)+
  ggtitle(("Juveniles, est and observed"))
 
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
  dplyr::mutate(time = rep(1:20, length.out = nrow(.)), 
                variable = case_when(grepl("kappa_marine_survival",rowname) ~ "kappa_marine_survival",
                                     TRUE ~ "kappa_j_survival"))%>%
  filter(!time<6)

ggplot(data = kappasurvival, aes(x=time, y = mean, group = variable ,color = variable)) + 
  geom_line( ) +
  geom_ribbon(aes(x=time, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5) 

ggplot(data = kappasurvival, aes(x=time, y = mean, group = variable ,color = variable)) + 
  geom_line( ) +
  geom_ribbon(aes(x=time, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5) +
  facet_wrap(~variable, scales = "free")

# plot  estimated marine mort ======
kappa_marine_mortality <- summary(bh_fit, pars = c("kappa_marine_mortality"), 
                    probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>% 
  dplyr::mutate(time = rep(1:20, length.out = nrow(.)) )

ggplot(data = kappa_marine_mortality, aes(x=time, y = mean )) + 
  geom_line( ) +
  geom_ribbon(aes(x=time, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5) 

# plot  estimated marine mort ======
kappa_marine_survival <- summary(bh_fit, pars = c("kappa_marine_survival"), 
                        probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>% 
  dplyr::mutate(time = rep(1:20, length.out = nrow(.)) ) 

ggplot(data = kappa_marine_survival, aes(x=time, y = mean )) + 
  geom_line( ) +
  geom_ribbon(aes(x=time, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5) 

# plot estimated productivity ======
productivity <- summary(bh_fit, pars = c("p_1", "p_2"), 
                    probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>% 
  dplyr::mutate(time = rep(1:20, length.out = nrow(.)), 
                variable = case_when(grepl("p_1",rowname) ~ "p_1",
                                     TRUE ~ "p_2"))

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

# PLOT PARAMS  ======================  
# data_list - holds simulated values, this is from: simulate_data_age_structure.R
params <- summary(bh_fit, pars = c("log_c_1","log_c_2","log_catch_q", 
                                   "D_scale", "theta1", "theta2", "basal_p_1", "basal_p_2"), 
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

