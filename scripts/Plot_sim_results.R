library(tidyverse)
library(tidybayes)
library(here)
library(rstan)
library(bayesplot)
library(rstanarm)
# pairs with stan mod BH SIM script and simulate_data_age_strucutre.R script

# functions for tidying ===========
# Function to remove '[' character
remove_bracket <- function(lst) {
  sapply(lst, function(x) gsub("\\[", "", x))
}
remove_bracket2 <- function(lst) {
  sapply(lst, function(x) gsub("\\]", "", x))
}
remove_comma <- function(lst) {
  sapply(lst, function(x) gsub("\\,", "", x))
}

# load model ==============
bh_fit<- read_rds("output/stan_fit_SIMULATED_OUTPUT.RDS")

# traceplot ========
traceplot(bh_fit,pars=  c( "D_scale", "theta1[1]","theta1[2]","theta2[1]"))

traceplot(bh_fit,pars=  c("prob[1]", "prob[2]","prob[3]", "basal_p_1", "basal_p_2"))

traceplot(bh_fit,pars=  c("log_F","log_catch_q","g"))

traceplot(bh_fit,pars=  c("N_sp_start_log"))

# parameter plots ======== 
plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "D_scale", "theta1[1]","theta1[2]","theta2[1]","theta2[2]"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "p_1"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c("log_c_1", "log_c_2"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "p_2"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
     pars=  c( "prob[1]", "prob[2]","prob[3]", "basal_p_1", "basal_p_2"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
     pars=  c(  "log_F" ),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
     pars=  c("log_catch_q"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
     pars=  c( "g"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
     pars=  c( "sigma_y_j","sigma_y_sp","sigma_y_r", "sigma_y_h"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
     pars=  c( "N_sp_start_log", #"N_ocean_start_log",
               "N_catch_start_log", "N_recruit_start_log"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
     pars=  c( "cov_eff1"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
     pars=  c( "cov_eff2"),
     fill_color = "blue")

# Plot Observed vs Predicted ========
## Spawners ==========
pred_N_SP <- summary(bh_fit, pars = c("N_sp"), 
                     probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:25, each=4),
                age = rep(1:4, length.out = nrow(.))) %>%
  filter(!time>21) # remove years without full return estiamtes 

# plt proportions 
# sum to comarpe with data 
summ_n_sp <- pred_N_SP %>%
  group_by(time) %>%
  summarise(pred_n_sp = sum(mean),
            pred_se = mean(se_mean)) %>% 
  cbind(obs = data_list_stan$data_stage_sp)  


ggplot(data = summ_n_sp) +
  geom_point(aes(x=time, y = obs)) +
  geom_line(aes(x=time, y = pred_n_sp)) +
  geom_ribbon(aes(x=time, ymin = pred_n_sp-pred_se,
                  ymax = pred_n_sp+pred_se))

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
  ggtitle(("Recruits, est and observed"))

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
#  dplyr::mutate(obs = obs*catch_q$mean) 

ggplot(data = summ_n_j) +
  geom_point(aes(x=time, y = obs)) +
  geom_line(aes(x=time, y = mean_J_Q)) +
  geom_ribbon(aes(x=time, ymin = mean_J_Q-se_mean,
                  ymax = mean_J_Q+se_mean), alpha = 0.5)+
  ggtitle(("Juveniles, est and observed"))

# plot time series of estimated fishing mortality ======
fishing <- summary(bh_fit, pars = c("log_F"), 
                   probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>% 
  mutate(mean = exp(mean),
         time = 1:nrow(.))

ggplot(data = fishing) + 
  geom_line(aes(x=time, y = mean)) +
  geom_ribbon(aes(x=time, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5) +
  ylab("Fm")

# plot  estimated survival ======
survival <- summary(bh_fit, pars = c("p_1", "p_2"), 
                    probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>% 
  dplyr::mutate(time = rep(1:20, length.out = nrow(.)), 
                variable = case_when(grepl("p_1",rowname) ~ "p_1",
                                     TRUE ~ "p_2"))

ggplot(data = survival, aes(x=time, y = mean, group = variable ,color = variable)) + 
  geom_line( ) +
  geom_ribbon(aes(x=time, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5) 


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
                                   "D_scale", "theta1", "theta2"), 
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
  facet_wrap(~rowname, scales = 'free') +
  labs(caption = "red is observed, black is model")


# OLD ===========================
# Plot Fishing Mortality ========
fm <- summary(bh_fit, pars = c("log_F"), 
              probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = 1:nrow(.))

fm_dat <- data.frame(log_F = log(data_list_plot$F)) %>%
  gather(1:ncol(.), key = "rowname", value = "mean_obs")  

ggplot() + 
  geom_density(data = fm, aes(x=mean), fill ="black", alpha = 0.5) +
  geom_density(data = fm_dat, aes(x=mean_obs), fill ="darkgreen", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(fm$mean)), color = "black") +
  geom_vline(aes(xintercept = mean(fm_dat$mean_obs)),color = "darkgreen")

# plot prob =====
age_structure <- summary(bh_fit, pars = c("prob"), probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column() %>% 
  dplyr::mutate(rowname = case_when(rowname == "prob[1]"~ "prob_1",
                                    rowname == "prob[2]"~ "prob_2",
                                    rowname == "prob[3]"~ "prob_3"))

prob_obs <- data.frame(prob_1 = data_list_plot$`prob[1]`,
                       prob_2 = data_list_plot$`prob[2]`,
                       prob_3 = data_list_plot$`prob[3]`) %>% 
  gather(1:ncol(.), key = "rowname", value = "mean_obs")  %>%
  left_join(age_structure)

ggplot()+
  geom_point(data = prob_obs, aes(x=rowname, y = mean_obs), color = "red")+
  geom_point(data = prob_obs, aes(x=rowname, y = mean), color = "black") +
  labs(caption = "black is modeled, red is observed")



# plot n ocean prop =====
n_ocean_summary <- summary(bh_fit, pars = c("N_ocean"), probs = c(0.1, 0.9))$summary

n_ocean_prop<- n_ocean_summary %>%
  data.frame()%>%
  rownames_to_column() %>% 
  separate(rowname, into = c("variable", "time", "age" ), sep =c(7,-2))  %>%
  dplyr::mutate(time = as.numeric(remove_comma(remove_bracket(time))),
                age=as.numeric(remove_comma(remove_bracket2(age)))) %>%
  as.data.frame() %>% 
  # dplyr::select(-del) %>%
  #filter(!is.nan(mean)) %>% 
  group_by(time) %>%
  dplyr::mutate(sum = sum(mean),
                proportion = mean/sum)  %>%
  select(time,age,proportion)  

ggplot(data = n_ocean_prop) +
  geom_bar(aes(x=time, y=proportion, 
               fill = age, group = age), stat = "identity")

obs_p <- as.data.frame(data_list_plot$p)  

n_ocean_prop_mean <- n_ocean_prop %>%
  ungroup() %>%
  group_by(age) %>%
  dplyr::summarise(mean_prop = mean(proportion),
                   sd = sd(proportion)) %>%
  cbind(obs_p)

n_ocean_prop_mean %>% 
  ggplot() +  
  geom_point(aes(age, mean_prop), color = "red",alpha = 0.5 )  +
  geom_point(aes(x=age, y = `data_list_plot$p`), color = "black" ,alpha = 0.5) + 
  scale_y_continuous(limits = c(0,1)) 


# plot n recruit prop =====
# load recruit from output and calculate proportions to see if the age structure is the same there.... 
n_recruit_summary <- summary(bh_fit, pars = c("N_recruit"), probs = c(0.1, 0.9))$summary

n_recruit_prop<- n_recruit_summary %>%
  data.frame()%>%
  rownames_to_column() %>% 
  separate(rowname, into = c("variable", "time", "age" ), sep =c(9,-2))  %>%
  dplyr::mutate(time = as.numeric(remove_comma(remove_bracket(time))),
                age=as.numeric(remove_comma(remove_bracket2(age)))) %>%
  as.data.frame() %>% 
  # dplyr::select(-del) %>%
  #filter(!is.nan(mean)) %>% 
  group_by(time) %>%
  dplyr::mutate(sum = sum(mean),
                proportion = mean/sum)  %>%
  select(time,age,proportion)  

ggplot(data =  n_recruit_prop) +
  geom_bar(aes(x=time, y=proportion, 
               fill = age, group = age), stat = "identity")

obs_p <- as.data.frame(data_list_plot$p)  

n_recruit_prop_mean <-  n_recruit_prop %>%
  ungroup() %>%
  group_by(age) %>%
  dplyr::summarise(mean_prop = mean(proportion),
                   sd = sd(proportion)) %>%
  cbind(obs_p)

n_recruit_prop_mean %>% 
  ggplot() +  
  geom_point(aes(age, mean_prop), color = "red",alpha = 0.5 )  +
  geom_point(aes(x=age, y = `data_list_plot$p`), color = "black" ,alpha = 0.5) + 
  scale_y_continuous(limits = c(0,1)) 

# plot n returning abundance =====
# load n_returning from output and calculate proportions to see if the age structure is the same there.... 
abund_recruit<-  n_recruit_summary %>%
  data.frame()%>%
  rownames_to_column() %>% 
  separate(rowname, into = c("variable", "time", "age" ), sep =c(9,-2))  %>%
  dplyr::mutate(time = remove_comma(remove_bracket(time)),
                age=remove_comma(remove_bracket2(age))) %>%
  as.data.frame() %>%  
  group_by(time) %>%
  select(time,age,mean) %>%
  mutate(time = as.numeric(time))%>% 
  filter(!time<10)

ggplot(data = abund_recruit) +
  geom_bar(aes(x=time, y=mean, 
               fill = age, group = age), stat = "identity", position = "stack")

# Plot N_sp proportions ====================
# load n_returning from output and calculate proportions to see if the age structure is the same there.... 
n_sp_summary <- summary(bh_fit, pars = c("N_sp"), probs = c(0.1, 0.9))$summary

n_sp_prop<- n_sp_summary %>%
  data.frame()%>%
  rownames_to_column() %>% 
  separate(rowname, into = c("variable", "time", "age" ), sep =c(4,-2))  %>%
  dplyr::mutate(time = as.numeric(remove_comma(remove_bracket(time))),
                age=as.numeric(remove_comma(remove_bracket2(age)))) %>%
  as.data.frame() %>% 
  # dplyr::select(-del) %>%
  filter(!is.nan(mean)) %>% 
  group_by(time) %>%
  dplyr::mutate(sum = sum(mean),
                proportion = mean/sum)  %>%
  select(time,age,proportion)  

ggplot(data = n_sp_prop) +
  geom_bar(aes(x=time, y=proportion, 
               fill = age, group = age), stat = "identity")

obs_p <- as.data.frame(data_list_plot$p)  

n_sp_prop_mean <- n_sp_prop %>%
  ungroup() %>%
  group_by(age) %>%
  dplyr::summarise(mean_prop = mean(proportion),
                   sd = sd(proportion)) %>%
  cbind(obs_p)

n_sp_prop_mean %>% 
  ggplot() +  
  geom_point(aes(age, mean_prop), color = "red",alpha = 0.5 )  +
  geom_point(aes(x=age, y = `data_list_plot$p`), color = "black" ,alpha = 0.5) + 
  scale_y_continuous(limits = c(0,1)) 

# Plot N_sp abundances ====================
abund_sp<-  n_sp_summary %>%
  data.frame()%>%
  rownames_to_column() %>% 
  separate(rowname, into = c("variable", "time", "age" ), sep =c(4,-2))  %>%
  dplyr::mutate(time = remove_comma(remove_bracket(time)),
                age=remove_comma(remove_bracket2(age))) %>%
  as.data.frame() %>%  
  group_by(time) %>%
  select(time,age,mean) %>%
  mutate(time = as.numeric(time))%>% 
  filter(!time<10)

ggplot(data = abund_sp) +
  geom_bar(aes(x=time, y=mean, 
               fill = age, group = age), stat = "identity", position = "stack")



# plot time series of predicted vs observed =========== 
#juveniles
obs_juv<-as.data.frame(data_list_plot$data_stage_j) %>%
  mutate(time = 1:nrow(.),
         obs_juv = data_list_plot$data_stage_j)
pred_juv <- summary(bh_fit, pars = c("N_j"), probs = c(0.1, 0.9))$summary

pred_juv_df <- pred_juv%>%
  data.frame()%>%
  rownames_to_column() %>% 
  separate(rowname, into = c("variable", "time", "del" ), sep =c(3,-1))  %>%
  dplyr::mutate(time = as.numeric(remove_comma(remove_bracket(time)))) %>%
  as.data.frame()  %>%
  dplyr::select(-del) %>%
  left_join(obs_juv)

ggplot(data=pred_juv_df) +
  geom_path(aes(x=time, y = mean)) +
  geom_ribbon(aes(x=time, ymin =X10., ymax = X90.)) + 
  geom_path(aes(x=time, y = obs_juv)) +
  theme_classic()

# recruit =======
obs_recruit <- as.data.frame(data_list_plot$data_stage_return) %>%
  mutate(time = 1:nrow(.)) %>%
  rename(obs_recruit = "data_list_plot$data_stage_return")

mod_recruit<-  abund_recruit %>%
  group_by(time) %>%
  summarise(mean = sum(mean)) %>%
  left_join(obs_recruit) %>%
  gather(2:3, key = "key", value = "value")

testr <- mod_recruit %>%
  filter(!is.nan(value),
         !is.na(value)) %>%
  group_by(key) %>%
  summarise(se = sd(value)/sqrt(length(value)),
            var = var(value))

ggplot(data = mod_recruit) +
  geom_path(aes(x=time, y = value, group =key, color = key))


# Plot Compare Kappa obs and derived ============

kappa_obs <-   data.frame(kappa_j=c(data_list_plot$kappa_j),
                          kappa_m=c(data_list_plot$kappa_marine)) %>% 
  mutate(time = 1:nrow(.)) %>%
  gather(1:2, key = "variable", value = "obs_kappa")

kappa_mod <- summary(bh_fit, pars = c("kappa_marine", "kappa_j"), probs = c(0.1, 0.9))$summary

kappa_mod_df<- kappa_mod %>%
  data.frame()%>%
  rownames_to_column() %>% 
  separate(rowname, into = c("variable", "del", "time" ), sep =c(7,-3))  %>%
  select(-del) %>% 
  dplyr::mutate(time = as.numeric(remove_bracket2(remove_bracket(time)))) %>%
  as.data.frame()  %>%
  left_join(kappa_obs) %>% 
  select(time, variable, mean, obs_kappa) %>%
  gather(3:4, key = "id", value = "value") %>% 
  filter(!time <10)

ggplot(data =kappa_mod_df) +
  geom_point(aes(x=time, y =value, color = id)) + 
  facet_wrap(~variable, scales = "free")

 