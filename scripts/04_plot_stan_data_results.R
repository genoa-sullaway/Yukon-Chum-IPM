library(tidyverse)
library(tidybayes)
library(here)
library(rstan)
library(bayesplot)
library(rstanarm)
 
# load model ==============
bh_fit<- read_rds("output/stan_fit_DATA.RDS")

# traceplot ========
traceplot(bh_fit,pars=  c( "D_scale", "theta1[1]","theta2[1]"))#,"theta1[3]","theta1[4]",
                           #"theta2[1]","theta2[2]","theta2[3]"))

traceplot(bh_fit,pars=  c("prob[1]", "prob[2]","prob[3]", "basal_p_1", "basal_p_2"))

traceplot(bh_fit,pars=  c("log_catch_q","g"))

traceplot(bh_fit,pars=  c("N_sp_start_log"))

traceplot(bh_fit,pars=  c("N_sp"))

traceplot(bh_fit,pars=  c("N_j"))

traceplot(bh_fit,pars=  c("N_j_start_log"))

# parameter plots ======== 
plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "D_scale",  "theta1[1]",#,"theta1[2]",#"theta1[3]","theta1[4]",
               "theta2[1]"#,"theta2[2]","theta2[3]" 
               ),
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
     pars=  c( "prob[1]", "prob[2]","prob[3]", "basal_p_1"),#, "basal_p_2"),
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

# 
plot(bh_fit, show_density = FALSE, ci_level = 0.95,
     pars=  c( "cov_eff1"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,
     pars=  c( "cov_eff2"),
     fill_color = "blue")

  plot(bh_fit, show_density = FALSE, ci_level = 0.95,
      pars=  c( "N_first_winter_start_log",
                "N_j_start_log",
                "N_sp_start_log",
                "N_egg_start_log",
                "N_recruit_start_log"),
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
  dplyr::mutate(time = rep(1:26, each=4),
                age = rep(3:6, length.out = nrow(.))) %>%
  filter(!time>21) # remove years without full return estimates 

# ggplot(data = pred_N_SP %>% mutate(age = factor(age))) +
#   geom_line(aes(x=time, y= mean, group = age, color = age))+
#   facet_wrap(~age, scales = "free")

# plot proportions 
# sum to compare with data 
summ_n_sp <- pred_N_SP %>%
  group_by(time) %>%
  summarise(mean = sum(mean),
            se_mean = mean(se_mean)) %>%
  cbind(obs = data_list_stan$data_stage_sp) %>%
  mutate(rowname = "sp") %>%
  filter(!time<7)
  
ggplot(data = summ_n_sp) +
  geom_point(aes(x=time, y = obs)) +
  geom_line(aes(x=time, y = mean)) +
  geom_ribbon(aes(x=time, ymin = mean-se_mean,
                  ymax = mean+se_mean)) +
  ggtitle("spawners: obs and predicted")

## recruits ====== 
pred_N_recruit <- summary(bh_fit, pars = c("N_recruit"), 
                     probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:26, each=4),
                age = rep(3:6, length.out = nrow(.))) %>%
   filter(!time>21) 

# plot proportions 
  # sum to compare with data 
summ_n_rec <- pred_N_recruit %>%
  group_by(time) %>%
  summarise(mean = sum(mean),
            se_mean = mean(se_mean)) %>% 
  cbind(obs = data_list_stan$data_stage_return)  %>%
  mutate(rowname = "recruit") %>%
  filter(!time<7) 

ggplot(data = summ_n_rec) +
  geom_point(aes(x=time, y = obs)) +
  geom_line(aes(x=time, y = mean)) +
  geom_ribbon(aes(x=time, ymin = mean-se_mean,
                  ymax = mean+se_mean))+
  ggtitle(("recruits: obs and predicted"))

## harvest ====== 
pred_N_harvest <- summary(bh_fit, pars = c("N_catch"), 
                          probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:26, each=4),
                age = rep(3:6, length.out = nrow(.))) %>% 
  filter(!time>21) 

# plt proportions 
# sum to compare with data 
summ_n_harvest <- pred_N_harvest %>%
  group_by(time) %>%
  summarise(mean = sum(mean),
            se_mean = mean(se_mean)) %>% 
  cbind(obs = data_list_stan$data_stage_harvest)  %>%
mutate(rowname = "harvest") %>% 
  filter(!time < 7)

ggplot(data = summ_n_harvest) +
  geom_point(aes(x=time, y = obs)) +
  geom_line(aes(x=time, y = mean)) +
  geom_ribbon(aes(x=time, ymin = mean-se_mean,
                  ymax = mean+se_mean))+
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
  cbind(obs = data_list_stan$data_stage_j) %>%
  mutate(rowname = "juv") #%>% 
#  filter(!time<7)
#  dplyr::mutate(obs = obs*catch_q$mean) 

ggplot(data = summ_n_j) +
  geom_point(aes(x=time, y = obs)) +
  geom_line(aes(x=time, y = mean_J_Q)) +
 # geom_line(aes(x=time, y = mean), color = "green") +
  geom_ribbon(aes(x=time, ymin = mean_J_Q-se_mean,
                  ymax = mean_J_Q+se_mean), alpha = 0.5)+
  ggtitle(("Juveniles, est and observed"))
 
## eggs =============================
pred_N_eggs_sum <- summary(bh_fit, pars = c("N_e_sum"), 
                          probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:22, each=1)) %>%
  filter(!time == 22) %>% 
  cbind(obs = data_list_stan$data_stage_j) %>% 
  filter(!time<7)  %>% 
  mutate(rowname = "eggs") %>%
  select(rowname,time,mean,obs)  
   

ggplot(data = pred_N_eggs_sum) + 
  geom_line(aes(x=time, y = mean)) +
  geom_line(aes(x=time, y = obs*100000), color = "green") + 
  # geom_ribbon(aes(x=time, ymin = mean-se_mean,
  #                 ymax = mean+se_mean))+
  ggtitle(("eggs: obs and predicted"))

# eggs before sum =======
pred_N_eggs <- summary(bh_fit, pars = c("N_e"), 
                           probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:26, each=4),
                age = rep(3:6, length.out = nrow(.))) %>% 
  
  filter(!time<6)  

ggplot(data = pred_N_eggs) + 
  geom_line(aes(x=time, y = mean)) +
  geom_ribbon(aes(x=time, ymin = mean-se_mean,
                  ymax = mean+se_mean))+
  facet_wrap(~age, scales = "free") + 
  ggtitle(("eggs: obs and predicted"))

# align all stages on one plot to look at scale. =====
all_stages<- rbind(summ_n_sp %>% select(-obs),
                   summ_n_rec%>% select(-obs),
                   summ_n_harvest%>% select(-obs),
                   summ_n_j%>% select(time, rowname,mean,se_mean)#,
                   #pred_N_eggs_sum
)

ggplot(data = all_stages) + 
  geom_line(aes(x=time, y = mean, group = rowname, color = rowname)) +
  geom_ribbon(aes(x=time, ymin = mean-se_mean,
                  ymax = mean+se_mean))+
  #  facet_wrap(~rowname, scales = "free") + 
  ggtitle(("all stages compare"))


# convert adults back to brood year
rec_brood <- pred_N_recruit %>% 
  mutate(brood =  time-age) %>% 
  group_by(brood) %>%
  summarise(mean = sum(mean)) %>% 
  mutate(rowname = "recruit")  

# juv obs 
juv_obs <- data.frame(brood = 1:21,
  mean= c(data_list_stan$data_stage_j *100), 
  rowname = "obs_j")

brood <- summary(bh_fit, pars = c("N_sp"), 
                     probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:26, each=4),
                age = rep(3:6, length.out = nrow(.))) %>%
 # filter(!time>23) %>% 
  mutate(brood =  time-age) %>% 
  group_by(brood) %>%
  summarise(mean = sum(mean)) %>% 
  mutate(rowname = "spawner") %>% 
  rbind(rec_brood#,
        # summ_n_j %>%
        #           mutate(brood = time,
        #                  mean =mean/100) %>%
        #   dplyr::select(brood, rowname,mean)
        )  %>% 
   rbind(juv_obs) %>%
  filter(!brood < 2)
          
# rename(brood = "time") %>% 
   # mutate(mean = mean /10))  

ggplot(data = brood) + 
  geom_line(aes(x=brood, y = mean, group = rowname, color = rowname)) +
   ggtitle(("all stages compare")) # +
  #facet_wrap(~rowname,scales = "free")

# Spawners by brood Year ============
age_comp = (data_list_stan$p_obs)

sp_obs_brood <- data.frame(obs = data_list_stan$data_stage_sp) %>%
  dplyr::mutate("3" = obs*age_comp[1],
                "4" = obs*age_comp[2],
                "5" = obs*age_comp[3],
                "6" = obs*age_comp[4]) %>%
  dplyr::select(2:5) %>%
  dplyr::mutate(time = 1:nrow(.)) %>%
  gather(1:4, key = "age", value = "abundance") %>% 
  dplyr::mutate(age = as.numeric(age),
                brood = time - age) %>% 
  group_by(brood) %>%
  dplyr::summarise(obs = sum(abundance))  
                
  
  brood_pred <- summary(bh_fit, pars = c("N_sp"), 
                   probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:26, each=4),
                age = rep(3:6, length.out = nrow(.))) %>%
  #filter(!time>23) %>% 
  mutate(brood =  time-age) %>% 
  group_by(brood) %>%
  summarise(pred = sum(mean)) %>% 
  mutate(rowname = "spawner") %>%
  left_join(sp_obs_brood) %>% 
  dplyr::select(-rowname) %>% 
  gather(2:3, key = "id", value = "value") %>%
  filter(!brood<2)


ggplot(data = brood_pred) + 
  geom_line(aes(x=brood, y = value, group = id, color = id)) +
  ggtitle(("all stages compare")) # +
#facet_wrap(~rowname,scales = "free")

# Add juveniles by brood Year ============
brood_year_j <- summ_n_j %>%
  select(mean, obs, time) %>% 
  rename(brood = "time",
         mean_juv = "mean",
         obs_j = "obs") %>% 
  mutate(obs_j = obs_j*100 ) %>% 
  gather(1:2, key = "id", value = "value") 

brood_pred2 <- brood_pred %>% 
  #mutate(brood = brood+1) %>% 
  rbind(brood_year_j) 


ggplot(data = brood_pred2) + 
  geom_line(aes(x=brood, y = value, group = id, color = id)) +
  ggtitle(("all stages compare")) # +

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
                                     TRUE ~ "kappa_j_survival"))

ggplot(data = kappasurvival, aes(x=time, y = mean, group = variable ,color = variable)) + 
  geom_line( ) +
  geom_ribbon(aes(x=time, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5) 

ggplot(data = kappasurvival, aes(x=time, y = mean, group = variable ,color = variable)) + 
  geom_line( ) +
  geom_ribbon(aes(x=time, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5) +
  facet_wrap(~variable, scales = "free")

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
                  ymax = mean+se_mean), alpha = 0.5) +
  facet_wrap(~variable, scales = "free")

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

