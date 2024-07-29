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

# traceplot ========
traceplot(bh_fit,pars=  c( "D_scale", "theta1[1]","theta2[1]"))#,"theta1[3]","theta1[4]",
                           #"theta2[1]","theta2[2]","theta2[3]"))

traceplot(bh_fit,pars=  c("prob[1]", "prob[2]","prob[3]", "basal_p_1", "basal_p_2"))

traceplot(bh_fit,pars=  c("log_catch_q","g"))

traceplot(bh_fit,pars=  c("N_sp_start_log"))

traceplot(bh_fit,pars=  c("N_sp"))

traceplot(bh_fit,pars=  c("N_j"))
traceplot(bh_fit,pars=  c("N_j_predicted"))

traceplot(bh_fit,pars=  c("N_j_start_log"))

# parameter plots ======== 

plot(bh_fit, show_density = TRUE, ci_level = 0.95, 
     pars=  c( "theta1[1]",#"theta1[2]","theta1[3]","theta1[4]",
               "theta1[2]",
               "theta1[3]",
               "theta2[1]",
               "theta2[2]"#,
             #  "theta2[2]"#,"theta2[2]"#,"theta2[3]" 
     ),
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
     pars=  c(  "log_c_1","log_c_2"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
     pars=  c("log_catch_q"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
     pars=  c( "g"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,
     pars=  c( "cov_eff1"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,
     pars=  c( "cov_eff2"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,
     pars=  c( "sigma_y_j"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,
      pars=  c( "N_first_winter_start_log",
                "N_j_start_log",
                "N_sp_start_log",
                "N_egg_start_log",
                "N_recruit_start_log"),
       fill_color = "blue")
 
plot(bh_fit, show_density = TRUE, ci_level = 0.89, 
     pars=  c( "theta_1_1_pp","theta_1_2_pp","theta_1_3_pp",#"theta_1_2_sim",
              # "theta_1_3_sim","theta_1_4_sim",
               #"theta1[2]","theta1[3]","theta1[4]",
               "theta_2_1_pp","theta_2_2_pp"),
     fill_color = "blue")

 
# Plot Observed vs Predicted ========
## Spawners ==========
pred_N_SP <- summary(bh_fit, pars = c("N_sp"), 
              probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:29, each=4),
                age = rep(3:6, length.out = nrow(.))) %>%
  filter(!time>21) %>% # remove years without full return estimates 
  left_join(years)  
# ggplot(data = pred_N_SP %>% mutate(age = factor(age))) +
#   geom_line(aes(x=time, y= mean, group = age, color = age))+
#   facet_wrap(~age, scales = "free")

# plot proportions 
# sum to compare with data 
summ_n_sp <- pred_N_SP %>%
  group_by(cal_year) %>%
  summarise(mean = sum(mean),
            sd = mean(sd) ) %>%
  cbind(obs = data_list_stan$data_stage_sp) %>%
  mutate(rowname = "sp")  
  
ggplot(data = summ_n_sp) +
  geom_point(aes(x=cal_year, y = obs)) +
  geom_line(aes(x=cal_year, y = mean)) +
  geom_ribbon(aes(x=cal_year, ymin = mean-sd,
                  ymax = mean+sd)) +
  ggtitle("Spawners: obs and predicted")+
  scale_x_continuous(breaks = c(2002, 2006,2010, 2015,2020)) + 
  theme_classic()

## recruits ====== 
pred_N_recruit <- summary(bh_fit, pars = c("N_recruit"), 
                     probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:29, each=4),
                age = rep(1:4, length.out = nrow(.))) %>%
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
  scale_x_continuous(breaks = c(2002, 2006,2010, 2015,2020))

## harvest ====== 
pred_N_harvest <- summary(bh_fit, pars = c("N_catch"), 
                          probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:29, each=4),
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
  dplyr::mutate(time = 1:nrow(.)) %>%
  filter(!time==22) %>%
  left_join(years)
                
# plot proportions 
# sum to compare with data 
summ_n_j <- pred_N_j %>%
 dplyr::mutate(mean_J_Q = mean*catch_q$mean,
               se_mean = se_mean*catch_q$mean) %>% 
  cbind(obs = data_list_stan$data_stage_j) %>%
  mutate(rowname = "juv") #%>% 
#  filter(!time<7)
#  dplyr::mutate(obs = obs*catch_q$mean) 

ggplot(data = summ_n_j %>% filter(!cal_year ==2002)) +
  geom_line(aes(x=cal_year, y = mean_J_Q)) + 
  # geom_line(aes(x=time, y = mean), color = "green") +
  # geom_ribbon(aes(x=time, ymin = mean_J_Q-se_mean,
  #                 ymax = mean_J_Q+se_mean), alpha = 0.5)+
  ggtitle(("Juveniles, est "))

ggplot(data = summ_n_j) +
  geom_point(aes(x=cal_year, y = obs)) +
  geom_line(aes(x=cal_year, y = mean_J_Q)) +
 # geom_line(aes(x=time, y = mean), color = "green") +
  geom_ribbon(aes(x=cal_year, ymin = mean_J_Q-se_mean,
                  ymax = mean_J_Q+se_mean), alpha = 0.5)+
  ggtitle(("Juveniles, est and observed"))

## eggs =============================
pred_N_eggs_sum <- summary(bh_fit, pars = c("N_e_sum"), 
                          probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:23, each=1)) %>%
  mutate(rowname = "eggs")  
   
ggplot(data = pred_N_eggs_sum) + 
 geom_line(aes(x=time, y = mean)) #+
 # geom_line(aes(x=time, y = mean*10), color = "green") +  
  # geom_line(aes(x=time, y = pred_j), color = "blue")  

ggplot(data = pred_N_eggs_sum %>% filter(!time < 7)) + 
  geom_line(aes(x=time, y = mean)) 

# align all stages on one plot to look at scale. =====
summ_n_eggs <- summary(bh_fit, pars = c("N_e_sum"), 
                           probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:23, each=1)) %>%
  filter(!time<7 & !time>15)  %>% 
  dplyr::mutate(rowname = "eggs",
         mean = as.numeric(scale(mean)))  %>%
  dplyr::select(rowname, mean, se_mean,time)
  
comp_j_e<- rbind(#summ_n_sp %>% select(-obs),
                   #summ_n_rec%>% select(-obs),
                   #summ_n_harvest%>% select(-obs),
                   summ_n_eggs,
                   summ_n_j %>% filter(!time<7 & !time>15) %>% 
                     select(time, rowname,mean,se_mean) %>% 
                     mutate(mean = as.numeric(scale(mean))
                            #time = time-1
                            ))   

ggplot(data = comp_j_e) + 
  geom_line(aes(x=time, y = mean, group = rowname, color = rowname)) +
  # geom_ribbon(aes(x=time, ymin = mean-se_mean,
  #                 ymax = mean+se_mean))+
  #  facet_wrap(~rowname, scales = "free") + 
  ggtitle(("juv eggs compare")) 
 
all_stages_scale <- all_stages %>% 
  group_by(rowname) %>%
  mutate(mean = as.numeric(scale(mean)))

ggplot(data = all_stages_scale) + 
  geom_line(aes(x=time, y = mean, group = rowname, color = rowname)) +
  # geom_ribbon(aes(x=time, ymin = mean-se_mean,
  #                 ymax = mean+se_mean))+
  #  facet_wrap(~rowname, scales = "free") + 
  ggtitle(("all stages compare - mean scaled")) +
  ylab("mean scaled")


# brood year convert ==============
rec_brood <- pred_N_recruit %>% 
  mutate(brood =  time-age-1) %>% 
  group_by(brood) %>%
  summarise(mean = sum(mean)) %>% 
  mutate(rowname = "recruit")  

# juv obs 
juv_obs <- data.frame(brood = 1:21,
  mean= c(data_list_stan$data_stage_j), 
  rowname = "obs_j")

brood <- summary(bh_fit, pars = c("N_sp"), 
                     probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:27, each=4),
                age = rep(1:4, length.out = nrow(.))) %>%
 # filter(!time>23) %>% 
  mutate(brood =  time-age-1) %>% 
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
  filter(!brood < 2) %>% 
  group_by(rowname) %>%
  mutate(mean = as.numeric(scale(mean)))
          
# rename(brood = "time") %>% 
   # mutate(mean = mean /10))  

ggplot(data = brood) + 
  geom_line(aes(x=brood, y = mean, group = rowname, color = rowname)) +
   ggtitle(("all stages compare - observed juveniles")) +
  ylab("mean scaled")
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
  dplyr::mutate(time = rep(1:29, each=4),
                age = rep(3:6, length.out = nrow(.))) %>%
    #filter(!time>23) %>% 
  dplyr::mutate(brood =  time-age) %>% 
  #  age = rep(1:4, length.out = nrow(.))) %>%
  # #filter(!time>23) %>% 
  # mutate(brood =  time-age-1) %>% 
  group_by(brood) %>%
  dplyr::summarise(pred = sum(mean)) %>% 
  mutate(rowname = "spawner") %>%
  left_join(sp_obs_brood) %>% 
  dplyr::select(-rowname) %>% 
  gather(2:3, key = "id", value = "value")  

ggplot(data = brood_pred) + 
  geom_line(aes(x=brood, y = value, group = id, color = id)) +
  ggtitle(("Compare spawners by brood year")) # +
#facet_wrap(~rowname,scales = "free")

# recruits by brood Year ============
age_comp = (data_list_stan$p_obs)

recruit_obs_brood <- data.frame(obs = data_list_stan$data_stage_return) %>%
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

brood_pred <- summary(bh_fit, pars = c("N_recruit"), 
                      probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:29, each=4),
                age = rep(3:6, length.out = nrow(.))) %>%
  #filter(!time>23) %>% 
  dplyr::mutate(brood =  time-age) %>% 
  #  age = rep(1:4, length.out = nrow(.))) %>%
  # #filter(!time>23) %>% 
  # mutate(brood =  time-age-1) %>% 
  group_by(brood) %>%
  summarise(pred = sum(mean)) %>% 
  mutate(rowname = "recruit") %>%
  left_join(recruit_obs_brood) %>% 
  dplyr::select(-rowname) %>% 
  gather(2:3, key = "id", value = "value")  

ggplot(data = brood_pred) + 
  geom_line(aes(x=brood, y = value, group = id, color = id)) +
  ggtitle(("Compare recruits by brood year"))  

# catch by brood Year ============
age_comp = (data_list_stan$p_obs)

harvest_obs_brood <- data.frame(obs = data_list_stan$data_stage_harvest) %>%
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

brood_pred <- summary(bh_fit, pars = c("N_catch"), 
                      probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:29, each=4),
                # age = rep(1:4, length.out = nrow(.)),
                age = rep(3:6, length.out = nrow(.)),
                ) %>%
  #filter(!time>23) %>% 
  mutate(brood =  time-age) %>% 
  group_by(brood) %>%
  summarise(pred = sum(mean)) %>% 
  mutate(rowname = "harvest") %>%
  left_join(harvest_obs_brood) %>% 
  dplyr::select(-rowname) %>% 
  gather(2:3, key = "id", value = "value") %>%
  filter(!brood<2)

ggplot(data = brood_pred) + 
  geom_line(aes(x=brood, y = value, group = id, color = id)) +
  ggtitle(("Comapre harvest by brood year"))  

# Add juveniles by brood Year ============
brood_year_j <- summ_n_j %>%
  select(mean, obs, time) %>% 
  rename(brood = "time",
         mean_juv = "mean",
         obs_j = "obs") %>% 
  mutate(obs_j = obs_j*100,
         brood = brood-1) %>% 
  gather(1:2, key = "id", value = "value") 

brood_pred2 <- brood_pred %>% 
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
  dplyr::mutate(time = rep(1:22, length.out = nrow(.)), 
                variable = case_when(grepl("kappa_marine_survival",rowname) ~ "kappa_marine_survival",
                                     TRUE ~ "kappa_j_survival")) %>% 
  left_join(years)# %>% 
  #filter(!time<5 & !time>20)

ggplot(data = kappasurvival%>%   filter(!time<2),aes(x=cal_year, y = mean, group = variable ,color = variable)) + 
  geom_line( ) +
  geom_ribbon(aes(x=cal_year, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5) + 
  scale_x_continuous(breaks = c(2002,2006,2010, 2015,2020, 2022))

ggplot(data = kappasurvival,# %>%   filter(!time<2),
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
         filter(!time<8 & !time>16), 
       aes(x=cal_year, y = mean, group = variable ,color = variable)) + 
  geom_line( ) +
  geom_ribbon(aes(x=cal_year, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5) +
  facet_wrap(~variable, scales = "free") + 
  scale_x_continuous(breaks = c(2002,2006,2010, 2015,2020, 2022)) +
  theme_classic() + 
  xlab("Calendar Year") + 
  ylab("Survival Rate")



# calculate rolling correlation in productivity....
# make a list of 5 year chunks .... 
n <- 5
kappasurvival_group <- kappasurvival %>% 
  dplyr::select(9:11, 2) %>% 
  spread(variable, mean) %>% 
  filter(!time %in% c(21,22)) %>% 
  mutate(id = rep(1:n, times=1, each=4)) 

corr <- list()

for (i in 1:n) {
  temp <- kappasurvival_group %>% 
    filter(id == i)
  
  corr[[i]]<- cor.test(temp$kappa_j_survival,temp$kappa_marine_survival)
  
}

corr
 
# plot estimated productivity ======
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

# calculate correlation in productivity ...
productivity_split <- productivity %>% 
  select(time, variable,mean) %>% 
  spread(variable, mean)

cor.test(productivity_split$p_1,productivity_split$p_2)

# calculate rolling correlation in productivity....
# make a list of 5 year chunks .... 
# could also look at warm vs cold year productivity 
n <- 5
productivity_group <- productivity %>% 
  select(time, variable,mean) %>% 
  spread(variable, mean) %>% 
   filter(!time %in% c(23,1,2)) %>% 
  mutate(id = rep(1:n, times=1, each=4)) 
 
corr <- list()

for (i in 1:n) {
  temp <- productivity_group %>% 
    filter(id == i)
  
  corr[[i]]<- cor.test(temp$p_1,temp$p_2)
  
}

corr


# productivity_late <- productivity %>% 
#   select(time, variable,mean) %>% 
#   spread(variable, mean) %>% 
#   filter(time>9)
# 
# cor.test(productivity_early$p_1,productivity_early$p_2)
# cor.test(productivity_late$p_1,productivity_late$p_2)


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


# Plot covariates with respective salmon population ======
brood_year_recruits <- summary(bh_fit, pars = c("N_recruit"), 
                          probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:29, each=4),
                age = rep(3:6, length.out = nrow(.))) %>%
  # filter(!time>21) %>%
  left_join(years)  %>%
  # group_by(cal_year) %>%
  # summarise(mean = sum(mean),
  #           se_mean = mean((sd))) %>% 
  mutate(rowname = "recruit",
         brood_year =  cal_year-age) %>% 
  group_by(brood_year) %>%
  summarise(mean = sum(mean)) %>% 
  mutate(mean = as.numeric(scale(mean))) %>% 
  mutate(rowname = "recruit")  
 
## Chum_hatchery ============ 
cov_B_forplot <- read_csv("data/processed_covariates/stage_b_all.csv") %>%
  rename(cov_year = "brood_year") %>%
  filter(cov_year >= year_min-1, 
         cov_year <= year_max_brood+1) %>% 
  dplyr::mutate(SST_CDD_GOA = as.numeric(scale(SST_CDD_GOA)),
                Chum_hatchery= as.numeric(scale(Chum_hatchery)),
                Pink_hatchery= as.numeric(scale(Pink_hatchery)),
                brood_year = cov_year-2 # this is the effect year 
                # the temp in 2001 is gonna effect fish from brood year 1999
                #yukon_mean_discharge_summer= as.numeric(scale(yukon_mean_discharge_summer))
  )  %>% 
  dplyr::select(brood_year, cov_year,Chum_hatchery)

brood_year_recruits_chum <- brood_year_recruits %>% 
                              left_join(cov_B_forplot) 

 ggplot(data =brood_year_recruits_chum) +
   geom_line(aes(x=brood_year, y = mean)) + 
   geom_line(aes(x=brood_year, y = Chum_hatchery), color = "purple") +
   labs(caption = "purple is the covariate")
  
 
 ## Aleutians temp =======
 cov_B_forplot <- read_csv("data/processed_covariates/stage_b_all.csv") %>%
   rename(cov_year = "brood_year") %>%
   filter(cov_year >= year_min-1, 
          cov_year <= year_max_brood+1) %>% 
   dplyr::mutate(SST_CDD_Aleut = as.numeric(scale(SST_CDD_Aleut)),
                 Chum_hatchery= as.numeric(scale(Chum_hatchery)),
                 Pink_hatchery= as.numeric(scale(Pink_hatchery)),
                 brood_year = cov_year-2 # this is the effect year 
                 # the temp in 2001 is gonna effect fish from brood year 1999
                 #yukon_mean_discharge_summer= as.numeric(scale(yukon_mean_discharge_summer))
   )  %>% 
   dplyr::select(brood_year, cov_year,SST_CDD_Aleut)
 
 brood_year_recruits_goa <- brood_year_recruits %>% 
   left_join(cov_B_forplot) 
 
 ggplot(data =brood_year_recruits_goa) +
   geom_line(aes(x=brood_year, y = mean)) + 
   geom_line(aes(x=brood_year, y = SST_CDD_Aleut), color = "purple") +
   labs(caption = "purple is the covariate")
 


# Covariates at time t+x with observed pred recruits 




# covariate effect2 and observed/ pred spawners 



# Covariates at time t+x with observed pred spawners 



