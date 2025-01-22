library(tidyverse)
library(tidybayes)
library(here)
library(rstan)
library(bayesplot)
library(rstanarm)
 
# load model ==============
bh_fit<- read_rds("output/stan_fit_DATA.RDS")
  
# bh_fit <- read_rds("output/stan_fit_DATA_forAFS.RDS")
# bh_fit <- read_rds("output/stan_fit_DATA_nocovar.RDS")

# year DF for joins ==================
years <-read_csv("data/processed_data/yukon_fall_spawners.csv") %>%
  filter(cal_year >= year_min) %>%
  dplyr::select(cal_year) %>%
  dplyr::mutate(time = c(1:nrow(.)))

# traceplot ========
traceplot(bh_fit,pars=  c( "theta1[1]" ,"theta1[2]" ,"theta1[3]" ,"theta1[4]" ,
                           "theta2[1]","theta2[2]","theta2[3]","theta2[4]"))

traceplot(bh_fit,pars=  c("D_scale"))

# traceplot(bh_fit,pars=  c( "log_c_1","log_c_2"))

traceplot(bh_fit,pars=  c( "log_catch_q" ))

traceplot(bh_fit,pars=  c(  "Dir_alpha"))

# traceplot(bh_fit,pars=  c("prob[1]", "prob[2]","prob[3]"))

# traceplot(bh_fit,pars=  c("p_1","p_2"))

traceplot(bh_fit,pars=  c("sigma_sp"))
traceplot(bh_fit,pars=  c("sigma_juv"))
traceplot(bh_fit,pars=  c("sigma_catch"))

traceplot(bh_fit,pars=  c("log_S"))

traceplot(bh_fit,pars=  c("pi"))

traceplot(bh_fit,pars=  c("basal_p_1", "basal_p_2"))
 
# parameter plots ======== 
plot(bh_fit, show_density = TRUE, ci_level = 0.95, 
     pars=  c( "theta1[1]","theta1[2]","theta1[3]","theta1[4]", 
               "theta2[1]","theta2[2]","theta2[3]","theta2[4]" 
     ),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "log_ricker_alpha" ),
     fill_color = "blue")


plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "log_ricker_beta" ),
     fill_color = "blue")


plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "log_F"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "log_S"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,
     pars=  c(  "pi"),
     fill_color = "blue") 

plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "basal_p_2"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "basal_p_1"),
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
     pars=  c(  "log_sigma_y_j"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,
     pars=  c(  "log_sigma_catch"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,
     pars=  c(  "log_sigma_sp"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,
     pars=  c(  "sigma_rec"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,
     pars=  c(  "sigma_juv"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,
     pars=  c(  "sigma_catch"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,
     pars=  c(  "sigma_sp"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,
     pars=  c(  "log_sigma_return"),
     fill_color = "blue")
 
plot(bh_fit, show_density = FALSE, ci_level = 0.95,
     pars=  c(  "basal_p_1", "basal_p_2" ),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,
     pars=  c(  "N_j_pp"  ),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,
     pars=  c(  "D_scale"),
     fill_color = "blue") 
 
plot(bh_fit, show_density = TRUE, ci_level = 0.5, 
     pars=  c( "theta_1_1_pp","theta_1_2_pp","theta_1_3_pp","theta_1_4_pp",
               "theta_2_1_pp","theta_2_2_pp","theta_2_3_pp","theta_2_4_pp"),
     fill_color = "blue")

# Plot Observed vs Predicted ========
## Spawners ==========
pred_N_SP <- summary(bh_fit, pars = c("N_sp"), 
              probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:26, each=4),
                age = rep(3:6, length.out = nrow(.))) %>%
  # filter(!time>20) %>% # remove years without full return estimates 
  left_join(years)   
 
# sum to compare with data 
summ_n_sp <- pred_N_SP %>%
  group_by(cal_year) %>%
  summarise(mean = sum(mean),
            sd = mean(sd)) %>%
  left_join(data.frame(cal_year = c(data_list_stan$years_data_sp),
                       obs = c(data_list_stan$data_stage_sp))) %>%
  dplyr::mutate(rowname = "sp")  

ggplot(data = summ_n_sp) +
  geom_point(aes(x=cal_year, y = obs)) +
  geom_line(aes(x=cal_year, y = mean)) + 
  ggtitle("Spawners: obs and predicted")+
  scale_x_continuous(breaks = c(2002, 2006,2010, 2015,2020)) + 
  theme_classic()
 
## recruit ==========
# pred_N_recruit <- summary(bh_fit, pars = c("N_recruit"), 
#                      probs = c(0.1, 0.9))$summary %>%
#   data.frame() %>%
#   rownames_to_column()  %>%
#   # dplyr::mutate(time = 1:17) %>% 
#   dplyr::mutate(time = rep(1:26, each=4),
#               age = rep(3:6, length.out = nrow(.))) %>%
#   # filter(!time>20) %>% # remove years without full return estimates 
#   left_join(years)   
# 
# # sum to compare with data 
# summ_n_rec <- pred_N_recruit %>%
#   group_by(cal_year) %>%
#   summarise(mean = sum(mean),
#             sd = mean(sd)) %>%
#   left_join(data.frame(cal_year = c(data_list_stan$years_data_sp),
#                        obs = c(data_list_stan$data_stage_return))) %>%
#   dplyr::mutate(rowname = "rec")   
# 
# ggplot(data = summ_n_rec) +
#   geom_point(aes(x=cal_year, y = obs)) +
#   geom_line(aes(x=cal_year, y = mean)) +
#   # geom_ribbon(aes(x=cal_year, ymin = mean-sd,
#   #                 ymax = mean+sd)) +
#   ggtitle("recruits: obs and predicted")+
#   scale_x_continuous(breaks = c(2002, 2006,2010, 2015,2020)) + 
#   theme_classic()

## recruits by age no data ====== 
# pred_N_recruit <- summary(bh_fit, pars = c("N_recruit"),
#                      probs = c(0.1, 0.9))$summary %>%
#   data.frame() %>%
#   rownames_to_column()  %>%
#   dplyr::mutate(time = rep(1:27, each=4),
#                 age = rep(1:4, length.out = nrow(.))) %>%
#  
#   left_join(years)  
#  
# ggplot(data = pred_N_recruit) +
#   geom_line(aes(x=cal_year, y = mean, group = age, color = age)) +
#   geom_ribbon(aes(x=cal_year, ymin = mean-se_mean,
#                   ymax = mean+se_mean))+
#   ggtitle(("recruits: obs and predicted"))+
#   scale_x_continuous(breaks = c(2002, 2006,2010, 2015,2020))

## brood year return ====== 
pred_N_brood_year_return <- summary(bh_fit, pars = c("N_brood_year_return"),
                          probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = (1:20)) %>%
  left_join(years)%>%
  left_join(data.frame(cal_year = c(data_list_stan$years_data_return),
                       obs = data_list_stan$data_stage_return)) %>%
  mutate(rowname = "recruit")

ggplot(data = pred_N_brood_year_return) +
  geom_line(aes(x=cal_year, y = mean)) +
  geom_point(aes(x=cal_year, y = obs)) +
  geom_ribbon(aes(x=cal_year, ymin = mean-se_mean,
                  ymax = mean+se_mean))+
  ggtitle(("brood year return"))

## harvest ========= 
pred_N_harvest <- summary(bh_fit, pars = c("N_catch"), 
                          probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:26, each=4),
                age = rep(3:6, length.out = nrow(.))) %>%
  left_join(years) %>%
  group_by(cal_year) %>%
  summarise(mean = sum(mean),
            se_mean = mean(se_mean)) %>% 
  dplyr::left_join( data.frame(cal_year = c(data_list_stan$years_data_sp),
                       obs = c(data_list_stan$data_stage_harvest)), by ="cal_year") %>%
  dplyr::mutate(rowname = "harvest")  

ggplot(data = pred_N_harvest) +
  geom_point(aes(x=cal_year, y = obs)) +
  geom_line(aes(x=cal_year, y = mean)) +
  geom_ribbon(aes(x=cal_year, ymin = mean-se_mean,
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
  # filter(!time==22) %>%
  left_join(years)
                
# plot proportions 
# sum to compare with data 
summ_n_j <- pred_N_j %>%
 dplyr::mutate(mean_J_Q = mean*catch_q$mean,
               se_mean = se_mean*catch_q$mean) %>% 
  left_join(data.frame(cal_year = c(data_list_stan$years_data_juv ),
                       obs = c(data_list_stan$data_stage_j))) %>%
  mutate(rowname = "juv") 

ggplot(data = summ_n_j) +
  geom_point(aes(x=cal_year, y = obs)) +
  geom_line(aes(x=cal_year, y = mean_J_Q)) +
 # geom_line(aes(x=time, y = mean), color = "green") +
  geom_ribbon(aes(x=cal_year, ymin = mean_J_Q-se_mean,
                  ymax = mean_J_Q+se_mean), alpha = 0.5)+
  ggtitle(("Juveniles, est and observed"))
 
# plot age comp through time =================
age_comp_dat <- data.frame(yukon_fall_obs_agecomp) %>% 
    dplyr::mutate(time = 1:21) %>% 
    left_join(years) %>%
    gather(1:4, key = "age", value = "obs") %>%
    dplyr::mutate(age = case_when(age == "abund_0.3" ~ 3,
                                  age == "abund_0.4" ~ 4,
                                  age == "abund_0.5" ~ 5,
                                  age == "abund_0.6" ~ 6))

age_comp_Q <- summary(bh_fit, pars = c("q"), 
                       probs = c(0.1, 0.9))$summary %>% 
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:21, each=4),
                age = rep(3:6, length.out = nrow(.))) %>%
  left_join(years) %>%
  left_join( age_comp_dat) %>%
  rename(pred = "mean") %>%
  dplyr::select(time,age,pred,obs) %>%
  gather(3:4, key = "id", value = "value")

ggplot(data= age_comp_Q) +
  geom_line(aes(x=time, y = value, group = id, color = id)) +
  facet_wrap(~age, scales = "free") +
  theme_classic() + 
  ylab("Proportion")
  
# align all stages on one plot to look at scale =====
# summ_n_eggs <- summary(bh_fit, pars = c("N_e_sum"), 
#                            probs = c(0.1, 0.9))$summary %>%
#   data.frame() %>%
#   rownames_to_column()  %>%
#   dplyr::mutate(time = rep(1:23, each=1)) %>%
#   filter(!time<7 & !time>15)  %>% 
#   dplyr::mutate(rowname = "eggs",
#          mean = as.numeric(scale(mean)))  %>%
#   dplyr::select(rowname, mean, se_mean,time)
#   
# comp_j_e<- rbind(#summ_n_sp %>% select(-obs),
#                    #summ_n_rec%>% select(-obs),
#                    #summ_n_harvest%>% select(-obs),
#                    summ_n_eggs,
#                    summ_n_j %>% filter(!time<7 & !time>15) %>% 
#                      select(time, rowname,mean,se_mean) %>% 
#                      mutate(mean = as.numeric(scale(mean))
#                             #time = time-1
#                             ))   
# 
# ggplot(data = comp_j_e) + 
#   geom_line(aes(x=time, y = mean, group = rowname, color = rowname)) +
#   # geom_ribbon(aes(x=time, ymin = mean-se_mean,
#   #                 ymax = mean+se_mean))+
#   #  facet_wrap(~rowname, scales = "free") + 
#   ggtitle(("juv eggs compare")) 
#  
# all_stages_scale <- all_stages %>% 
#   group_by(rowname) %>%
#   mutate(mean = as.numeric(scale(mean)))
# 
# ggplot(data = all_stages_scale) + 
#   geom_line(aes(x=time, y = mean, group = rowname, color = rowname)) +
#   # geom_ribbon(aes(x=time, ymin = mean-se_mean,
#   #                 ymax = mean+se_mean))+
#   #  facet_wrap(~rowname, scales = "free") + 
#   ggtitle(("all stages compare - mean scaled")) +
#   ylab("mean scaled")
# 

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
         time = 1:nrow(.)) #%>% 
#  filter(!time> 21 & !time<5)

ggplot(data = fishing) + 
  geom_line(aes(x=time, y = mean)) + 
  ylab("Instantaneous fishing mortality")

# Plot selectivity ======
S <- summary(bh_fit, pars = c("S"),
                   probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column() %>% 
  mutate(rowname = case_when(rowname == "S[1]" ~ "Age_3",
                             rowname == "S[2]" ~ "Age_4",
                             rowname == "S[3]" ~ "Age_5",
                             rowname == "S[4]" ~ "Age_6"
                             ))

ggplot(data = S,aes(x=rowname, y = mean)) +
  geom_point( ) +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width = 0.1) + 
  ylab("Selectivity")+
  theme_classic() + 
  xlab(" ")

#   ricker alpha  ======
# literature has alpha less than 3.7
  summary(bh_fit, pars = c("log_ricker_alpha"),
                 probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()

#   ricker beta  ======
# literature has beta at 6^e-7
# currently its like 0.003 though 
summary(bh_fit, pars = c("log_ricker_beta"),
        probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()

# Plot theta ======
theta <- summary(bh_fit, pars = c("theta1[1]","theta1[2]","theta1[3]","theta1[4]",
                                  "theta2[1]","theta2[2]", "theta2[3]","theta2[4]"),
                   probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()

ggplot(data = theta,aes(x=rowname, y = mean)) +
  geom_point( ) +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width = 0.1) 

# plot  estimated kappas survival ======
kappasurvival <- summary(bh_fit, pars = c("kappa_marine_survival", "kappa_j_survival"), 
                    probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>% 
  dplyr::mutate(time = rep(1:21, length.out = nrow(.)), 
                variable = case_when(grepl("kappa_marine_survival",rowname) ~ "kappa_marine_survival",
                                     grepl("kappa_j_survival",rowname) ~ "kappa_j_survival")) %>% 
  left_join(years)# %>% 
  #filter(!time<5 & !time>20)

ggplot(data = kappasurvival,
       aes(x=cal_year, y = mean, group = variable ,color = variable)) + 
  geom_line( ) +
  geom_ribbon(aes(x=cal_year, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5) + 
  scale_x_continuous(breaks = c(2002,2006,2010, 2015,2020, 2022)) +
  scale_y_continuous(limits = c(0,1))

ggplot(data = kappasurvival, 
       aes(x=cal_year, y = mean, group = variable ,color = variable)) + 
  geom_line( ) +
  geom_ribbon(aes(x=cal_year, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5) +
  facet_wrap(~variable, scales = "free") + 
  scale_x_continuous(breaks = c(2002,2006,2010, 2015,2020, 2022)) +
  theme_classic() + 
  xlab("Brood Year") + 
  ylab("Survival Rate")

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
                  ymax = mean+se_mean), alpha = 0.5)+
  scale_y_continuous(limits = c(0,1))

                    productivity %>%
                              group_by(variable) %>% 
                              dplyr::mutate(mean = as.numeric(scale(mean))) %>% 
                            ggplot(aes(x=time, y = mean, group = variable ,color = variable)) +
                            geom_line( )  
                           
# plot sigma  ======
sigma <- summary(bh_fit, pars = c(
                                  "sigma_catch",
                                  "sigma_sp"),
                    probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column() 

ggplot(data = sigma,aes(x= rowname, y = mean)) +
  geom_point( ) +
  theme_classic() +
  geom_errorbar(aes(ymin = mean - sd, ymax=mean+sd),width = 0.1)

# look at n_eff ==========

all_summ <- summary(bh_fit)$summary %>%
  data.frame() %>%
  rownames_to_column()  

low_n_eff <- all_summ %>% 
  filter(n_eff <20)

low_n_eff

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



