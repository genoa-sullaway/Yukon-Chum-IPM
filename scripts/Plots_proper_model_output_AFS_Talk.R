library(tidyverse)
library(tidybayes)
library(here)
library(rstan)
library(bayesplot)
library(rstanarm)

library(bayestestR)

# load model ==============
bh_fit<- read_rds("output/stan_fit_DATA.RDS")
# bh_fit <- read_rds("output/stan_fit_DATA_nocovar.RDS")

# get this from the model call script: year_min = 2001
years <-read_csv("data/processed_data/yukon_fall_spawners.csv") %>%
  filter(cal_year >= year_min) %>%
  dplyr::select(cal_year) %>%
  dplyr::mutate(time = c(1:nrow(.)))

# Spawners PP ====== 
pred_N_SP_sim <- summary(bh_fit, pars = c("N_sp_pp"), 
                     probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = 1:nrow(.),
                 mean=  (mean),
                 ci_10 =  (X10.),
                 ci_90 =  (X90.)) %>%
  left_join(years) %>%
  cbind(obs = data_list_stan$data_stage_sp) %>%
  dplyr::mutate(rowname = "sp")  

ggplot(data = pred_N_SP_sim) +
  geom_point(aes(x=cal_year, y = obs/1000000),color = "purple") +
  geom_line(aes(x=cal_year, y = obs/1000000), color = "purple") +
  geom_line(aes(x=cal_year, y = mean/1000000)) +
  geom_ribbon(aes(x=cal_year, ymin = ci_10/1000000,
                  ymax = ci_90/1000000), alpha = 0.5) +
  # ggtitle("Spawners")+
  scale_x_continuous(breaks = c(2002, 2005,2010, 2015,2020)) +
  theme_classic() + 
  xlab("Calendar Year") + 
  ylab("Estimated Spawners\n(Millions)")

# recruits ====== 
pred_N_R_sim <- summary(bh_fit, pars = c("N_rec_pp"), 
                         probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = 1:nrow(.),
                mean=  (mean),
                ci_10 =  (X10.),
                ci_90 =  (X90.)
  ) %>%
  left_join(years) %>%
  cbind(obs = data_list_stan$data_stage_return) %>%
  dplyr::mutate(rowname = "recruit")  

ggplot(data = pred_N_R_sim) +
  geom_point(aes(x=cal_year, y = obs/1000000),color = "purple") +
  geom_line(aes(x=cal_year, y = obs/1000000), color = "purple") +
  geom_line(aes(x=cal_year, y = mean/1000000)) +
  geom_ribbon(aes(x=cal_year, ymin = ci_10/1000000,
                  ymax = ci_90/1000000), alpha = 0.5) +
  # ggtitle("Spawners")+
  scale_x_continuous(breaks = c(2002, 2005,2010, 2015,2020)) +
  theme_classic() + 
  xlab("Calendar Year") + 
  ylab("Estimated Return\n(Millions)")

# harvest ====== 
pred_N_H_sim <- summary(bh_fit, pars = c("N_catch_pp"), 
                        probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = 1:nrow(.),
                mean=  (mean),
                # ci_10 =  (X10.),
                # ci_90 =  (X90.)
                ci_10 =  (X10.),
                ci_90 =  (X90.)
  ) %>%
  left_join(years) %>%
  cbind(obs = data_list_stan$data_stage_harvest) %>%
  dplyr::mutate(rowname = "harvest")  

ggplot(data = pred_N_H_sim) +
  geom_point(aes(x=cal_year, y = obs/1000000),color = "purple") +
  # geom_line(aes(x=cal_year, y = obs/1000000), color = "purple") +
  geom_line(aes(x=cal_year, y = mean/1000000)) +
  geom_ribbon(aes(x=cal_year, ymin = ci_10/1000000,
                  ymax = ci_90/1000000), alpha = 0.5) +
  # ggtitle("Spawners")+
  scale_x_continuous(breaks = c(2002, 2005,2010, 2015,2020)) +
  theme_classic() + 
  xlab("Calendar Year") + 
  ylab("Estimated Catch\n(Millions)")

# return PP ====== 
pred_N_return_pp <- summary(bh_fit, pars = c("N_return_pp"), 
                      probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = 1:nrow(.)) %>%
  left_join(years) %>%
  left_join(data.frame(cal_year = c(data_list_stan$years_data_return),
                       obs = data_list_stan$data_stage_return)) %>%
  dplyr::mutate(mean = (mean),
                ci_10=(X10.),
                ci_90=(X90.)) 

ggplot(data = pred_N_return_pp) +
  geom_point(aes(x=cal_year, y = log(obs)),color = "purple") +
  geom_line(aes(x=cal_year, y = log(obs)), color = "purple") +
  geom_line(aes(x=cal_year, y = mean )) +
  geom_ribbon(aes(x=cal_year, ymin = ci_10 ,
                  ymax = ci_90 ), alpha = 0.5) +
  scale_x_continuous(breaks = c(2002, 2005,2010, 2015,2020)) +
  theme_classic() + 
  xlab("Brood Year") + 
  ylab("Estimated Log Return Abundance")

# 
# ggplot(data = pred_N_return_pp) +
#   geom_point(aes(x=cal_year, y = (obs)),color = "purple") +
#   # geom_line(aes(x=cal_year, y = obs/1000000), color = "purple") +
#   geom_line(aes(x=cal_year, y = exp(mean) )) +
#   geom_ribbon(aes(x=cal_year, ymin = exp(ci_10),
#                   ymax = exp(ci_90)), alpha = 0.5) +
#   scale_x_continuous(breaks = c(2002, 2005,2010, 2015,2020)) +
#   theme_classic() + 
#   xlab("Brood Year") + 
#   ylab("Estimated Log Return Abundance")

# ## try other way =====
# return_df <- as.data.frame(bh_fit, pars = c("N_return_pp")) %>% 
#   mutate(draw = 1:nrow(.)) %>%
#   gather(1:17, key = "time", value = "abundance")  %>%
#   group_by(time) %>% 
#   dplyr::summarise(mean = mean(abundance),
#                    ci_80_low = as.numeric(ci(abundance, method = "HDI", ci = 0.80)$CI_low),
#                    ci_80_high = as.numeric(ci(abundance, method = "HDI", ci = 0.80)$CI_high),
#                    ci_95_low = as.numeric(ci(abundance, method = "HDI", ci = 0.95)$CI_low),
#                    ci_95_high = as.numeric(ci(abundance, method = "HDI", ci = 0.95)$CI_high)) %>%
#   separate(time, into =c("del", "time", "del2"), sep = c(-3,-1)) %>%
#   arrange(time) %>%
#   mutate(time = 1:nrow(.))
# 
# ggplot(data = return_df) +
#   # geom_point(aes(x=cal_year, y = obs/1000000),color = "purple") +
#   # geom_line(aes(x=cal_year, y = obs/1000000), color = "purple") +
#   geom_line(aes(x=time, y = mean )) +
#   geom_ribbon(aes(x=time, ymin = ci_95_low ,
#                   ymax = ci_95_high ), alpha = 0.5) +
#   scale_x_continuous(breaks = c(2002, 2005,2010, 2015,2020)) +
#   theme_classic() + 
#   xlab("Brood Year") + 
#   ylab("Estimated Return\n(Millions)")
#  
# 


# Juveniles PP ====== 
# multiply by catch q to fit observations

pred_N_jpp <- summary(bh_fit, pars = c("N_j_pp"), 
                    probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = 1:nrow(.)) %>%
  left_join(years) %>%
  # cbind(obs = data_list_stan$data_stage_j) %>% 
  left_join(data.frame(cal_year = c(data_list_stan$years_data_juv ),
                       obs = c(data_list_stan$data_stage_j))) %>%
  dplyr::mutate(mean = (mean),
         ci_10=(X10.),
         ci_90=(X90.))

ggplot(data = pred_N_jpp) +
  geom_point(aes(x=cal_year, y = log(obs)),color = "purple") +
  geom_line(aes(x=cal_year, y = log(obs)), color = "purple") +
  geom_line(aes(x=cal_year, y = mean)) +
  geom_ribbon(aes(x=cal_year, ymin =ci_10,
                  ymax = ci_90), alpha = 0.5) +
  scale_x_continuous(breaks = c(2002, 2005,2010, 2015,2020)) +
  theme_classic() + 
  xlab("Brood Year") + 
  ylab("Estimated Log Juvenile Abundance")
 
# juv return together pred ======
pred_juv_rec <- left_join(pred_N_jpp %>% 
                            dplyr::select(cal_year, mean) %>%
                            rename(juv = "mean"),
                          pred_N_return_pp %>% 
                            dplyr::select(cal_year, mean) %>%
                            rename(rec = "mean")) %>%
  gather(2:3, key = "id", value = "value") %>%
  group_by(id) %>%
  mutate(value = as.numeric(scale(value)))

ggplot(data = pred_juv_rec,aes(x=cal_year, y = value, group = id, color = id)) +
  geom_point() +
  geom_line()+
  ylab("Predicted, Mean-Scaled Abundance") +
  theme_classic() + 
  xlab("Brood Year")

# juv return together obs ======
obs_juv_rec <- left_join(pred_N_jpp %>% 
                            dplyr::select(cal_year, obs) %>%
                            rename(juv = "obs"),
                          pred_N_return_pp %>% 
                            dplyr::select(cal_year, obs) %>%
                            rename(rec = "obs")) %>%
  gather(2:3, key = "id", value = "value") %>%
  group_by(id) %>%
  mutate(value = as.numeric(scale(value)))

ggplot(data = obs_juv_rec,aes(x=cal_year, y = value, group = id, color = id)) +
  geom_point() +
  geom_line() +
  ylab("Observed, Mean-Scaled Abundance") +
  theme_classic() + 
  xlab("Brood Year")

# age comp ======
age_comp <- summary(bh_fit, pars = c("p"), 
                    probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column() %>% 
  rename(pred = "mean") %>% 
  cbind(obs = data_list_stan$p_obs) %>% 
  dplyr::select(1,2,9,5,6) %>% 
  gather(2:3, key = "key", value = "value") %>% 
  dplyr::mutate(X10. = case_when(key == "obs" ~ NA,
                         TRUE ~ X10.),
         X90. = case_when(key == "obs" ~ NA,
                          TRUE ~ X90.),
         rowname = case_when(rowname == "p[1]" ~ "Age 3",
                             rowname == "p[2]" ~ "Age 4",
                             rowname == "p[3]" ~ "Age 5",
                             rowname == "p[4]" ~ "Age 6"))

ggplot(data = age_comp, aes(x= rowname, y = value, group = key, 
                            color = key)) +
  geom_point() + 
  geom_errorbar(aes(ymin = X10., ymax = X90.,group = key, color = key, width = 0.1))+
  theme_classic() +
  theme(legend.title = element_blank()) + 
  xlab(" ") +
  ylab("Proportion") +
  ggtitle("Age Composition")


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

productivity <- rbind(productivity1,productivity2) %>%
  left_join(years) %>%
  dplyr::mutate(variable = case_when(variable == "p_1" ~ "Juvenile Productivity",
                              variable == "p_2" ~ "Marine Productivity"))

ggplot(data = productivity, aes(x=cal_year, y = mean, group = variable ,color = variable)) + 
  geom_line( ) +
  geom_ribbon(aes(x=cal_year, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5) +
  facet_wrap(~variable, scales = "free")

ggplot(data = productivity, aes(x=cal_year, y = mean, group = variable ,color = variable)) + 
  geom_line( ) +
  geom_ribbon(aes(x=cal_year, ymin = X10.,
                  ymax = X90.,
                  ), alpha = 0.5) + 
  theme_classic() +
  xlab("Calendar Year") +
  ylab("Productivity") +
  theme(legend.title = element_blank())

# Survival ======
kappasurvival <- summary(bh_fit, pars = c("kappa_marine_survival", "kappa_j_survival"), 
                         probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>% 
  dplyr::mutate(time = rep(1:21, length.out = nrow(.)), 
                variable = case_when(grepl("kappa_marine",rowname) ~ "Marine Survival",
                                     grepl("kappa_j",rowname) ~ "Juvenile Survival")) %>% 
  left_join(years)

ggplot(data = kappasurvival,
       aes(x=cal_year, y = mean, group = variable ,color = variable)) + 
  geom_line( ) +
  geom_ribbon(aes(x=cal_year, ymin = mean-sd,
                  ymax = mean+sd), alpha = 0.5) + 
  scale_x_continuous(breaks = c(2002,2006,2010, 2015,2020, 2022)) +
  theme_classic() +
  xlab("Brood Year") +
  ylab("Survival") +
  theme(legend.title = element_blank())
 
# plot theta ========
## PP =========
theta_df <- as.data.frame(bh_fit, pars = c(#"theta1[1]",
                                         "theta_1_1_pp","theta_1_2_pp","theta_1_3_pp",
                                         "theta_2_1_pp","theta_2_2_pp", "theta_2_3_pp","theta_2_4_pp")) %>% 
  mutate(draw = 1:nrow(.)) %>%
  gather(1:7, key = "rowname", value = "value") %>% 
  dplyr::mutate(variable = case_when(rowname=="theta_1_1_pp" ~ "NBS July/August Temperature",
                                     rowname== "theta_1_2_pp" ~ "Large Zooplankton Index",
                                     rowname=="theta_1_3_pp" ~ "Gelatinous Zooplankton Index",
                                    
                                     rowname== "theta_2_1_pp" ~ "Aleutian Winter Temperature", 
                                     rowname=="theta_2_2_pp" ~ "Chum Salmon Hatchery Release Abundance",
                                     rowname=="theta_2_3_pp" ~ "Pink Salmon Hatchery Release Abundance",
                                     rowname=="theta_2_4_pp" ~ "Fullness Index"), 
                variable = factor(variable, levels = rev(c("NBS July/August Temperature",
                                                        "Large Zooplankton Index",
                                                        "Gelatinous Zooplankton Index",
                                                        "Aleutian Winter Temperature",
                                                        "Chum Salmon Hatchery Release Abundance",
                                                        "Pink Salmon Hatchery Release Abundance",
                                                        "Fullness Index")))) %>% 
  group_by(variable) %>% 
  dplyr::summarise(mean = mean(value),
            ci_80_low = as.numeric(ci(value, method = "HDI", ci = 0.80)$CI_low),
            ci_80_high = as.numeric(ci(value, method = "HDI", ci = 0.80)$CI_high),
            ci_95_low = as.numeric(ci(value, method = "HDI", ci = 0.95)$CI_low),
            ci_95_high = as.numeric(ci(value, method = "HDI", ci = 0.95)$CI_high))
 
ggplot(data = theta_df,
          aes(x= mean, y = variable, 
              group = variable, color = variable)) +
        geom_errorbar(aes(xmin =ci_95_low, xmax = ci_95_high),width = 0, linewidth = 0.5, color = "black") + 
        geom_point() + 
        theme_classic() +
        geom_errorbar(aes(xmin =ci_80_low, xmax = ci_80_high), linewidth = 1, width = 0) + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
        geom_vline(xintercept=0)+
        ggtitle("Covariate Coefficients")+
        ylab("")

## parameters  =========
theta_df <- as.data.frame(bh_fit, pars = c("theta1[1]", "theta1[2]","theta1[3]",
                                           "theta2[1]","theta2[2]","theta2[3]","theta2[4]")) %>% 
  mutate(draw = 1:nrow(.)) %>%
  gather(1:7, key = "rowname", value = "value") %>% 
  dplyr::mutate(variable = case_when(rowname=="theta1[1]" ~ "NBS July/August Temperature",
                                     rowname== "theta1[2]" ~ "Large Zooplankton Index",
                                     rowname=="theta1[3]" ~ "Gelatinous Zooplankton Index",
                                     
                                     rowname=="theta2[1]" ~ "Aleutian Winter Temperature", 
                                     rowname=="theta2[2]" ~ "Chum Salmon Hatchery Release Abundance",
                                     rowname=="theta2[3]" ~ "Pink Salmon Hatchery Release Abundance",
                                     rowname=="theta2[4]" ~ "Fullness Index"), 
                variable = factor(variable, levels = rev(c("NBS July/August Temperature",
                                                           "Large Zooplankton Index",
                                                           "Gelatinous Zooplankton Index",
                                                           "Aleutian Winter Temperature",
                                                           "Chum Salmon Hatchery Release Abundance",
                                                           "Pink Salmon Hatchery Release Abundance",
                                                           "Fullness Index")))) %>% 
  group_by(variable) %>% 
  dplyr::summarise(mean = mean(value),
                   ci_80_low = as.numeric(ci(value, method = "HDI", ci = 0.80)$CI_low),
                   ci_80_high = as.numeric(ci(value, method = "HDI", ci = 0.80)$CI_high),
                   ci_95_low = as.numeric(ci(value, method = "HDI", ci = 0.95)$CI_low),
                   ci_95_high = as.numeric(ci(value, method = "HDI", ci = 0.95)$CI_high))

ggplot(data = theta_df,
       aes(x= mean, y = variable, 
           group = variable, color = variable)) +
  geom_errorbar(aes(xmin =ci_95_low, xmax = ci_95_high),width = 0, linewidth = 0.5, color = "black") + 
  geom_point() + 
  theme_classic() +
  geom_errorbar(aes(xmin =ci_80_low, xmax = ci_80_high), linewidth = 1, width = 0) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_vline(xintercept=0)+
  ggtitle("Covariate Coefficients")+
  ylab("")
