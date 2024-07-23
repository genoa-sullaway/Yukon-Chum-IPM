library(tidyverse)
library(tidybayes)
library(here)
library(rstan)
library(bayesplot)
library(rstanarm)

# load model ==============
bh_fit<- read_rds("output/stan_fit_DATA.RDS")
year_min = 2002
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
  geom_line(aes(x=cal_year, y = obs/1000000), color = "purple") +
  geom_line(aes(x=cal_year, y = mean/1000000)) +
  geom_ribbon(aes(x=cal_year, ymin = ci_10/1000000,
                  ymax = ci_90/1000000), alpha = 0.5) +
  # ggtitle("Spawners")+
  scale_x_continuous(breaks = c(2002, 2005,2010, 2015,2020)) +
  theme_classic() + 
  xlab("Calendar Year") + 
  ylab("Estimated Catch\n(Millions)")

# Juveniles PP ====== 
# multiply by catch q to fit observations

pred_N_jpp <- summary(bh_fit, pars = c("N_j_pp"), 
                    probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = 1:nrow(.)) %>%
  #filter(!time==22) %>%
  left_join(years) %>%
  cbind(obs = data_list_stan$data_stage_j) %>% 
  dplyr::mutate(mean = (mean),
         ci_10=(X10.),
         ci_90=(X90.))

ggplot(data = pred_N_jpp) +
  geom_point(aes(x=cal_year, y = obs/1000000),color = "purple") +
  geom_line(aes(x=cal_year, y = obs/1000000), color = "purple") +
  geom_line(aes(x=cal_year, y = mean/1000000)) +
  geom_ribbon(aes(x=cal_year, ymin = ci_10/1000000,
                  ymax = ci_90/1000000), alpha = 0.5) +
  # ggtitle("Spawners")+
  scale_x_continuous(breaks = c(2002, 2005,2010, 2015,2020)) +
  theme_classic() + 
  xlab("Calendar Year") + 
  ylab("Estimated Juveniles\n(Millions)")

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
                variable = case_when(grepl("kappa_marine_survival",rowname) ~ "Marine Survival",
                                     TRUE ~ "Juvenile Survival")) %>% 
  left_join(years)

ggplot(data = kappasurvival,
       aes(x=cal_year, y = mean, group = variable ,color = variable)) + 
  geom_line( ) +
  geom_ribbon(aes(x=cal_year, ymin = X10.,
                  ymax = X90.), alpha = 0.5) + 
  scale_x_continuous(breaks = c(2002,2006,2010, 2015,2020, 2022)) +
  theme_classic() +
  xlab("Calendar Year") +
  ylab("Survival") +
  theme(legend.title = element_blank())
 

ggplot(data = kappasurvival %>% filter(!time<2),
       aes(x=cal_year, y = mean, group = variable ,color = variable)) + 
  geom_line( ) +
  geom_ribbon(aes(x=cal_year, ymin = X10.,
                  ymax = X90.), alpha = 0.5) +
  facet_wrap(~variable, scales = "free") + 
  scale_x_continuous(breaks = c(2002,2006,2010, 2015,2020, 2022)) +
  theme_classic() + 
  xlab("Calendar Year") + 
  ylab("Survival Rate") +
  theme(legend.title = element_blank())

ggplot(data = kappasurvival %>%
         filter(!time<8 & !time>20), 
       aes(x=cal_year, y = mean, group = variable ,color = variable)) + 
  geom_line( ) +
  geom_ribbon(aes(x=cal_year, ymin = X10.,
                  ymax = X90.), alpha = 0.5) +
  facet_wrap(~variable, scales = "free") + 
  scale_x_continuous(breaks = c(2002,2006,2010, 2015,2020, 2022)) +
  theme_classic() + 
  xlab("Calendar Year") + 
  ylab("Survival Rate")

# plot theta ========
theta <- summary(bh_fit, pars = c("theta_1_1_pp","theta_1_2_pp","theta_1_3_pp",
                                  "theta_2_1_pp","theta_2_2_pp"), 
                 probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column() %>% 
  dplyr::mutate(rowname = case_when(rowname=="theta_1_1_pp" ~ "NBS July/August Temperature",
                                    rowname== "theta_1_2_pp" ~ "Large Zooplankton Index",
                                    rowname=="theta_1_3_pp" ~ "Gelatinous Zooplankton Index",
                                    rowname== "theta_2_1_pp" ~ "GOA Winter Temperature",
                                    rowname=="theta_2_2_pp" ~ "Pink Salmon Hatchery Release Abundance"),
                rowname = factor(rowname, levels = rev(c("NBS July/August Temperature",
                                                        "Large Zooplankton Index",
                                                        "Gelatinous Zooplankton Index",
                                                        "GOA Winter Temperature",
                                                        "Pink Salmon Hatchery Release Abundance"))))

ggplot(data = theta,aes(x= mean, y = rowname, group = rowname, color = rowname)) +
  geom_point() + 
  theme_classic() +
  geom_errorbar(aes(xmin =X10., xmax = X90.),width = 0.1) + 
  # facet_wrap(~rowname,scales = "free",
  #            ncol = 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_vline(xintercept=0)+
  ggtitle("Covariate Coefficients")+
  ylab("")
