library(tidyverse)
library(tidybayes)
library(here)
# library(rstan)
library(bayesplot)
# library(rstanarm)
library(bayestestR)

# load model ==============
# bh_fit<- read_rds("output/stan_fit_DATA.RDS")
# bh_fit <- read_rds("output/stan_fit_DATA_nocovar.RDS")
 bh_fit <- read_rds("output/stan_fit_DATA_forAFS.RDS")

# get this from the model call script: year_min = 2001
years <-read_csv("data/processed_data/yukon_fall_spawners.csv") %>%
  filter(cal_year >= year_min) %>%
  dplyr::select(cal_year) %>%
  dplyr::mutate(brood_year = cal_year) %>% 
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
adult_cvs <- read_xlsx("data/chum_cv.xlsx") %>%
  dplyr::select(year,fall_spawner_cv) %>%
  dplyr::mutate(brood_year = year-3)

pred_N_return_pp <- summary(bh_fit, pars = c("N_return_pp"), 
                      probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = 1:nrow(.)) %>%
  left_join(data.frame(time = 1:nrow(.),
                       brood_year = 2001:2018,
                       obs = yukon_fall_return_brood_year$Brood_Year_Return)) %>% # data_list_stan$data_stage_return[1:nrow(.)])) %>% 
 left_join(adult_cvs) %>% 
  dplyr::select(time, brood_year,obs, fall_spawner_cv,
                X10.,X90., mean) %>% 
  dplyr::mutate( obs= log(obs),
                 sd_obs = log(fall_spawner_cv*obs)) %>%
  dplyr::rename(pred = (mean),
                ci_10=(X10.),
                ci_90=(X90.)) 

return_plot <- ggplot(data = pred_N_return_pp) +
  geom_ribbon(aes(x=brood_year, ymin =ci_10,
                  ymax = ci_90),   fill =  "#2d9d92") +
  geom_line(aes(x=brood_year, y = pred), color = "white") +
  geom_errorbar(aes(x=brood_year, ymin = obs-sd_obs,
                    ymax = obs+sd_obs), width = 0.1, 
                color = "white", alpha = 0.6) +
  geom_point(aes(x=brood_year, y = obs),color = "white",alpha = 0.6) +
  # scale_x_continuous(breaks = c(2001, 2006,2011, 2018)) +
  theme_classic() + 
  xlab("Brood Year") + 
  ylab("Estimated Log Return Abundance") +
  theme(panel.background = element_blank(),  
        plot.background = element_blank(),  
        legend.background = element_blank(),
        legend.text = element_text(color = "white"),
        legend.title = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "white", fill = NA), 
        strip.text.x = element_blank(), 
        axis.line = element_line(color = "white"), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,color = "white"),
        axis.text.y = element_text(color = "white"),
        axis.title.y = element_text(color = "white"),
        axis.title.x = element_text(color = "white"),
        axis.ticks.y = element_line(color = "white"),
        axis.ticks.x = element_line(color = "white")) 

return_plot
ggsave("output/return_est_plot.png", width = 7, height = 4, bg = "transparent")


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

# Juveniles PP ====== 
# multiply by catch q to fit observations
juv_obs <-read_csv("data/processed_data/tidy_juv_fall_yukon.csv") %>% 
     dplyr::mutate(brood_year = Year - 1,
                   # se = `Std. Error for Estimate`,
                   # se_log = `Std. Error for ln(Estimate)`,
                   time = as.numeric(1:nrow(.)),
                   obs = log(fall_abundance),
                   sd = log(CV*obs)) %>%  
     dplyr::select(brood_year,time,obs,sd)  

pred_N_jpp <- summary(bh_fit, pars = c("N_j_pp"), 
                    probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = as.numeric(1:nrow(.))) %>%
  dplyr::select(-sd) %>% 
  left_join(juv_obs, by = "time") %>%  
  dplyr::mutate( 
         ci_10=(X10.),
         ci_90=(X90.))

juv_plot <- ggplot(data = pred_N_jpp) +
  geom_ribbon(aes(x=brood_year, ymin =ci_10,
                  ymax = ci_90),   fill =  "#EAAA00") +
  geom_line(aes(x=brood_year, y = mean), color = "white") +
  geom_errorbar(aes(x=brood_year, ymin = (obs)-(sd),
                    ymax = (obs)+(sd)), width = 0.1,  color = "white", alpha = 0.6) + 
  geom_point(aes(x=brood_year, y = (obs)),color = "white", alpha = 0.6) +
  # geom_line(aes(x=brood_year, y = (obs)), color = "white" ) +
  # scale_x_continuous(breaks = c(2002, 2005,2010, 2015,2020)) +
  theme_classic() + 
  xlab("Brood Year") + 
  ylab("Estimated Log Juvenile Abundance") +
  theme(panel.background = element_blank(), #element_rect(fill = "black", colour = NA),
        plot.background = element_blank(), #element_rect(fill = "black", colour = NA),
        legend.background = element_blank(),
        legend.text = element_text(color = "white"),
        legend.title = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "white", fill = NA), 
        strip.text.x = element_blank(), 
        axis.line = element_line(color = "white"), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,color = "white"),
        axis.text.y = element_text(color = "white"),
        axis.title.y = element_text(color = "white"),
        axis.title.x = element_text(color = "white"),
        axis.ticks.y = element_line(color = "white"),
        axis.ticks.x = element_line(color = "white")) 

juv_plot 

## save ======== 
ggsave("output/juv_est_plot.png", width = 7, height = 4, bg = "transparent")

# juv return together pred ======
pred_juv_rec <- left_join(pred_N_jpp %>% 
                            dplyr::select(brood_year, mean) %>%
                            rename(juv = "mean"),
                          pred_N_return_pp %>% 
                            dplyr::select(brood_year, pred) %>%
                            rename(rec = "pred")) %>%
  gather(2:3, key = "id", value = "value") %>%
  group_by(id) %>%
  mutate(value = as.numeric(scale(value)))

ggplot(data = pred_juv_rec,aes(x=brood_year, y = value, group = id, color = id)) +
  geom_point() +
  geom_line()+
  ylab("Predicted, Mean-Scaled Abundance") +
  theme_classic() + 
  xlab("Brood Year")

# juv + return observations plot by brood year ======
juv_rec_obs_temp <- pred_N_return_pp %>% 
              dplyr::select(brood_year, obs) %>%
              dplyr::rename(rec = "obs") %>% 
  left_join(adult_cvs) %>% 
  dplyr::mutate(rec_sd = (fall_spawner_cv*rec)) %>%
  left_join(juv_obs %>% 
              dplyr::rename(juv = "obs",
                            juv_sd = "sd"))

juv_rec_obs <- juv_rec_obs_temp %>%
  dplyr::select(1,2,7) %>% 
  gather(2:3, key = "id", value = "mean") %>%
  left_join(  juv_rec_obs_temp %>%
              dplyr::select(1,5,8) %>% 
               dplyr::rename(juv = "juv_sd",
                             rec = "rec_sd") %>% 
              gather(2:3, key = "id", value = "sd"))  %>%
  group_by(id) %>%
  dplyr::mutate(mean = as.numeric(scale(mean)))

obs_plot<-ggplot(data = juv_rec_obs, aes(x=brood_year, y = mean, group = id, color = id)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width = 0.1) + 
  ylab("Observed, Mean-Scaled Abundance") +
  theme_classic() + 
  xlab("Brood Year") +
  scale_color_manual(values = c("#EAAA00", "#2d9d92"), labels = c("Juvenile", "Return" )) + 
  theme(panel.background = element_blank(), #element_rect(fill = "black", colour = NA),
        plot.background = element_blank(), #element_rect(fill = "black", colour = NA),
        legend.background = element_blank(),
        legend.text = element_text(color = "white"),
        legend.title = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "white", fill = NA),  
        strip.text.x = element_blank(), 
        axis.line = element_line(color = "white"), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,color = "white"),
        axis.text.y = element_text(color = "white"),
        axis.title.y = element_text(color = "white"),
        axis.title.x = element_text(color = "white"),
        axis.ticks.y = element_line(color = "white"),
        axis.ticks.x = element_line(color = "white"))

## save ======== 
obs_plot
ggsave("output/juv_returns_obs_plot.png", width = 7, height = 4, bg = "transparent")

## same plot but linear scatter plot =========
### dont mean scale data ===== 
juv_rec_obs_absolute <- juv_rec_obs_temp %>%
  dplyr::select(1,2,7) %>% 
  gather(2:3, key = "id", value = "mean") %>%
  left_join( juv_rec_obs_temp %>%
               dplyr::select(1,5,8) %>% 
               dplyr::rename(juv = "juv_sd",
                             rec = "rec_sd") %>% 
               gather(2:3, key = "id", value = "sd"))  %>%
  group_by(id) %>%
  dplyr::mutate(mean = as.numeric(scale(mean)),
                mean =mean - min(mean))  %>% 
  select(brood_year, id,mean) %>%
  spread(id,mean)

ggplot(data = juv_rec_obs_absolute %>% filter(!brood_year >2015),
       aes(x=juv, y = rec, color = brood_year)) +
  geom_point() +
  geom_abline(slope =1) +
  geom_smooth(method = "lm")
 
mod <- lm(juv_rec_obs_absolute$rec~juv_rec_obs_absolute$juv, data= juv_rec_obs_absolute%>% filter(!brood_year >2015))
summary(mod)
plot(resid(mod))

juv_rec_obs_absolute$resid <- resid(mod)

juv_rec_obs_absolute <- juv_rec_obs_absolute %>% 
  dplyr::mutate(color_id = case_when(resid > 1 ~ "juv",
                                     resid < -1 ~ "rec",
                                     TRUE ~ "none"))



ggplot(data=juv_rec_obs_absolute,aes(x= brood_year, y = resid, color = color_id)) +
  geom_point() + 
  theme_classic() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_hline(yintercept = -1, linetype = 2)

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
                                     rowname=="theta_1_3_pp" ~ "Yukon River Mainstem Discharge", #  "Gelatinous Zooplankton Index",
                                    
                                     rowname== "theta_2_1_pp" ~ "Aleutian Winter Temperature", 
                                     rowname=="theta_2_2_pp" ~ "Chum Salmon Hatchery Release Abundance",
                                     rowname=="theta_2_3_pp" ~ "Pink Salmon Hatchery Release Abundance",
                                     rowname=="theta_2_4_pp" ~ "Fullness Index"), 
                variable = factor(variable, levels = rev(c("Yukon River Mainstem Discharge",
                                                           "NBS July/August Temperature",
                                                           "Large Zooplankton Index",
                                                            # "Gelatinous Zooplankton Index",
                                                            "Aleutian Winter Temperature",
                                                            "Chum Salmon Hatchery Release Abundance",
                                                            "Pink Salmon Hatchery Release Abundance",
                                                            "Fullness Index"))),
                stage = case_when(variable %in% c( "Yukon River Mainstem Discharge",
                                                   "NBS July/August Temperature",
                                                  "Large Zooplankton Index",
                                                  "Gelatinous Zooplankton Index") ~ "Juvenile",
                                  variable %in% c("Aleutian Winter Temperature",
                                                  "Chum Salmon Hatchery Release Abundance",
                                                  "Pink Salmon Hatchery Release Abundance",
                                                  "Fullness Index") ~ "Marine")) %>% 
  group_by(stage,variable) %>% 
  dplyr::summarise(mean = median(value),
            ci_80_low = as.numeric(ci(value, method = "HDI", ci = 0.8)$CI_low),
            ci_80_high = as.numeric(ci(value, method = "HDI", ci = 0.8)$CI_high),
            ci_95_low = as.numeric(ci(value, method = "HDI", ci = 0.95)$CI_low),
            ci_95_high = as.numeric(ci(value, method = "HDI", ci = 0.95)$CI_high))
 
theta_plot <- ggplot(data = theta_df,
          aes(x= mean, y = variable, 
              group = variable, color = stage)) +
        geom_errorbar(aes(xmin =ci_95_low, xmax = ci_95_high),
                      width = 0, linewidth = 0.5, color = "white") + 
        geom_point(size = 2) + 
        geom_errorbar(aes(xmin =ci_80_low, xmax = ci_80_high), linewidth = 1.5, width = 0) + 
        theme_classic() +
        scale_color_manual(values =c("#EAAA00", "#2d9d92")) +
        theme(panel.background = element_blank(), #element_rect(fill = "black", colour = NA),
              plot.background = element_blank(), #element_rect(fill = "black", colour = NA),
              legend.background = element_blank(),
              legend.text = element_text(color = "white"),
              legend.title = element_blank(),#"none",
              strip.text = element_blank( ), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_blank(),
              panel.border = element_rect(colour = "white", fill = NA), 
              strip.text.x = element_blank(), 
              axis.line = element_line(color = "white"), 
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,color = "white"),
              axis.text.y = element_text(color = "white"),
              axis.title.y = element_text(color = "white"),
              axis.title.x = element_text(color = "white"),
              axis.ticks.y = element_line(color = "white"),
              axis.ticks.x = element_line(color = "white"),
              panel.spacing.y=unit(0, "lines")) + 
        geom_vline(xintercept=0, color = "white")+
        ylab("") +
        xlab("Mean Covariate Coefficient Value") +
        facet_wrap(~stage, scales = "free_y", ncol = 1)  

theta_plot
ggsave("output/theta_plot.png", width = 7, height = 4, bg = "transparent")

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
                                                           "Fullness Index"))),
                stage = case_when(variable %in% c("NBS July/August Temperature",
                                                  "Large Zooplankton Index",
                                                  "Gelatinous Zooplankton Index") ~ "Juvenile",
                                  variable %in% c("Aleutian Winter Temperature",
                                                  "Chum Salmon Hatchery Release Abundance",
                                                  "Pink Salmon Hatchery Release Abundance",
                                                  "Fullness Index") ~ "Marine")) %>%
  group_by(stage,variable) %>%
  dplyr::summarise(mean = mean(value),
                   ci_80_low = as.numeric(ci(value, method = "HDI", ci = 0.80)$CI_low),
                   ci_80_high = as.numeric(ci(value, method = "HDI", ci = 0.80)$CI_high),
                   ci_95_low = as.numeric(ci(value, method = "HDI", ci = 0.95)$CI_low),
                   ci_95_high = as.numeric(ci(value, method = "HDI", ci = 0.95)$CI_high))

ggplot(data = theta_df,
       aes(x= mean, y = variable,
           group = variable, color = stage)) +
  geom_errorbar(aes(xmin =ci_95_low, xmax = ci_95_high),width = 0, linewidth = 0.5, color = "black") +
  geom_point() +
  theme_classic() +
  geom_errorbar(aes(xmin =ci_80_low, xmax = ci_80_high), linewidth = 1, width = 0) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=0)+
  facet_wrap(~stage, scales = "free_y", ncol = 1) +
  ggtitle("Covariate Coefficients")+
  ylab("")

# Plot A-Gelatinous Zoops and Juvenile Abundance by brood year ============== 
sst_zoop_cov <- read_csv("data/processed_covariates/stage_a_all.csv") %>%
  dplyr::mutate(SST_CDD_NBS = as.numeric(scale(SST_CDD_NBS)),
                yukon_mean_discharge = as.numeric(scale(yukon_mean_discharge))) %>%
  # zoop are already mean scaled
  dplyr::rename(cal_year = Year) %>% 
  dplyr::mutate(brood_year = cal_year-1) %>%  
  dplyr::select(cal_year,brood_year,SST_CDD_NBS, yukon_mean_discharge,Large_zoop,
                Cnideria)
 
pred_N_jpp_cov <- summary(bh_fit, pars = c("N_j_pp"), 
                      probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = 1:nrow(.)) %>%
  left_join(data.frame(time = c(1:nrow(.)), 
                       brood_year = c(data_list_stan$years_data_juv ),
                       obs = c(data_list_stan$data_stage_j))) %>%
  dplyr::mutate(ci_10=(X10.),
                ci_90=(X90.),
                mean = as.numeric(scale(mean))) %>% 
  left_join(sst_zoop_cov) %>% 
  dplyr::select(brood_year,SST_CDD_NBS,yukon_mean_discharge,Large_zoop,
                Cnideria,mean,ci_10,ci_90)

ggplot(data =pred_N_jpp_cov) +
  geom_line(aes(x=brood_year, y = mean)) + 
  geom_line(aes(x=brood_year, y = yukon_mean_discharge), color = "purple") +
  labs(caption = "purple is the covariate")

ggplot(data =pred_N_jpp_cov) +
  geom_line(aes(x=brood_year, y = mean)) + 
  geom_line(aes(x=brood_year, y = SST_CDD_NBS), color = "purple") +
  labs(caption = "purple is the covariate")

ggplot(data =pred_N_jpp_cov) +
  geom_line(aes(x=brood_year, y = mean)) + 
  geom_line(aes(x=brood_year, y = Large_zoop), color = "purple") +
  labs(caption = "purple is the covariate")

ggplot(data =pred_N_jpp_cov) +
  # geom_line(aes(x=brood_year, y = mean)) + 
  geom_line(aes(x=brood_year, y = Cnideria), color = "purple") +
  labs(caption = "purple is the covariate")

ggplot(data =pred_N_jpp_cov) +
  # geom_line(aes(x=brood_year, y = mean)) + 
  geom_line(aes(x=brood_year, y = Cnideria), color = "purple") +
  labs(caption = "purple is the covariate")

ggplot(data =pred_N_jpp_cov) +
  geom_line(aes(x=brood_year, y = Cnideria)) + 
  geom_line(aes(x=brood_year, y = Large_zoop), color = "purple") +
  labs(caption = "purple is the covariate")

# Plot B-Fullness and return abundance by brood year ============== 
fullness_hatchery_temp_cov <- read_csv("data/processed_covariates/stage_b_all.csv") %>%
  dplyr::mutate(SST_CDD_Aleut = as.numeric(scale(SST_CDD_Aleut)),
                Chum_hatchery= as.numeric(scale(Chum_hatchery)), 
                full_index = as.numeric(scale(full_index))  ) %>% 
  dplyr::rename(cal_year = Year) %>% 
  dplyr::mutate(brood_year = cal_year-2) %>%  
  dplyr::select(brood_year,cal_year,SST_CDD_Aleut,
                Chum_hatchery,
                Pink_hatchery,
                full_index) 

pred_N_return_pp_cov <- summary(bh_fit, pars = c("N_return_pp"), 
                            probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = 1:nrow(.)) %>%
  left_join(data.frame(time = c(1:nrow(.)),
                       brood_year = c(data_list_stan$years_data_return),
                       obs = data_list_stan$data_stage_return)) %>%
  dplyr::mutate(ci_10=(X10.),
                ci_90=(X90.),
                mean = as.numeric(scale(mean))) %>% 
  left_join(fullness_hatchery_temp_cov) %>% 
  dplyr::select(brood_year,SST_CDD_Aleut,Chum_hatchery,full_index,mean,ci_10,ci_90)

ggplot(data =pred_N_return_pp_cov) +
  geom_line(aes(x=brood_year, y = mean)) + 
  geom_line(aes(x=brood_year, y = SST_CDD_Aleut), color = "purple") +
  labs(caption = "purple is the covariate")

ggplot(data =pred_N_return_pp_cov) +
  geom_line(aes(x=brood_year, y = mean)) + 
  geom_line(aes(x=brood_year, y = full_index), color = "purple") +
  labs(caption = "purple is the covariate")



# Plot JUST covariates ==============
## A SST ========= 
sst_cov_plot <- sst_zoop_cov %>%
  gather(3:6, key = "variable", value = "value") %>%
  filter(!cal_year<2000, variable%in% c("SST_CDD_NBS" ))

a_sst <- ggplot(data = sst_cov_plot,aes(x=cal_year, y = value, group = variable, color = variable)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values= PNWColors::pnw_palette(name="Shuksan2",n=1,type="discrete")) +
  theme_classic() +
  ylab("Mean Scaled Trend") + 
  xlab("Calendar Year") +
  geom_hline(yintercept =0, linetype =2, color = "white") + 
  theme(panel.background = element_blank(),  
        plot.background = element_blank(),  
        legend.background = element_blank(),
        legend.position = "none",# element_blank(),# "top",
        legend.text = element_text(color = "white"),
        legend.title = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "white", fill = NA), 
        strip.text.x = element_text(color = "white"),
        axis.line = element_line(color = "white"), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,color = "white"),
        axis.text.y = element_text(color = "white"),
        axis.title.y = element_text(color = "white"),
        axis.title.x = element_text(color = "white"),
        axis.ticks.y = element_line(color = "white"),
        axis.ticks.x = element_line(color = "white")) 
a_sst
ggsave("output/SST_plot.png", width = 4, height = 2, bg = "transparent")

## A Zoop backup ========= 
sst_cov_plot <- sst_zoop_cov %>%
  gather(3:6, key = "variable", value = "value") %>%
  filter(!cal_year<2000, variable%in% c("SST_CDD_NBS"))

ggplot(data = sst_cov_plot,aes(x=cal_year, y = value, group = variable, color = variable)) +
  geom_line() +
  geom_point() +
  # facet_wrap(~variable, ncol =1) +
  theme_classic() +
  geom_hline(yintercept =0, linetype =2) + 
  theme(panel.background = element_blank(),  
        plot.background = element_blank(),  
        legend.background = element_blank(),
        legend.position = "none",# element_blank(),# "top",
        legend.text = element_text(color = "white"),
        legend.title = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "white", fill = NA), 
        strip.text.x = element_text(color = "white"),
        axis.line = element_line(color = "white"), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,color = "white"),
        axis.text.y = element_text(color = "white"),
        axis.title.y = element_text(color = "white"),
        axis.title.x = element_text(color = "white"),
        axis.ticks.y = element_line(color = "white"),
        axis.ticks.x = element_line(color = "white")) 

## B ============== 
fullness_hatchery_temp_cov_plot <- fullness_hatchery_temp_cov %>%
  gather(3:6, key = "variable", value = "value") %>%
  filter(!cal_year<2000, variable%in% c("SST_CDD_Aleut",
                                        "Chum_hatchery",
                                        "full_index")) %>% 
  dplyr::mutate(variable = case_when(variable == "SST_CDD_Aleut" ~ "Aleutian Winter Temperature",
                                     variable == "Chum_hatchery" ~ "Chum Salmon Hatchery Release Abundance",
                                     variable == "full_index" ~   "Fullness Index")) 

cov_b_plot<-ggplot(data = fullness_hatchery_temp_cov_plot,
       aes(x=cal_year, y = value, group = variable, color = variable)) +
  geom_hline(yintercept =0, linetype =2, color = "white") +
   geom_line() +
  geom_point() +
  scale_color_manual(values= PNWColors::pnw_palette(name="Anemone",n=3,type="discrete")) +
  facet_wrap(~variable, ncol =1) +
  theme_classic() +
  ylab("Mean Scaled Trend") + 
  xlab("Calendar Year") +
  theme(panel.background = element_blank(),  
              plot.background = element_blank(),  
              legend.background = element_blank(),
              legend.position = "none",# element_blank(),# "top",
              legend.text = element_text(color = "white"),
              legend.title = element_blank(), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_blank(),
              panel.border = element_rect(colour = "white", fill = NA), 
               strip.text.x = element_text(color = "white"),
              axis.line = element_line(color = "white"), 
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,color = "white"),
              axis.text.y = element_text(color = "white"),
              axis.title.y = element_text(color = "white"),
              axis.title.x = element_text(color = "white"),
              axis.ticks.y = element_line(color = "white"),
              axis.ticks.x = element_line(color = "white")) 
cov_b_plot  
ggsave("output/significant_B_Covariates.png", width = 4, height = 3, bg = "transparent")

