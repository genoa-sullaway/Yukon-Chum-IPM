library(tidyverse)
library(tidybayes)
library(here)
# library(rstan)
library(bayesplot)
# library(rstanarm)
library(bayestestR)

# load model ==============
bh_fit <- read_rds("output/stan_fit_DATA.RDS")

# get this from the model call script: year_min = 2001
years <-read_csv("data/processed_data/yukon_fall_spawners.csv") %>%
  filter(cal_year >= year_min) %>%
  dplyr::select(cal_year) %>%
  dplyr::mutate(brood_year = cal_year) %>% 
  dplyr::mutate(time = c(1:nrow(.)))

# return  ====== 
adult_cvs <- read_xlsx("data/chum_cv.xlsx") %>%
  dplyr::select(year,fall_spawner_cv) %>%
  dplyr::mutate(brood_year = year-3)

pred_return <- summary(bh_fit, pars = c("N_brood_year_return"), 
                            probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = 1:nrow(.)) %>%
  left_join(data.frame(time = c(1:17),
                       brood_year = c(2002:2018),
                       obs = c(yukon_fall_return_brood_year$Brood_Year_Return))) %>% # data_list_stan$data_stage_return[1:nrow(.)])) %>% 
  left_join(adult_cvs) %>% 
  dplyr::select(time, brood_year,obs, fall_spawner_cv,
                X10.,X90., mean) %>% 
  dplyr::mutate( obs=   (obs),
                 sd_obs =   (fall_spawner_cv*obs)) %>%
  dplyr::rename(pred =  (mean),
                ci_10= (X10.),
                ci_90= (X90.)) 

return_plot <- ggplot(data = pred_return) +
  geom_ribbon(aes(x=brood_year, ymin =ci_10/1000000,
                  ymax = ci_90/1000000),   fill =  "#2d9d92") +
  geom_line(aes(x=brood_year, y = pred/1000000)#, color = "white"
            ) +
  geom_errorbar(aes(x=brood_year, ymin = (obs-sd_obs)/1000000,
                    ymax = (obs+sd_obs)/1000000), width = 0.1) +
  geom_point(aes(x=brood_year, y = obs/1000000) ) +
  theme_classic() + 
  xlab("Brood Year") + 
  ylab("Est. Return Abundance\n(Millions)") +
  scale_y_continuous(limits = c(0, 2500000/1000000)) + 
  theme(panel.background = element_blank(),  
        plot.background = element_blank(),  
        legend.background = element_blank(),
        # legend.text = element_text(color = "white"),
        legend.title = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        # panel.border = element_rect(colour = "white", fill = NA), 
        strip.text.x = element_blank(), 
        axis.title.y = element_text(size = 10),
        # axis.line = element_line(color = "white"), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
        ) 

return_plot
# ggsave("output/return_est_plot.png", width = 7, height = 4, bg = "transparent")



# Juveniles PP ====== 
# multiply by catch q to fit observations
juv_obs <-read_csv("data/processed_data/tidy_juv_fall_yukon.csv") %>% 
  dplyr::mutate(brood_year = Year - 1,
                # se = `Std. Error for Estimate`,
                # se_log = `Std. Error for ln(Estimate)`,
                time = as.numeric(1:nrow(.)),
                obs = (fall_abund),
                sd = Std..Error.for.Estimate ) %>% #(CV*obs)) %>%  
  filter(!Year %in% c(2020, 2008,2013)) %>% 
  dplyr::select(time,obs,sd)  

catch_q <- summary(bh_fit, pars = c("log_catch_q"), 
                   probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>% 
  mutate(mean = exp(mean))

pred_N_j <- summary(bh_fit, pars = c("N_j"), 
                    probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = 1:nrow(.),
                ci_10=(X10.),
                ci_90=(X90.)) %>% 
  dplyr::mutate(mean_J_Q = mean*catch_q$mean,
                ci_10 = ci_10*catch_q$mean,
                ci_90 = ci_90*catch_q$mean) %>% 
  left_join(years) %>%
  dplyr::select(-sd) %>% 
  left_join(juv_obs, by = "time")  

juv_plot <- ggplot(data = pred_N_j) +
  geom_ribbon(aes(x=brood_year, ymin =ci_10/1000000,
                  ymax = ci_90/1000000),   fill =  "#EAAA00") +
  geom_line(aes(x=brood_year, y = mean_J_Q/1000000)) +
  geom_errorbar(aes(x=brood_year, ymin = (obs-sd)/1000000,
                    ymax = (obs+sd)/1000000), width = 0.1, alpha = 0.6) + 
  geom_point(aes(x=brood_year, y = (obs)/1000000), alpha = 0.6) +
  # geom_line(aes(x=brood_year, y = (obs)), color = "white" ) +
  # scale_x_continuous(breaks = c(2002, 2005,2010, 2015,2020)) +
  theme_classic() + 
  xlab("Brood Year") + 
  ylab("Est. Juv. Abundance\n (Millions)") +
  theme(panel.background = element_blank(), #element_rect(fill = "black", colour = NA),
        plot.background = element_blank(), #element_rect(fill = "black", colour = NA),
        legend.background = element_blank(),
        # legend.text = element_text(color = "white"),
        legend.title = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        # panel.border = element_rect(colour = "white", fill = NA), 
        strip.text.x = element_blank(), 
        # axis.line = element_line(color = "white"), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),#,color = "white"),
        # axis.text.y = element_text(color = "white"),
        axis.title.y = element_text(size = 10)
        # axis.title.x = element_text(color = "white"),
        # axis.ticks.y = element_line(color = "white"),
        # axis.ticks.x = element_line(color = "white")
        ) 

juv_plot 

## save ======== 
obs_plot <- ggpubr::ggarrange(juv_plot,return_plot, nrow = 2, labels = c("a.", "b."))
obs_plot

ggsave("output/Plot_Manuscript_Juv_ReturnFit_Obs.png", width = 7, height = 5, bg="white")

