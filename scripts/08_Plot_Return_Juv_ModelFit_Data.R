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
  dplyr::mutate(brood_year = year-3) %>%
  dplyr::select(brood_year,fall_spawner_cv) 

pred_return <-  as.data.frame(bh_fit, pars = c("N_brood_year_return")) %>%
  mutate(draw = 1:nrow(.)) %>%
  gather(1:(ncol(.)-1), key = "rowname", value = "value") %>%
  data.frame() %>%
  group_by(rowname) %>%
  summarise(mean = mean(value),
            ci_80_low = as.numeric(ci(value, method = "HDI", ci = 0.80)$CI_low),
            ci_80_high = as.numeric(ci(value, method = "HDI", ci = 0.80)$CI_high),
            ci_95_low = as.numeric(ci(value, method = "HDI", ci = 0.99)$CI_low),
            ci_95_high = as.numeric(ci(value, method = "HDI", ci = 0.99)$CI_high)) %>%
  separate(rowname, into = c("del1","year", "del2"), sep = c(-3,-1)) %>%
  arrange(row_number(.)) %>%
  dplyr::mutate(brood_year = c(2002:2021)) %>%
  left_join(data.frame(time = c(1:17),
                       brood_year = c(2002:2018),
                       obs = c(yukon_fall_return_brood_year$Brood_Year_Return))) %>% # data_list_stan$data_stage_return[1:nrow(.)])) %>% 
  left_join(adult_cvs) %>% 
  dplyr::select(brood_year,obs, fall_spawner_cv,
              mean,ci_95_low,ci_95_high) %>% 
  dplyr::mutate(sd_obs =   (fall_spawner_cv*obs)) %>%
  filter(!brood_year > 2018)

return_plot <- ggplot(data = pred_return) +
  geom_ribbon(aes(x=brood_year, ymin =ci_95_low/1000000,
                  ymax = ci_95_high/1000000),   fill =  "#2d9d92") +
  geom_line(aes(x=brood_year, y = mean/1000000)#, color = "white"
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
# plot with different confidence intervals=====
# Juveniles PP ====== 
# multiply by catch q to fit observations
juv_obs <-read_csv("data/processed_data/tidy_juv_fall_yukon.csv") %>% 
  dplyr::mutate(brood_year = Year - 1,
                time = as.numeric(1:nrow(.)),
                obs = (fall_abund),
                sd = Std..Error.for.Estimate ) %>% #(CV*obs)) %>%  
  # filter(!Year %in% c(2020, 2008,2013)) %>% 
  dplyr::select(brood_year,obs,sd)  
  # rbind(data.frame(brood_year = c(2008, 2013,2020),
  #                  obs = c(NA,NA,NA),
  #                  sd = c(NA,NA,NA)))  

# plot juvenile observations ======
juv_all_plot <- ggplot(data = juv_obs, aes(x=brood_year, y = obs)) +
  geom_point(color ="#EAAA00") + 
  geom_line(color ="#EAAA00") + 
  geom_errorbar(aes(x=brood_year, ymin = obs-sd, 
                    ymax= obs+sd), 
                width = 0.1, color = "#EAAA00") +
  theme_classic() +
  ylab("Abundance Estimate") +
  xlab("Brood Year") + 
theme(panel.background = element_blank(), #element_rect(fill = "black", colour = NA),
      plot.background = element_blank(), #element_rect(fill = "black", colour = NA),
      legend.background = element_blank(),
      legend.text = element_text(color = "white"),
      legend.title = element_blank(), 
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
      panel.spacing.y=unit(0, "lines")) 


juv_all_plot
ggsave("output/juvenile_observations_fallchum_plot.png", width = 5, height = 3)

# catch_q <- summary(bh_fit, pars = c("log_catch_q"), 
#                    probs = c(0.1, 0.9))$summary %>%
#   data.frame() %>%
#   rownames_to_column()  %>% 
#   mutate(mean = exp(mean))

catch_q <- as.data.frame(bh_fit, pars = c("log_catch_q")) %>%
  dplyr::mutate(catch_q = exp(log_catch_q)) %>%
  dplyr::select(-log_catch_q)

pred_N_j_adjusted <- as.data.frame(bh_fit, pars = c("N_j")) %>%
  dplyr::mutate(draw = 1:nrow(.)) %>%
  cbind(catch_q) %>%
  dplyr::mutate(across(starts_with("N_j"), ~ .x * catch_q))  

# Process each column individually and create a clean dataframe
n_j_cols <- grep("^N_j\\[", names(pred_N_j_adjusted), value = TRUE)

n_j_summary_clean <- map_dfr(n_j_cols, function(col) {
  values <- pred_N_j_adjusted[[col]]
  tibble(
    parameter = col,
    mean = mean(values, na.rm = TRUE),
    median = median(values, na.rm = TRUE),
    ci_80_low = as.numeric(ci(values, ci = 0.8, method = "HDI")$CI_low),
    ci_80_high = as.numeric(ci(values, ci = 0.8, method = "HDI")$CI_high),
    ci_95_low = as.numeric(ci(values, ci = 0.95, method = "HDI")$CI_low),
    ci_95_high = as.numeric(ci(values, ci = 0.95, method = "HDI")$CI_high)
  )
}) %>% 
  cbind(brood_year=years$brood_year[1:20]) %>%
  left_join(juv_obs)  
 
juv_plot <- ggplot(data = n_j_summary_clean) +
  geom_ribbon(aes(x=brood_year, ymin =ci_95_low/1000000,
                  ymax = ci_95_high/1000000),   fill =  "#EAAA00") +
  geom_line(aes(x=brood_year, y = mean/1000000)) +
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

ggsave("output/Plot_Manuscript_Juv_ReturnFit_Obs.png", width = 6, height = 5, bg="white")

# juv plot for talk ===================
juv_plot_talk <- ggplot(data = n_j_summary_clean) +
  geom_ribbon(aes(x=brood_year, ymin =ci_95_low/1000000,
                  ymax = ci_95_high/1000000),   fill =  "#EAAA00") +
  geom_line(aes(x=brood_year, y = mean/1000000)) +
  geom_errorbar(aes(x=brood_year, ymin = (obs-sd)/1000000,
                    ymax = (obs+sd)/1000000), width = 0.1, alpha = 0.6,color = "white") + 
  geom_point(aes(x=brood_year, y = (obs)/1000000), alpha = 0.6, color = "white") +
  # geom_line(aes(x=brood_year, y = (obs)), color = "white" ) +
  # scale_x_continuous(breaks = c(2002, 2005,2010, 2015,2020)) +
  theme_classic() + 
  xlab("Brood Year") + 
  ylab("Est. Juv. Abundance\n (Millions)") +
  theme(panel.background = element_blank(), #element_rect(fill = "black", colour = NA),
        plot.background = element_blank(), #element_rect(fill = "black", colour = NA),
        legend.background = element_blank(),
        legend.text = element_text(color = "white"),
        legend.title = element_blank(), 
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
        panel.spacing.y=unit(0, "lines")) 

   
juv_plot_talk 
ggsave("output/Plot_juv_fit_Obs_TALK.png", width = 8, height = 6, bg="transparent")

# return plot for talk =======
return_plot_talk <- ggplot(data = pred_return) +
  geom_ribbon(aes(x=brood_year, ymin =ci_95_low/1000000,
                  ymax = ci_95_high/1000000),   fill =  "#2d9d92") +
  geom_line(aes(x=brood_year, y = mean/1000000)#, color = "white"
  ) +
  geom_errorbar(aes(x=brood_year, ymin = (obs-sd_obs)/1000000,
                    ymax = (obs+sd_obs)/1000000), width = 0.1, color = "white",alpha = 0.7) +
  geom_point(aes(x=brood_year, y = obs/1000000), color = "white",alpha = 0.7 ) +
  theme_classic() + 
  xlab("Brood Year") + 
  ylab("Est. Return Abundance\n(Millions)") +
  scale_y_continuous(limits = c(0, 2500000/1000000)) + 
  theme(panel.background = element_blank(), #element_rect(fill = "black", colour = NA),
        plot.background = element_blank(), #element_rect(fill = "black", colour = NA),
        legend.background = element_blank(),
        legend.text = element_text(color = "white"),
        legend.title = element_blank(), 
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
        panel.spacing.y=unit(0, "lines")) 


return_plot_talk 
ggsave("output/Plot_return_fit_Obs_TALK.png", width = 10, height = 6, bg="transparent")

