library(tidyverse)
library(tidybayes)
library(here) 
library(bayesplot) 
library(bayestestR)

# Plot percent change in survival ============
source("scripts/Calculate_Percent_Survival.R")

ci_df <- read_csv("output/survival_percent_diff.csv") %>%
          dplyr::mutate(covariate = case_when(covariate == "SST CDD" ~ "NBS SST",
                                              covariate == "AI Temp" ~ "AI SST",
                                              covariate == "Fullness" ~ "Fullness Index",
                                              covariate == "Pollock" ~ "Pollock Recruitment",
                                              covariate == "Chum" ~ "Hatchery Chum",
                                              TRUE ~ covariate), 
            covariate = factor(covariate, levels = rev(c("Spawner Size",
                                             "Snow Pack",
                                             "NBS SST",
                                             "Pollock Recruitment",
                                             
                                             "Fullness Index",
                                             "AI SST",
                                             "Hatchery Chum"))))
 
theta_plot <- ggplot(data = ci_df,
                     aes(x= mean_percent_change, y = covariate, 
                         group = stage, color = stage)) +
  geom_errorbar(aes(xmin =lower_95, xmax = upper_95),
                width = 0, linewidth = 0.5 ) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(xmin =lower_50, xmax = upper_50), linewidth = 1.5, width = 0) + 
  theme_classic() +
  scale_color_manual(values =c("#EAAA00", "#2d9d92")) +
  theme(panel.background = element_blank(),  
        plot.background = element_blank(),  
        legend.background = element_blank(), 
        legend.title = element_blank(), 
        strip.text = element_blank( ), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(), 
        strip.text.x = element_blank(),  
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1 ), 
        panel.spacing.y=unit(0, "lines")) + 
  geom_vline(xintercept=0 )+
  ylab("") +
  xlab("Estimated Percent Change in Survival") +
  facet_wrap(~stage, scales = "free_y", ncol = 1)  

theta_plot
ggsave("output/Plot_Manuscript_Covariates.png", width = 7, height = 4 )

# STOP HERE FOR MANUSCRIPT PLOTS ===== 
## perent change plot for talk - white ==========================
theta_plot1 <- ggplot(data = ci_df,
                      aes(x= mean_percent_change, y = covariate, 
                          group = stage, color = stage)) +
  geom_errorbar(aes(xmin =lower_95, xmax = upper_95),
                width = 0, linewidth = 0.5 ) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(xmin =lower_50, xmax = upper_50), linewidth = 1.5, width = 0) + 
  theme_classic() + 
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.6, color = "white") + 
  scale_color_manual(values =c("#EAAA00", "#2d9d92")) +
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
        panel.spacing.y=unit(0, "lines")) +  
  ylab("") +
  xlab("Mean Percent Change in Survival") +
  facet_wrap(~stage, scales = "free_y", ncol = 1)  

theta_plot1
ggsave("output/Plot_Talk_White_Covariates.png", width = 7, height = 4 )



# seperate plots % change ==========

## Theta plot for talk - white ==========================
theta_plot_juv <- ggplot(data = ci_df %>% filter(stage == "Juvenile"), 
         aes(x= mean_percent_change, y = covariate, 
             group = stage, color = stage)) +
  geom_errorbar(aes(xmin =lower_95, xmax = upper_95),
                width = 0, linewidth = 0.5 ) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(xmin =lower_50, xmax = upper_50), linewidth = 1.5, width = 0) + 
  theme_classic() +
  scale_color_manual(values =c("#EAAA00")) +
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.6, color = "white") + 
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
        panel.spacing.y=unit(0, "lines")) +  
  ylab("")  +
  xlab("Mean Percent Change in Survival") 

theta_plot_juv
ggsave("output/Plot_JUV_ Talk_White_Covariates.png", width = 7, height = 4 )


## Theta plot for talk - white ==========================
theta_plot_mature <- ggplot(data = ci_df %>% filter(stage == "Return"), 
                                   aes(x= mean_percent_change, y = covariate, 
                                       group = stage, color = stage)) +
                              geom_errorbar(aes(xmin =lower_95, xmax = upper_95),
                                            width = 0, linewidth = 0.5 ) + 
                              geom_point(size = 2) + 
                              geom_errorbar(aes(xmin =lower_50, xmax = upper_50), linewidth = 1.5, width = 0) + 
                              theme_classic() +
  scale_color_manual(values =c("#2d9d92")) +
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.6, color = "white") + 
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
        panel.spacing.y=unit(0, "lines")) +  
  ylab("") +
  xlab("Mean Percent Change in Survival")  

theta_plot_mature

ggsave("output/Plot_MATURE_Talk_White_Covariates.png", width = 7, height = 4 )







# OLD Plot thetas directly =============
## load model ==============
bh_fit <- read_rds("output/stan_fit_DATA.RDS")
 
theta_df <- as.data.frame(bh_fit, pars = c("theta1[1]", "theta1[2]","theta1[3]","theta1[4]",
                                           # "theta1[5]","theta1[6]",
                                           "theta2[1]","theta2[2]","theta2[3]"
                                           #,"theta2[4]"
                                           )) %>%
  mutate(draw = 1:nrow(.)) %>%
  gather(1:7, key = "rowname", value = "value") %>%
  dplyr::mutate(variable = case_when(rowname=="theta1[1]" ~ "Mean Spawner Size", 
                                     rowname=="theta1[2]"~  "Winter Snowpack", 
                                     rowname=="theta1[3]" ~ "NBS July/August Temperature",
                                     rowname=="theta1[4]" ~ "Pollock Recruitment",
                                     # rowname=="theta1[5]" ~ "Winter Snowpack",
                                     # rowname=="theta1[6]" ~ "Juvenile Sockeye Salmon Abundance",
                                     
                                     rowname=="theta2[1]" ~ "Aleutian Winter Temperature",
                                     rowname=="theta2[2]" ~ "Chum Salmon Hatchery Release Abundance",
                                     rowname=="theta2[3]" ~ "Fullness Index"#"Pink Salmon Hatchery Release Abundance",
                                     #rowname=="theta2[4]" ~ 
                                       ),
                variable = factor(variable, levels = rev(c("Mean Spawner Size",
                                                           "Winter Snowpack",
                                                           "Yukon River - Mean Flow", 
                                                           "NBS July/August Temperature",
                                                           "Juvenile Sockeye Salmon Abundance",
                                                           "Pollock Recruitment",
                                                           
                                                           "Fullness Index",
                                                           "Aleutian Winter Temperature",
                                                           "Chum Salmon Hatchery Release Abundance",
                                                           "Pink Salmon Hatchery Release Abundance"))),
                stage = case_when(variable %in% c("NBS July/August Temperature",
                                                  "Yukon River - Mean Flow", 
                                                  "Winter Snowpack",
                                                  "Pollock Recruitment",
                                                  "Mean Spawner Size",
                                                  "Juvenile Sockeye Salmon Abundance") ~ "Juvenile",
                                  variable %in% c( "Aleutian Winter Temperature",
                                                   "Chum Salmon Hatchery Release Abundance",
                                                   "Pink Salmon Hatchery Release Abundance",
                                                   "Fullness Index") ~ "Marine")) %>%
  group_by(stage,variable) %>%
  dplyr::summarise(mean = median(value),
                   ci_50_low = as.numeric(ci(value, method = "HDI", ci = 0.50)$CI_low),
                   ci_50_high = as.numeric(ci(value, method = "HDI", ci = 0.50)$CI_high),
                   ci_80_low = as.numeric(ci(value, method = "HDI", ci = 0.80)$CI_low),
                   ci_80_high = as.numeric(ci(value, method = "HDI", ci = 0.80)$CI_high))

theta_plot <- ggplot(data = theta_df,
                     aes(x= mean, y = variable, 
                         group = variable, color = stage)) +
  geom_errorbar(aes(xmin =ci_80_low, xmax = ci_80_high),
                width = 0, linewidth = 0.5 ) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(xmin =ci_50_low, xmax = ci_50_high), linewidth = 1.5, width = 0) + 
  theme_classic() +
  scale_color_manual(values =c("#EAAA00", "#2d9d92")) +
  theme(panel.background = element_blank(),  
        plot.background = element_blank(),  
        legend.background = element_blank(), 
        legend.title = element_blank(), 
        strip.text = element_blank( ), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(), 
        strip.text.x = element_blank(),  
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1 ), 
        panel.spacing.y=unit(0, "lines")) + 
  geom_vline(xintercept=0 )+
  ylab("") +
  xlab("Median Covariate Coefficient Value") +
  facet_wrap(~stage, scales = "free_y", ncol = 1)  

theta_plot
ggsave("output/Plot_Manuscript_Covariates.png", width = 7, height = 4 )

 
## Theta plot for talk - white ==========================
theta_plot1 <- ggplot(data = theta_df,
                     aes(x= mean, y = variable, 
                         group = variable, color = stage)) +
  
  geom_vline(xintercept=0, color = "white", linetype = 2, alpha = 0.6 )+
  geom_errorbar(aes(xmin =ci_80_low, xmax = ci_80_high),
                width = 0, linewidth = 0.5 ) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(xmin =ci_50_low, xmax = ci_50_high), linewidth = 1.5, width = 0) + 
  theme_classic() +
  scale_color_manual(values =c("#EAAA00", "#2d9d92")) +
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
        panel.spacing.y=unit(0, "lines")) +  
  ylab("") +
  xlab("Mean Covariate Coefficient Value") +
  facet_wrap(~stage, scales = "free_y", ncol = 1)  

theta_plot1
ggsave("output/Plot_Talk_White_Covariates.png", width = 7, height = 4 )



# seperate plots ==========

## Theta plot for talk - white ==========================
theta_plot_juv <- ggplot(data = theta_df %>% filter(stage == "Juvenile"),
                      aes(x= mean, y = variable, 
                          group = variable, color = stage)) + 
  geom_vline(xintercept=0, color = "white", linetype = 2, alpha = 0.6 )+
  geom_errorbar(aes(xmin =ci_80_low, xmax = ci_80_high),
                width = 0, linewidth = 0.5 ) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(xmin =ci_50_low, xmax = ci_50_high), linewidth = 1.5, width = 0) + 
  theme_classic() +
  scale_color_manual(values =c("#EAAA00")) +
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
        panel.spacing.y=unit(0, "lines")) +  
  ylab("") +
  xlab("Mean Covariate Coefficient Value")  

theta_plot_juv
ggsave("output/Plot_JUV_ Talk_White_Covariates.png", width = 7, height = 4 )


## Theta plot for talk - white ==========================
theta_plot_mature <- ggplot(data = theta_df %>% filter(stage == "Marine"),
                      aes(x= mean, y = variable, 
                          group = variable, color = stage)) + 
  geom_vline(xintercept=0, color = "white", linetype = 2, alpha = 0.6 )+
  geom_errorbar(aes(xmin =ci_80_low, xmax = ci_80_high),
                width = 0, linewidth = 0.5 ) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(xmin =ci_50_low, xmax = ci_50_high), linewidth = 1.5, width = 0) + 
  theme_classic() +
  scale_color_manual(values =c("#2d9d92")) +
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
        panel.spacing.y=unit(0, "lines")) +  
  ylab("") +
  xlab("Mean Covariate Coefficient Value")  

theta_plot_mature

ggsave("output/Plot_MATURE_Talk_White_Covariates.png", width = 7, height = 4 )
