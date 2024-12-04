library(tidyverse)
library(tidybayes)
library(here) 
library(bayesplot) 
library(bayestestR)

# load model ==============
bh_fit <- read_rds("output/stan_fit_DATA.RDS")
 

theta_df <- as.data.frame(bh_fit, pars = c("theta1[1]", "theta1[2]","theta1[3]","theta1[4]",
                                           "theta2[1]","theta2[2]","theta2[3]","theta2[4]")) %>%
  mutate(draw = 1:nrow(.)) %>%
  gather(1:8, key = "rowname", value = "value") %>%
  dplyr::mutate(variable = case_when(rowname=="theta1[1]" ~ "NBS July/August Temperature",
                                     rowname== "theta1[2]" ~ "Yukon River - Mean Flow",
                                     rowname=="theta1[3]" ~ "Pollock Recruitment",
                                     rowname=="theta1[4]" ~ "Mean Spawner Size",
                                     
                                     rowname=="theta2[1]" ~ "Aleutian Winter Temperature",
                                     rowname=="theta2[2]" ~ "Chum Salmon Hatchery Release Abundance",
                                     rowname=="theta2[3]" ~ "Pink Salmon Hatchery Release Abundance",
                                     rowname=="theta2[4]" ~ "Fullness Index"),
                variable = factor(variable, levels = rev(c("NBS July/August Temperature",
                                                           "Yukon River - Mean Flow", 
                                                           "Pollock Recruitment",
                                                           "Mean Spawner Size",
                                                           
                                                           "Aleutian Winter Temperature",
                                                           "Chum Salmon Hatchery Release Abundance",
                                                           "Pink Salmon Hatchery Release Abundance",
                                                           "Fullness Index"))),
                stage = case_when(variable %in% c("NBS July/August Temperature",
                                                  "Yukon River - Mean Flow", 
                                                  "Pollock Recruitment",
                                                  "Mean Spawner Size") ~ "Juvenile",
                                  variable %in% c( "Aleutian Winter Temperature",
                                                   "Chum Salmon Hatchery Release Abundance",
                                                   "Pink Salmon Hatchery Release Abundance",
                                                   "Fullness Index") ~ "Marine")) %>%
  group_by(stage,variable) %>%
  dplyr::summarise(mean = mean(value),
                   ci_80_low = as.numeric(ci(value, method = "HDI", ci = 0.65)$CI_low),
                   ci_80_high = as.numeric(ci(value, method = "HDI", ci = 0.65)$CI_high),
                   ci_95_low = as.numeric(ci(value, method = "HDI", ci = 0.80)$CI_low),
                   ci_95_high = as.numeric(ci(value, method = "HDI", ci = 0.80)$CI_high))

theta_plot <- ggplot(data = theta_df,
                     aes(x= mean, y = variable, 
                         group = variable, color = stage)) +
  geom_errorbar(aes(xmin =ci_95_low, xmax = ci_95_high),
                width = 0, linewidth = 0.5 ) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(xmin =ci_80_low, xmax = ci_80_high), linewidth = 1.5, width = 0) + 
  theme_classic() +
  scale_color_manual(values =c("#EAAA00", "#2d9d92")) +
  theme(panel.background = element_blank(), #element_rect(fill = "black", colour = NA),
        plot.background = element_blank(), #element_rect(fill = "black", colour = NA),
        legend.background = element_blank(),
        # legend.text = element_text(color = "white"),
        legend.title = element_blank(),#"none",
        strip.text = element_blank( ), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        # panel.border = element_rect(colour = "white", fill = NA), 
        strip.text.x = element_blank(), 
        # axis.line = element_line(color = "white"), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1 ),
        # axis.text.y = element_text(color = "white"),
        # axis.title.y = element_text(color = "white"),
        # axis.title.x = element_text(color = "white"),
        # axis.ticks.y = element_line(color = "white"),
        # axis.ticks.x = element_line(color = "white"),
        panel.spacing.y=unit(0, "lines")) + 
  geom_vline(xintercept=0 )+
  ylab("") +
  xlab("Mean Covariate Coefficient Value") +
  facet_wrap(~stage, scales = "free_y", ncol = 1)  

theta_plot
ggsave("output/Plot_Manuscript_Covariates.png", width = 7, height = 4 )
