library(tidyverse)
library(tidybayes)
library(here)
library(rstan)
library(bayesplot)
library(rstanarm)
library(LaplacesDemon)

# load model ==============
bh_fit<- read_rds("output/stan_fit_DATA.RDS")

# bh_fit <- read_rds("output/stan_fit_DATA_forAFS.RDS")
# bh_fit <- read_rds("output/stan_fit_DATA_nocovar.RDS")

# year DF for joins ==================
years <-read_csv("data/processed_data/yukon_fall_spawners.csv") %>%
  filter(cal_year >= year_min) %>%
  dplyr::select(cal_year) %>%
  dplyr::mutate(time = c(1:nrow(.)))



# plot  estimated kappas survival ======
kappasurvival <- summary(bh_fit, pars = c("kappa_marine_survival", "kappa_j_survival"), 
                         probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>% 
  dplyr::mutate(time = rep(1:21, length.out = nrow(.)), 
                variable = case_when(grepl("kappa_marine_survival",rowname) ~ "kappa_marine_survival",
                                     grepl("kappa_j_survival",rowname) ~ "kappa_j_survival")) %>% 
  left_join(years)
 

survival_plot<- ggplot(data = kappasurvival,
       aes(x = cal_year, y = mean, group = variable, color = variable, fill = variable)) +
  
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd),
              alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) + #, stroke = 1.2) +
  
  scale_x_continuous(
    breaks = c(2002, 2006, 2010, 2015, 2020, 2022),
    expand = c(0.02, 0)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = c(0.02, 0)
  ) +
  scale_color_manual(
    values = c("kappa_j_survival" = "#EAAA00",
               "kappa_marine_survival" = "#2A9D8F"),
    labels = c("kappa_j_survival" = "Juvenile Survival",
               "kappa_marine_survival" = "Marine Survival")
  ) +
 
  scale_fill_manual(
    values = c("kappa_j_survival" = "#EAAA00",
               "kappa_marine_survival" = "#2A9D8F"),
    guide = "none"
    # labels = c("kappa_j_survival" = "Juvenile Survival",
    #            "kappa_marine_survival" = "Marine Survival")
  ) +
  
  labs(
    # title = "Kappa Survival Estimates Over Time",
    # subtitle = "Shaded bands represent ±1 SD",
    x = "Calendar Year",
    y = "Mean Survival Rate",
    color = NULL,
    fill = NULL
  ) +
  
  theme_bw(base_size = 13) +
  theme(
    # plot.title      = element_text(face = "bold", size = 15, margin = margin(b = 4)),
    # plot.subtitle   = element_text(color = "grey50", size = 11, margin = margin(b = 10)),
    legend.position = "bottom",
    legend.key.width = unit(1.5, "cm"),
    panel.grid.minor = element_blank(),
    # panel.grid.major = element_line(color = "grey90"),
    # axis.title      = element_text(face = "bold"),
    # axis.text       = element_text(color = "grey30"),
    plot.margin     = margin(12, 16, 12, 12)
  )

survival_plot

ggsave("output_sullaway_etal/Plot_Supplemental_Survival.png", width = 7, 
       height = 4, bg = "transparent")
