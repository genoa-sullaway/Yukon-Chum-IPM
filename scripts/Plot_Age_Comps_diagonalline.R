
library(PNWColors)
library(tidyverse)
library(here)

# load model ==============
bh_fit<- read_rds("output/stan_fit_DATA.RDS")
# load data 
yukon_fall_obs_agecomp <- read_csv("data/processed_data/yukon_fall_age_comp.csv") %>%
  filter(cal_year >= year_min, 
         cal_year <= year_max_cal) %>%
  dplyr::select(2:ncol(.)) %>%
  as.matrix()

# plot age comps pred obs on a diagonal  =================
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
  dplyr::select(time,age,pred,obs)  %>%
  dplyr::mutate(age = factor(age, levels =c("3","4","5","6")))

ggplot(data= age_comp_Q) +
  geom_point(aes(x=pred, y = obs, color = age)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +   
  theme_classic() + 
  ylab("Observed Proportion") +
  xlab("Predicted Proportion") +
  scale_color_manual(values = pnw_palette("Sailboat", n=4) ) +
  guides(color = guide_legend(title = "Age Group"))  # Add this line

ggsave("output/Supplement_Plot_age_comp_predobs.png", height = 3, width =4)
