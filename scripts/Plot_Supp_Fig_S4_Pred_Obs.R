
library(PNWColors)
library(tidyverse)
library(here)


# year DF for joins ==================
years <-read_csv("data/processed_data/yukon_fall_spawners.csv") %>%
  filter(cal_year >= year_min) %>%
  dplyr::select(cal_year) %>%
  dplyr::mutate(time = c(1:nrow(.)))


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

age <- ggplot(data= age_comp_Q) +
  geom_point(aes(x=pred, y = obs, color = age)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +   
  theme_classic() + 
  ylab("Observed Proportion") +
  xlab("Predicted Proportion") +
  scale_color_manual(values = pnw_palette("Sailboat", n=4) ) +
  guides(color = guide_legend(title = "Age Group"))  # Add this line
age
ggsave("output/Supplement_Plot_age_comp_predobs.png", height = 3, width =4)

# diagonal lines for all ==============

## spawners =======
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

sp <- ggplot(data= summ_n_sp) +
  geom_point(aes(x=mean/10000, y = obs/10000, color = cal_year)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +   
  theme_classic() + 
  ylab("Observed Abund") +
  xlab("Predicted Abund") +
  ggtitle("Stage: Spawners") +
  # scale_color_manual(values = pnw_palette("Sailboat", type = "continuous" ) ) +
  guides(color = FALSE) #guide_legend(title = "Cal. year"))  # Add this line

## returns =======

## brood year return ====== 
brood_year <- summary(bh_fit, pars = c("N_brood_year_return"),
                                    probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = (1:20)) %>%
  left_join(years)%>%
  left_join(data.frame(cal_year = c(data_list_stan$years_data_return),
                       obs = data_list_stan$data_stage_return)) %>%
  mutate(rowname = "recruit")
 
return <- ggplot(data= brood_year) +
  geom_point(aes(x=mean/10000, y = obs/10000, color = cal_year)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +   
  theme_classic() + 
  ylab("Observed Abund") +
  xlab("Predicted Abund") +
  ggtitle("Stage: Return") +
  # scale_color_manual(values = pnw_palette("Sailboat", type = "continuous" ) ) +
  guides(color = FALSE) #guide_legend(title = "Cal. year"))  # Add this line

## harvest ========= 
harvest <- summary(bh_fit, pars = c("N_catch"), 
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

hrv<-ggplot(data= harvest) +
  geom_point(aes(x=mean/10000, y = obs/10000, color = cal_year)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +   
  theme_classic() + 
  ylab("Observed Abund") +
  xlab("Predicted Abund") +
  ggtitle("Stage: Harvest") +
  # scale_color_manual(values = pnw_palette("Sailboat", type = "continuous" ) ) +
  guides(color = FALSE) #guide_legend(title = "Cal. year"))  # Add this line


## juveniles =========  

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
  left_join(years)
 
juvs <- pred_N_j %>%
  dplyr::mutate(mean_J_Q = mean*catch_q$mean,
                se_mean = se_mean*catch_q$mean) %>% 
  left_join(data.frame(cal_year = c(data_list_stan$years_data_juv ),
                       obs = c(data_list_stan$data_stage_j))) %>%
  mutate(rowname = "juv") 

juv<-ggplot(data= juvs) +
  geom_point(aes(x=mean_J_Q/10000, y = obs/10000, color = cal_year)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +   
  theme_classic() + 
  ylab("Observed Abund") +
  xlab("Predicted Abund") +
  ggtitle("Stage: Juvenile") +
  guides(color = FALSE) #guide_legend(title = "Cal. year"))  # Add this line

supp<-ggpubr::ggarrange(juv,return,hrv, sp,
                  labels = c("A.", "B.", "C.", "D."))
supp
ggsave("output/Supplement_Plot_S4_predobs.png", height = 5, width =7)
