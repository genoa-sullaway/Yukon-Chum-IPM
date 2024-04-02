library(tidyverse)
library(tidybayes)
library(here)



# translate simulated data_list to actual observations 
bh_fit<- read_rds("output/stan_fit_SIMULATED_OUTPUT_statespace.RDS")

bh_summary <- summary(bh_fit)$summary %>% 
  as.data.frame(bh_fit) %>% 
  mutate(variable_mod = (names(bh_fit))) %>% 
  select(variable_mod, everything()) %>% 
  as_data_frame()  

# parameters  ======================  
# data_list - holds simulated values, this is from: simulate_data_age_structure.R
params<-bh_summary %>% 
  slice(1:8)

obs_dat <- data.frame(log_c_1  = c(data_list$log_c_1),
                      log_c_2 = c(data_list$log_c_2),
                      log_p_1 = c(data_list$log_p_1[1,1]),
                      log_p_2 = c(data_list$log_p_2[1,1]),
                      D_scale = c(data_list$D_scale),
                      prob_1 = c(data_list$prob[1]),
                      prob_2 = c(data_list$prob[2]),
                      prob_3 = c(data_list$prob[3])) %>% 
  gather(1:ncol(.), key = "variable", value = "mean_obs") %>%
  cbind(params) %>%  
  select(-variable_mod)

# plot ===========
obs_dat %>% 
  ggplot() + 
  geom_linerange(aes(variable, ymin = `2.5%`,ymax = `97.5%`)) + 
  geom_crossbar(aes(variable, mean, ymin = `25%`, ymax = `75%`), fill= 'grey') + 
  facet_wrap(~variable, scales = 'free') +
  geom_point(aes(variable, mean_obs), color = "red" ) #observed

# compare other posteriors ===========
## G ============
# gamma variates used in age structure 
obs_g <- as.data.frame(data_list$g) %>% 
  mutate(time = 1:nrow(.)) %>% 
  gather(c(1:4),key = "age", value = "variable") %>%
  separate(age, into = c("del", "age"), sep = 1) %>% 
  dplyr::select(-del)%>% 
  group_by(age) %>%
  dplyr::summarise(mean_obs = mean(variable),
                   variance_obs = var(variable))


bh_summary_g <- bh_summary %>% 
  slice(6:337) %>%
  separate(variable_mod, into = c("variable", "time", "del","age"), sep =c(-5,-3,-2,-1)) %>%
  dplyr::select(-del) %>%
  # separate(variable, into = c("variable", "del" ), sep =c(1)) %>%
  # dplyr::select(-del) %>% 
  dplyr::mutate(variable = (gsub("\\[", "", variable)),
                time = as.numeric((gsub("\\[", "", time)))) %>% 
  group_by(variable, age) %>%
  dplyr::summarise(mean_mod = mean(mean),
                   lower = mean(`2.5%`),
                   upper = mean(`97.5%`),
                   variance_mod = var(mean)) %>%
  left_join(obs_g)

bh_summary_g %>% 
  ggplot() + 
  geom_linerange(aes(age, ymin = lower,ymax = upper)) + 
  geom_point(aes(age, mean_mod ), fill= 'grey') + 
  geom_point(aes(age, mean_obs), color = "red" ) +  
  labs(caption = "Red is observed, black is model output") + 
  ggtitle("G")

bh_summary_g %>% 
  ggplot() + 
  geom_point(aes(age, variance_mod ), color = "black") + 
  geom_point(aes(age, variance_obs), color = "red" ) +  
  labs(caption = "Red is observed, black is model output") + 
  ggtitle("G")

# g plot not averaged ============= 
obs_g_all <- as.data.frame(data_list$g) %>% 
  dplyr::mutate(time = 1:nrow(.),
                id="obs") %>% 
  gather(c(1:4),key = "age", value = "variable") %>%
  separate(age, into = c("del", "age"), sep = 1) %>%
  dplyr::select(-del) 
# group_by(age) %>%
# dplyr::summarise(mean_obs = mean(variable))

bh_summary_g_all <- bh_summary %>% 
  slice(6:337) %>%
  separate(variable_mod, into = c("variable", "time", "del","age"), sep =c(-5,-3,-2,-1)) %>%
  dplyr::select(-del) %>%
  # separate(variable, into = c("variable", "del" ), sep =c(1)) %>%
  # dplyr::select(-del) %>% 
  dplyr::mutate(#variable = (gsub("\\[", "", variable)),
    variable = mean, 
    id="mod",
    time = as.numeric((gsub("\\[", "", time)))) %>% 
  dplyr::select(time,age,variable,id) %>%
  rbind(obs_g_all) %>%
  as.data.frame()

ggplot(data = bh_summary_g_all) + 
  geom_path(aes(x= time, y = variable, group = id, color = id)) + 
  #ggtitle("G") +
  facet_wrap(~age)

# dir_alpha ===============
dir_alpha_obs <- data.frame(dir_alpha_obs = data_list$Dir_alpha)  

bh_summary_alpha <- bh_summary %>% 
  filter(grepl("Dir_alpha",variable_mod)) %>%
  cbind(dir_alpha_obs)

bh_summary_alpha %>% 
  ggplot() + 
  geom_point(aes(variable_mod, mean ), fill= 'grey') + 
  geom_point(aes(variable_mod, dir_alpha_obs), color = "red" ) +#observed
  labs(caption = "Red is observed, black is model output")

# P ============
# age structures 
obs_p <- as.data.frame(data_list$p) %>% 
  mutate(time = 1:nrow(.)) %>% 
  gather(c(1:4),key = "age", value = "variable") %>%
  separate(age, into = c("del", "age"), sep = 1) %>% 
  dplyr::select(-del)%>% 
  group_by(age) %>%
  dplyr::summarise(mean_obs = mean(variable))

bh_summary_p <- bh_summary %>% 
  slice(1871:2202) %>%
  separate(variable_mod, into = c("variable", "time", "del","age"), sep =c(-5,-3,-2,-1)) %>%
  dplyr::select(-del) %>%
  dplyr::mutate(variable = (gsub("\\[", "", variable)),
                time = as.numeric((gsub("\\[", "", time)))) %>% 
  group_by(variable, age) %>%
  dplyr::summarise(mean_mod = mean(mean),
                   lower = mean(`2.5%`),
                   upper = mean(`97.5%`)) %>%
  left_join(obs_p)

bh_summary_p %>% 
  ggplot() + 
  geom_linerange(aes(age, ymin = lower,ymax = upper)) + 
  geom_point(aes(age, mean_mod ), fill= 'grey') + 
  geom_point(aes(age, mean_obs), color = "red" ) +#observed
  labs(caption = "Red is observed, black is model output")+
  ggtitle("P")


# Q ============
# compared to observed run composition in simulation and in model 
obs_q <- as.data.frame(data_list$o_run_comp) %>% 
  mutate(time = 1:nrow(.),
         id = "obs") %>% 
  gather(c(1:4),key = "age", value = "variable") %>%
  separate(age, into = c("del", "age"), sep = 1) %>% 
  dplyr::select(-del)  
# group_by(age) %>%
# dplyr::summarise(mean_obs = mean(variable))

bh_summary_q <- bh_summary %>% 
  filter(grepl("q",variable_mod)) %>%
  slice(-c(1:2)) %>%
  separate(variable_mod, into = c("variable", "time", "del","age"), sep =c(-5,-3,-2,-1)) %>%
  dplyr::select(-del,-variable) %>%
  dplyr::mutate(variable = mean, #variable = (gsub("\\[", "", variable)),
                id = "mod",
                time = as.numeric((gsub("\\[", "", time)))) %>%
  select(time, id, age, variable) %>% 
  # group_by(variable, age) %>%
  # dplyr::summarise(mean_mod = mean(mean),
  #                  lower = mean(`2.5%`),
  #                  upper = mean(`97.5%`)) %>%
  rbind(obs_q)

bh_summary_q %>% 
  ggplot() + 
  geom_path(aes(x=time, y = variable, group = id, color = id)) +
  facet_wrap(~age) + 
  labs(caption = "Red is observed, black is model output")+
  ggtitle("Q")

## Q avg ===========

obs_q <- as.data.frame(data_list$o_run_comp) %>% 
  mutate(time = 1:nrow(.),
         id = "obs") %>% 
  gather(c(1:4),key = "age", value = "variable") %>%
  separate(age, into = c("del", "age"), sep = 1) %>% 
  dplyr::select(-del) %>%
  group_by(id,age) %>%
  dplyr::summarise(mean = mean(variable))

bh_summary_q <- bh_summary %>% 
  filter(grepl("q",variable_mod)) %>%
  slice(-c(1:2)) %>%
  separate(variable_mod, into = c("variable", "time", "del","age"), sep =c(-5,-3,-2,-1)) %>%
  dplyr::select(-del,-variable) %>%
  dplyr::mutate(variable = mean, #variable = (gsub("\\[", "", variable)),
                id = "mod",
                time = as.numeric((gsub("\\[", "", time)))) %>%
  dplyr::select(time, id, age, variable) %>% 
  group_by(id, age) %>%
  dplyr::summarise(mean = mean(variable)) %>% #,
  # lower = mean(`2.5%`),
  #upper = mean(`97.5%`)) %>%
  rbind(obs_q)


bh_summary_q %>% 
  ggplot() + 
  # geom_linerange(aes(age, ymin = lower,ymax = upper)) + 
  geom_point(aes(age, mean, color = id, group =id )) + 
  # geom_point(aes(age, mean_obs), color = "red" ) +#observed
  #  labs(caption = "Red is observed, black is model output")+
  ggtitle("Q")

