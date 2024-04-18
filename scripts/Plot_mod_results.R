library(tidyverse)
library(tidybayes)
library(here)

library(actuaryr)
library(readxl)
library(rstan)
library(dirmult)
library(Rlab)

# translate simulated data_list to actual observations 
bh_fit<- read_rds("output/stan_fit_SIMULATED_OUTPUT.RDS")

bh_summary <- summary(bh_fit)$summary %>% 
  as.data.frame( ) %>% 
  mutate(variable_mod = (names(bh_fit))) %>% 
  select(variable_mod, everything()) %>% 
  as_data_frame()  

# parameters  ======================  
# data_list - holds simulated values, this is from: simulate_data_age_structure.R
params<-bh_summary %>% 
  slice(1:4,10) %>% 
  separate(variable_mod, into=c("param", "delete"), sep = -5) %>%
  dplyr::select(-delete) %>%
  rbind(bh_summary %>% 
          slice(5) %>% rename(param = "variable_mod")) %>%
  dplyr::mutate(mean_mod=mean) %>%
  dplyr::select(param, mean_mod,`2.5%`,`25%`,`75%`,`97.5%`)

 dat<-data.frame(log_c_1 = data_list$log_c_1,
                    log_c_2 = data_list$log_c_2,
                    log_catch_q = data_list$catch_q,
                    log_p_1 = data_list$log_p_1,
                    log_p_2 = data_list$log_p_2,
                    D_scale = data_list$D_scale#, 
                    #g = data_list$g
                 ) %>%
  gather(1:ncol(.), key = "param", value = "mean_obs") %>%
  left_join(params)

dat %>% 
  ggplot() + 
  geom_linerange(aes(param, ymin = `2.5%`,ymax = `97.5%`)) + 
  geom_crossbar(aes(param, mean_mod, ymin = `25%`, ymax = `75%`), fill= 'grey') + 
  geom_point(aes(x=param, y = mean_obs), color = "red") + 
  facet_wrap(~param, scales = 'free') +
  labs(caption = "red is observed, black is model")


# Q ============
# compared to observed run composition in simulation and in model 
obs_run_comp <- as.data.frame(colMeans(data_list$o_run_comp)) %>% 
  dplyr::rename(obs_run_comp = `colMeans(data_list$o_run_comp)`)

bh_summary_q <- bh_summary %>% 
  filter(grepl("q",variable_mod)) %>%
  slice(-c(1:2)) %>% 
  separate(variable_mod, into = c("variable", "time", "del","age"), sep =c(-5,-3,-2,-1)) %>%
  dplyr::select(-del,-variable) %>%
  dplyr::mutate(variable = mean, #variable = (gsub("\\[", "", variable)),
                id = "mod",
                time = as.numeric((gsub("\\[", "", time)))) %>%
  dplyr::select(time, id, age, variable,6,10) %>% 
  group_by(id, age) %>%
  dplyr::summarise(mean_mod = mean(variable),
                   lower = mean(`2.5%`),
                   upper = mean(`97.5%`)) %>%
  cbind(obs_run_comp)

bh_summary_q %>% 
  ggplot() + 
  geom_linerange(aes(age, ymin = lower,ymax = upper)) + 
  geom_point(aes(age, mean_mod ), fill= 'grey') + 
  geom_point(aes(age, obs_run_comp), color = "red" ) +  
  labs(caption = "Red is observed, black is model output") + 
  ggtitle("Q")

# through time 
bh_q <- bh_summary %>% 
  filter(grepl("q",variable_mod)) %>%
  slice(-c(1:2)) %>% 
  separate(variable_mod, into = c("variable", "time", "del","age"), sep =c(-5,-3,-2,-1)) %>%
  dplyr::select(-del,-variable) %>%
  dplyr::mutate(variable = mean, #variable = (gsub("\\[", "", variable)),
                id = "mod",
                time = as.numeric((gsub("\\[", "", time)))) %>%
  dplyr::select(time, age, variable) %>%
  spread(age, variable)

 
barplot(t(bh_q[,2:5]))

# Function to remove '[' character
remove_bracket <- function(lst) {
  sapply(lst, function(x) gsub("\\[", "", x))
}
remove_comma <- function(lst) {
  sapply(lst, function(x) gsub("\\,", "", x))
}
 
# n returning =====
# load n_returning from output and calculate proportions to see if the age structure is the same there.... 
proportion_q<- bh_summary %>% 
  filter(grepl("N_returning",variable_mod),
         !grepl("start",variable_mod)) %>%
  separate(variable_mod, into = c("variable", "time", "age", "del" ), sep =c(11,-4,-2))  %>%
  dplyr::mutate(time = remove_comma(remove_bracket(time)),
                age=remove_comma(age)) %>%
  as.data.frame() %>% 
  dplyr::select(-del) %>%
  filter(!is.nan(mean)) %>% 
  group_by(time) %>%
  dplyr::mutate(sum = sum(mean),
         proportion = mean/sum)  %>%
  select(time,age,proportion) %>%
  mutate(time = as.numeric(time))

ggplot(data = proportion_q) +
  geom_bar(aes(x=time, y=proportion, 
               fill = age, group = age), stat = "identity")

proportion_q_time <- proportion_q %>%
  ungroup() %>%
  group_by(age) %>%
  dplyr::summarise(mean_prop = mean(proportion),
                   sd = sd(proportion))

proportion_q_time %>% 
  ggplot() +  
  geom_point(aes(age, mean_prop), color = "red" )  +
  scale_y_continuous(limits = c(0,1))


# n sp =====
# load n_returning from output and calculate proportions to see if the age structure is the same there.... 
proportion_nsp<- bh_summary %>% 
  filter(grepl("N_sp",variable_mod),
         !grepl("start",variable_mod)) %>%
  separate(variable_mod, into = c("variable", "time", "age", "del" ), sep =c(4,-4,-2))  %>%
  dplyr::mutate(time = remove_comma(remove_bracket(time)),
                age=remove_comma(age)) %>%
  as.data.frame() %>% 
  dplyr::select(-del ) %>%
  filter(!is.nan(mean)) %>% 
  group_by(time) %>%
  dplyr::mutate(sum = sum(mean),
                proportion = mean/sum)  %>%
  select(time,age,proportion) %>%
  mutate(time = as.numeric(time))

ggplot(data = proportion_nsp) +
  geom_bar(aes(x=time, y=proportion, 
               fill = age, group = age), stat = "identity")
 

# n eggs =====
# load n_returning from output and calculate proportions to see if the age structure is the same there.... 
proportion_n_e<- bh_summary %>% 
  filter(grepl("N_e",variable_mod),
         !grepl("sum",variable_mod),
         !grepl("start",variable_mod)) %>%
  separate(variable_mod, into = c("variable", "time", "age", "del1"), sep =c(3,6,-3))  %>%
  dplyr::mutate(time = remove_comma(remove_bracket(time)),
                age = remove_comma(age)) %>%
  as.data.frame() %>% 
  dplyr::select( -del1) %>%
  filter(!is.nan(mean)) %>% 
  group_by(time) %>%
  dplyr::mutate(sum = sum(mean),
                proportion = mean/sum)  %>%
  select(time,age,proportion) %>%
  mutate(time = as.numeric(time))

ggplot(data = proportion_n_e) +
  geom_bar(aes(x=time, y=proportion, 
               fill = age, group = age), stat = "identity") +
  ggtitle("N_e")

# n recruit =====
n_recruit<- bh_summary %>% 
  filter(grepl("N_recruit",variable_mod),
         !grepl("sum",variable_mod),
         !grepl("start",variable_mod)) %>%
  separate(variable_mod, into = c("variable","del1", "time", "del"), sep =c(9,10,-2,-1))  %>%
  dplyr::mutate(#time = remove_bracket(time),
                time = as.numeric(remove_comma(time))) %>%
  as.data.frame() %>% 
  dplyr::select(-del, -del1) %>%
  filter(!is.nan(mean)) 

ggplot(data = n_recruit) +
  geom_point(aes(x=time, y = mean)) +
  geom_errorbar(aes(x=time, ymin = `2.5%`, ymax=`97.5%`))

# G ============
# gamma variates used in age structure 
obs_g <- as.data.frame(data_list$g)  

bh_summary_g <- bh_summary %>% 
  slice(6:9) %>%
  separate(variable_mod, into = c("variable", "del"), sep =c(-3)) %>%
  dplyr::select(-del) %>%
  dplyr::mutate(mean_mod =  mean,
                age = 1:nrow(.)) %>%
  dplyr::select(1,5,9,12,13) %>%
  cbind(obs_g) %>%
  rename(obs_g= `data_list$g`,
         lower= `2.5%`,
         upper = `97.5%`) 

bh_summary_g %>% 
  ggplot() + 
  geom_linerange(aes(age, ymin = lower,ymax = upper)) + 
  geom_point(aes(age, mean_mod ), fill= 'grey') + 
  geom_point(aes(age, obs_g), color = "red" ) +  
  labs(caption = "Red is observed, black is model output") + 
  ggtitle("G")
 
# bh_summary_g <- bh_summary %>% 
#   slice(6:337) %>%
#   separate(variable_mod, into = c("variable", "time", "del","age"), sep =c(-5,-3,-2,-1)) %>%
#   dplyr::select(-del) %>%
#   # separate(variable, into = c("variable", "del" ), sep =c(1)) %>%
#   # dplyr::select(-del) %>% 
#   dplyr::mutate(variable = (gsub("\\[", "", variable)),
#                 time = as.numeric((gsub("\\[", "", time)))) %>% 
#   group_by(variable, age) %>%
#   dplyr::summarise(mean_mod = mean(mean),
#                    lower = mean(`2.5%`),
#                    upper = mean(`97.5%`),
#                    variance_mod = var(mean)) %>%
#   left_join(obs_g)

# bh_summary_g %>% 
#   ggplot() + 
#   geom_linerange(aes(age, ymin = lower,ymax = upper)) + 
#   geom_point(aes(age, mean_mod ), fill= 'grey') + 
#   geom_point(aes(age, mean_obs), color = "red" ) +  
#   labs(caption = "Red is observed, black is model output") + 
#   ggtitle("G")

# variance
# bh_summary_g %>% 
#   ggplot() + 
#   geom_point(aes(age, variance_mod ), color = "black") + 
#   geom_point(aes(age, variance_obs), color = "red" ) +  
#   labs(caption = "Red is observed, black is model output") + 
#   ggtitle("G")

# g plot not averaged ============= 
obs_g_all <- as.data.frame(data_list$g) %>% 
  dplyr::mutate(time = 1:nrow(.),
                id="obs") %>% 
  gather(c(1:4),key = "age", value = "variable") %>%
  separate(age, into = c("del", "age"), sep = 1) %>%
  dplyr::select(-del) 

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
  labs(caption = "Red is observed, black is model output") +
  ggtitle("Dir_alpha")

# P ============
# age structures 

obs_p <- as.data.frame(data_list$p)  

bh_summary_p <- bh_summary %>% 
  filter(variable_mod %in% c( "p[1]","p[2]","p[3]","p[4]" )) %>% 
  cbind(obs_p) %>%
  #slice(6:9) %>%
  #separate(variable_mod, into = c("variable", "del"), sep =c(-3)) %>%
  dplyr::select(1,2,5,9,12) 

bh_summary_p %>% 
  ggplot() + 
  geom_linerange(aes(variable_mod, ymin = `2.5%`,ymax = `97.5%`)) + 
  geom_point(aes(variable_mod, mean), fill= 'grey') + 
  geom_point(aes(variable_mod, `data_list$p`), color = "red", alpha = 0.5 ) +  
  labs(caption = "Red is observed, black is model output") + 
  ggtitle("P")

# 
# obs_p <- as.data.frame(data_list$p) %>% 
#   mutate(time = 1:nrow(.)) %>% 
#   gather(c(1:4),key = "age", value = "variable") %>%
#   separate(age, into = c("del", "age"), sep = 1) %>% 
#   dplyr::select(-del)%>% 
#   group_by(age) %>%
#   dplyr::summarise(mean_obs = mean(variable))
# 
# bh_summary_p <- bh_summary %>% 
#   slice(1871:2202) %>%
#   separate(variable_mod, into = c("variable", "time", "del","age"), sep =c(-5,-3,-2,-1)) %>%
#   dplyr::select(-del) %>%
#   dplyr::mutate(variable = (gsub("\\[", "", variable)),
#                 time = as.numeric((gsub("\\[", "", time)))) %>% 
#   group_by(variable, age) %>%
#   dplyr::summarise(mean_mod = mean(mean),
#                    lower = mean(`2.5%`),
#                    upper = mean(`97.5%`)) %>%
#   left_join(obs_p)
# 
# bh_summary_p %>% 
#   ggplot() + 
#   geom_linerange(aes(age, ymin = lower,ymax = upper)) + 
#   geom_point(aes(age, mean_mod ), fill= 'grey') + 
#   geom_point(aes(age, mean_obs), color = "red" ) +#observed
#   labs(caption = "Red is observed, black is model output")+
#   ggtitle("P")

# D_scale ============
d_scale <- as.data.frame(data_list$D_scale) %>% 
  cbind(params %>% filter(variable_mod == "D_scale") %>%
          dplyr::select(variable_mod, mean, se_mean)) %>% 
  rename(d_scale_obs=`data_list$D_scale`) %>%
  select(-2) %>%
  gather(1:2, key = "param", value ="value") %>%
  dplyr::mutate(param = case_when(param == "mean" ~ "model",
                           TRUE ~ "obs"),
                d_sum = 1/(value^2) )

d_scale %>% 
  ggplot() + 
  #geom_linerange(aes(age, ymin = lower,ymax = upper)) + 
  geom_point(aes(param, value,color =param) ) + 
  # geom_point(aes(age, mean_obs), color = "red" ) +#observed
  # labs(caption = "Red is observed, black is model output")+
  ggtitle("D scale")
