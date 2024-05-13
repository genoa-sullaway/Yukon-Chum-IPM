library(tidyverse)
library(tidybayes)
library(here)
library(rstan)
library(bayesplot)
library(rstanarm)

# functions for tidying ===========
# Function to remove '[' character
remove_bracket <- function(lst) {
  sapply(lst, function(x) gsub("\\[", "", x))
}
remove_bracket2 <- function(lst) {
  sapply(lst, function(x) gsub("\\]", "", x))
}
remove_comma <- function(lst) {
  sapply(lst, function(x) gsub("\\,", "", x))
}

# load model ==============
bh_fit<- read_rds("output/stan_fit_DATA.RDS")

# traceplot ========
traceplot(bh_fit,pars=  c( "D_scale", "theta1[1]","theta1[2]","theta2[1]"))

traceplot(bh_fit,pars=  c("prob[1]", "prob[2]","prob[3]", "basal_p_1", "basal_p_2"))

traceplot(bh_fit,pars=  c("log_F","log_catch_q","g"))

# parameter plots ======== 

plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "D_scale", "theta1[1]","theta1[2]","theta2[1]"),
     fill_color = "blue")


plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "p_1"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "p_2"),
     fill_color = "blue")


plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
     pars=  c( "prob[1]", "prob[2]","prob[3]", "basal_p_1", "basal_p_2"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
     pars=  c(  "log_F" ),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
     pars=  c("log_catch_q"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
     pars=  c( "g"),
     fill_color = "blue")

# PLOT PARAMS  ======================  
# data_list - holds simulated values, this is from: simulate_data_age_structure.R
params <- summary(bh_fit, pars = c("log_c_1","log_c_2","log_catch_q", 
                                   "D_scale", "theta1", "theta2"), 
                  probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column() %>%
  dplyr::mutate(rowname = case_when(rowname == "theta1[1]"~ "theta1_1",
                                    rowname == "theta1[2]"~ "theta1_2",
                                    rowname == "theta2[1]"~ "theta2_1",
                             TRUE ~ rowname))

params %>% 
  ggplot() + 
  geom_linerange(aes(rowname, ymin = X10.,ymax = X90.)) + 
  geom_crossbar(aes(rowname, mean, ymin = X10.,ymax = X90.),  fill= 'grey') + 
  #geom_point(aes(x=rowname, y = mean_obs), color = "red") + 
  facet_wrap(~rowname, scales = 'free') +
  labs(caption = "red is observed, black is model")


# Plot Observed vs Predicted ========
## Spawners ==========
pred_N_SP <- summary(bh_fit, pars = c("N_sp"), 
              probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:21, each=4),
                age = rep(1:4, length.out = nrow(.)))

# plt proportions 


# sum to comarpe with data 
summ_n_sp <- pred_N_SP %>%
  group_by(time) %>%
  summarise(pred_n_sp = sum(mean),
            pred_se = mean(se_mean)) %>% 
  cbind(obs = data_list_stan$data_stage_sp)
  
ggplot(data = summ_n_sp) +
  geom_point(aes(x=time, y = obs)) +
  geom_line(aes(x=time, y = pred_n_sp)) +
  geom_ribbon(aes(x=time, ymin = pred_n_sp-pred_se,
                  ymax = pred_n_sp+pred_se))

## recruits ====== 
pred_N_recruit <- summary(bh_fit, pars = c("N_recruit"), 
                     probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:21, each=4),
                age = rep(1:4, length.out = nrow(.)))

# plt proportions 

# sum to compare with data 
summ_n_rec <- pred_N_recruit %>%
  group_by(time) %>%
  summarise(pred_n_rec = sum(mean),
            pred_se = mean(se_mean)) %>% 
  cbind(obs = data_list_stan$data_stage_return)

ggplot(data = summ_n_rec) +
  geom_point(aes(x=time, y = obs)) +
  geom_line(aes(x=time, y = pred_n_rec)) +
  geom_ribbon(aes(x=time, ymin = pred_n_rec-pred_se,
                  ymax = pred_n_rec+pred_se))

## harvest ====== 
pred_N_harvest <- summary(bh_fit, pars = c("N_catch"), 
                          probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:21, each=4),
                age = rep(1:4, length.out = nrow(.)))

# plt proportions 

# sum to compare with data 
summ_n_harvest <- pred_N_harvest %>%
  group_by(time) %>%
  summarise(pred_n_harvest = sum(mean),
            pred_se = mean(se_mean)) %>% 
  cbind(obs = data_list_stan$data_stage_harvest)

ggplot(data = summ_n_harvest) +
  geom_point(aes(x=time, y = obs)) +
  geom_line(aes(x=time, y = pred_n_harvest)) +
  geom_ribbon(aes(x=time, ymin = pred_n_harvest-pred_se,
                  ymax = pred_n_harvest+pred_se))




## juveniles ====== 
pred_N_j <- summary(bh_fit, pars = c("N_j"), 
                          probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = 1:nrow(.))
                
# plt proportions 

# sum to compare with data 
summ_n_j <- pred_N_j %>%
  group_by(time)  %>% 
  cbind(obs = data_list_stan$data_stage_j)

ggplot(data = summ_n_j) +
  geom_point(aes(x=time, y = obs)) +
  geom_line(aes(x=time, y = mean)) +
  geom_ribbon(aes(x=time, ymin = mean-se_mean,
                  ymax = mean+se_mean))


# OLD ===========================
# Plot Fishing Mortality ========
fm <- summary(bh_fit, pars = c("log_F"), 
                  probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = 1:nrow(.))

fm_dat <- data.frame(log_F = log(data_list_plot$F)) %>%
  gather(1:ncol(.), key = "rowname", value = "mean_obs")  

ggplot() + 
  geom_density(data = fm, aes(x=mean), fill ="black", alpha = 0.5) +
  geom_density(data = fm_dat, aes(x=mean_obs), fill ="darkgreen", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(fm$mean)), color = "black") +
  geom_vline(aes(xintercept = mean(fm_dat$mean_obs)),color = "darkgreen")

# plot prob =====
age_structure <- summary(bh_fit, pars = c("prob"), probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column() %>% 
  dplyr::mutate(rowname = case_when(rowname == "prob[1]"~ "prob_1",
                          rowname == "prob[2]"~ "prob_2",
                          rowname == "prob[3]"~ "prob_3"))
 
prob_obs <- data.frame(prob_1 = data_list_plot$`prob[1]`,
                       prob_2 = data_list_plot$`prob[2]`,
                       prob_3 = data_list_plot$`prob[3]`) %>% 
  gather(1:ncol(.), key = "rowname", value = "mean_obs")  %>%
  left_join(age_structure)

ggplot()+
  geom_point(data = prob_obs, aes(x=rowname, y = mean_obs), color = "red")+
  geom_point(data = prob_obs, aes(x=rowname, y = mean), color = "black") +
  labs(caption = "black is modeled, red is observed")



# plot n ocean prop =====
n_ocean_summary <- summary(bh_fit, pars = c("N_ocean"), probs = c(0.1, 0.9))$summary

n_ocean_prop<- n_ocean_summary %>%
  data.frame()%>%
  rownames_to_column() %>% 
  separate(rowname, into = c("variable", "time", "age" ), sep =c(7,-2))  %>%
  dplyr::mutate(time = as.numeric(remove_comma(remove_bracket(time))),
                age=as.numeric(remove_comma(remove_bracket2(age)))) %>%
  as.data.frame() %>% 
  # dplyr::select(-del) %>%
  #filter(!is.nan(mean)) %>% 
  group_by(time) %>%
  dplyr::mutate(sum = sum(mean),
                proportion = mean/sum)  %>%
  select(time,age,proportion)  

ggplot(data = n_ocean_prop) +
  geom_bar(aes(x=time, y=proportion, 
               fill = age, group = age), stat = "identity")

obs_p <- as.data.frame(data_list_plot$p)  

n_ocean_prop_mean <- n_ocean_prop %>%
  ungroup() %>%
  group_by(age) %>%
  dplyr::summarise(mean_prop = mean(proportion),
                   sd = sd(proportion)) %>%
  cbind(obs_p)

n_ocean_prop_mean %>% 
  ggplot() +  
  geom_point(aes(age, mean_prop), color = "red",alpha = 0.5 )  +
  geom_point(aes(x=age, y = `data_list_plot$p`), color = "black" ,alpha = 0.5) + 
  scale_y_continuous(limits = c(0,1)) 


# plot n recruit prop =====
# load recruit from output and calculate proportions to see if the age structure is the same there.... 
 n_recruit_summary <- summary(bh_fit, pars = c("N_recruit"), probs = c(0.1, 0.9))$summary

n_recruit_prop<- n_recruit_summary %>%
  data.frame()%>%
  rownames_to_column() %>% 
  separate(rowname, into = c("variable", "time", "age" ), sep =c(9,-2))  %>%
  dplyr::mutate(time = as.numeric(remove_comma(remove_bracket(time))),
                age=as.numeric(remove_comma(remove_bracket2(age)))) %>%
  as.data.frame() %>% 
 # dplyr::select(-del) %>%
  #filter(!is.nan(mean)) %>% 
  group_by(time) %>%
  dplyr::mutate(sum = sum(mean),
         proportion = mean/sum)  %>%
  select(time,age,proportion)  

ggplot(data =  n_recruit_prop) +
  geom_bar(aes(x=time, y=proportion, 
               fill = age, group = age), stat = "identity")

obs_p <- as.data.frame(data_list_plot$p)  

n_recruit_prop_mean <-  n_recruit_prop %>%
  ungroup() %>%
  group_by(age) %>%
  dplyr::summarise(mean_prop = mean(proportion),
                   sd = sd(proportion)) %>%
  cbind(obs_p)
 
n_recruit_prop_mean %>% 
  ggplot() +  
  geom_point(aes(age, mean_prop), color = "red",alpha = 0.5 )  +
  geom_point(aes(x=age, y = `data_list_plot$p`), color = "black" ,alpha = 0.5) + 
  scale_y_continuous(limits = c(0,1)) 

# plot n returning abundance =====
# load n_returning from output and calculate proportions to see if the age structure is the same there.... 
abund_recruit<-  n_recruit_summary %>%
  data.frame()%>%
  rownames_to_column() %>% 
  separate(rowname, into = c("variable", "time", "age" ), sep =c(9,-2))  %>%
  dplyr::mutate(time = remove_comma(remove_bracket(time)),
                age=remove_comma(remove_bracket2(age))) %>%
  as.data.frame() %>%  
  group_by(time) %>%
  select(time,age,mean) %>%
  mutate(time = as.numeric(time))%>% 
  filter(!time<10)

ggplot(data = abund_recruit) +
  geom_bar(aes(x=time, y=mean, 
               fill = age, group = age), stat = "identity", position = "stack")
 
# Plot N_sp proportions ====================
# load n_returning from output and calculate proportions to see if the age structure is the same there.... 
n_sp_summary <- summary(bh_fit, pars = c("N_sp"), probs = c(0.1, 0.9))$summary

n_sp_prop<- n_sp_summary %>%
  data.frame()%>%
  rownames_to_column() %>% 
  separate(rowname, into = c("variable", "time", "age" ), sep =c(4,-2))  %>%
  dplyr::mutate(time = as.numeric(remove_comma(remove_bracket(time))),
                age=as.numeric(remove_comma(remove_bracket2(age)))) %>%
  as.data.frame() %>% 
  # dplyr::select(-del) %>%
  filter(!is.nan(mean)) %>% 
  group_by(time) %>%
  dplyr::mutate(sum = sum(mean),
                proportion = mean/sum)  %>%
  select(time,age,proportion)  

ggplot(data = n_sp_prop) +
  geom_bar(aes(x=time, y=proportion, 
               fill = age, group = age), stat = "identity")

obs_p <- as.data.frame(data_list_plot$p)  

n_sp_prop_mean <- n_sp_prop %>%
  ungroup() %>%
  group_by(age) %>%
  dplyr::summarise(mean_prop = mean(proportion),
                   sd = sd(proportion)) %>%
  cbind(obs_p)

n_sp_prop_mean %>% 
  ggplot() +  
  geom_point(aes(age, mean_prop), color = "red",alpha = 0.5 )  +
  geom_point(aes(x=age, y = `data_list_plot$p`), color = "black" ,alpha = 0.5) + 
  scale_y_continuous(limits = c(0,1)) 

# Plot N_sp abundances ====================
abund_sp<-  n_sp_summary %>%
  data.frame()%>%
  rownames_to_column() %>% 
  separate(rowname, into = c("variable", "time", "age" ), sep =c(4,-2))  %>%
  dplyr::mutate(time = remove_comma(remove_bracket(time)),
                age=remove_comma(remove_bracket2(age))) %>%
  as.data.frame() %>%  
  group_by(time) %>%
  select(time,age,mean) %>%
  mutate(time = as.numeric(time))%>% 
  filter(!time<10)

ggplot(data = abund_sp) +
  geom_bar(aes(x=time, y=mean, 
               fill = age, group = age), stat = "identity", position = "stack")



# plot time series of predicted vs observed =========== 
#juveniles
obs_juv<-as.data.frame(data_list_plot$data_stage_j) %>%
  mutate(time = 1:nrow(.),
         obs_juv = data_list_plot$data_stage_j)
pred_juv <- summary(bh_fit, pars = c("N_j"), probs = c(0.1, 0.9))$summary

pred_juv_df <- pred_juv%>%
  data.frame()%>%
  rownames_to_column() %>% 
  separate(rowname, into = c("variable", "time", "del" ), sep =c(3,-1))  %>%
  dplyr::mutate(time = as.numeric(remove_comma(remove_bracket(time)))) %>%
  as.data.frame()  %>%
  dplyr::select(-del) %>%
  left_join(obs_juv)

ggplot(data=pred_juv_df) +
  geom_path(aes(x=time, y = mean)) +
  geom_ribbon(aes(x=time, ymin =X10., ymax = X90.)) + 
  geom_path(aes(x=time, y = obs_juv)) +
  theme_classic()

# recruit =======
  obs_recruit <- as.data.frame(data_list_plot$data_stage_return) %>%
    mutate(time = 1:nrow(.)) %>%
    rename(obs_recruit = "data_list_plot$data_stage_return")

mod_recruit<-  abund_recruit %>%
    group_by(time) %>%
    summarise(mean = sum(mean)) %>%
    left_join(obs_recruit) %>%
  gather(2:3, key = "key", value = "value")

testr <- mod_recruit %>%
  filter(!is.nan(value),
         !is.na(value)) %>%
  group_by(key) %>%
  summarise(se = sd(value)/sqrt(length(value)),
            var = var(value))

ggplot(data = mod_recruit) +
  geom_path(aes(x=time, y = value, group =key, color = key))


# Plot Compare Kappa obs and derived ============

kappa_obs <-   data.frame(kappa_j=c(data_list_plot$kappa_j),
                         kappa_m=c(data_list_plot$kappa_marine)) %>% 
  mutate(time = 1:nrow(.)) %>%
  gather(1:2, key = "variable", value = "obs_kappa")

kappa_mod <- summary(bh_fit, pars = c("kappa_marine", "kappa_j"), probs = c(0.1, 0.9))$summary

kappa_mod_df<- kappa_mod %>%
  data.frame()%>%
  rownames_to_column() %>% 
  separate(rowname, into = c("variable", "del", "time" ), sep =c(7,-3))  %>%
  select(-del) %>% 
  dplyr::mutate(time = as.numeric(remove_bracket2(remove_bracket(time)))) %>%
  as.data.frame()  %>%
  left_join(kappa_obs) %>% 
  select(time, variable, mean, obs_kappa) %>%
  gather(3:4, key = "id", value = "value") %>% 
  filter(!time <10)

ggplot(data =kappa_mod_df) +
  geom_point(aes(x=time, y =value, color = id)) + 
  facet_wrap(~variable, scales = "free")


# OLD Not updated with new extraction format ==============
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
# n sp =====
# load n_returning from output and calculate proportions to see if the age structure is the same there.... 
proportion_nsp<- bh_summary %>% 
  filter(grepl("N_sp",variable_mod),
         !grepl("start",variable_mod)) %>%
  separate(variable_mod, into = c("variable", "time", "age"), sep =c(4,-2))  %>%
  dplyr::mutate(time = remove_comma(remove_bracket(time)),
                age=remove_comma(remove_bracket2(age))) %>%
  as.data.frame() %>% 
  #dplyr::select(-del ) %>%
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
  separate(variable_mod, into = c("variable", "time", "age"), sep =c(3,6,-1))  %>%
  dplyr::mutate(time = remove_comma(remove_bracket(time)),
                age = remove_comma(remove_bracket2(age))) %>%
  as.data.frame() %>% 
 # dplyr::select( -del1) %>%
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
  cbind(params %>% filter(param == "D_scale") %>%
          dplyr::select(param, mean_mod)) %>% 
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
