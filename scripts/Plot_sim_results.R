library(tidyverse)
library(tidybayes)
library(here)
library(rstan)
library(bayesplot)
library(rstanarm)
# pairs with stan mod BH SIM script and simulate_data_age_strucutre.R script

# PLOT data =======
data_list_plot <-    list(nByrs=nByrs_stan,
                                          nRyrs=nRyrs_stan,
                                          nRyrs_T = nRyrs_T_stan, 
                                          A=A,
                                          t_start = t_start,
                          
                                          Dir_alpha=Dir_alpha,
                                          D_scale = D_scale,
                                          prob = prob, 
                                          pi = pi, 
                                          Ps=Ps,
                                          fs=fs,
                                          M = M_fill_stan, 
                                          F=F,
                                          log_c_1 = log_c_1,
                                          log_c_2=log_c_2,
                                          p_1=p_1,
                                          p_2=p_2,
                                          theta1=theta1,
                                          theta2=theta2,
                                          cov_eff1=cov_eff1,
                                          cov_eff2=cov_eff2,
                          
                                          data_stage_j = N_j_sim_hat,#[6:nByrs+1],
                                          data_stage_return = N_brood_year_return_sim,#[6:nRyrs+2],
                                          data_stage_recruit = N_recruit,#[6:nRyrs+2],
                                          data_stage_sp = N_sp_sim_s,#[6:nRyrs+2],
                                          data_stage_harvest = N_catch_sim_s,#[6:nRyrs+2], 
                                          kappa_j = kappa_j,
                                          kappa_marine = kappa_marine,
                                          # kappa_marine_mortality = kappa_marine_mortality,
                                          # kappa_marine_mort_start = c(-log(basal_p_2), -log(basal_p_2)),                
                                          # kappa_marine_start = c(basal_p_2, basal_p_2), 
                                          #  kappa_j_start = basal_p_1,
                                          
                                          basal_p_1 = basal_p_1,
                                          basal_p_2 = basal_p_2,
                                          
                                          ncovars1=ncovars1,
                                          ncovars2=ncovars2,
                                          # log_S = log_S, 
                                          sigma_y_j=process_error_j, 
                                          g=g, 
                                          p =p,
                                          o_run_comp=o_run_comp,#[8:nByrs,],
                                          ess_age_comp=ess_age_comp)#[8:(nByrs-1)] )


 
# load model ==============
bh_fit<- read_rds("output/stan_fit_SIMULATED_OUTPUT.RDS")

# traceplot ========
traceplot(bh_fit,pars=  c( "theta1[1]" ,
                           "theta2[1]" ))

traceplot(bh_fit,pars=  c( "D_scale" ))

traceplot(bh_fit,pars=  c( "basal_p_1","basal_p_2"))

traceplot(bh_fit,pars=  c( "log_catch_q" ))

traceplot(bh_fit,pars=  c(  "Dir_alpha"))
 
traceplot(bh_fit,pars=  c("prob[1]", "prob[2]","prob[3]"#, 
                          #"basal_p_1", "basal_p_2"
                          ))

traceplot(bh_fit,pars=  c("N_sp_start_log"))

traceplot(bh_fit,pars=  c("log_c_1","log_c_2"))

traceplot(bh_fit,pars=  c("p_1","p_2"))

traceplot(bh_fit,pars=  c("g"))

traceplot(bh_fit,pars=  c("sigma_y_j"))

traceplot(bh_fit,pars=  c("log_F"))

traceplot(bh_fit,pars=  c("D_sum"))

traceplot(bh_fit,pars=  c("Dir_alpha"))

# pairs======
pairs(bh_fit, pars= c("prob" ))

pairs(bh_fit, pars= c("g" ))

pairs(bh_fit, pars= c("log_F" ))
 
pairs(bh_fit, pars= c( "log_catch_q","prob" ))

stan_par(bh_fit, par = c("sigma_y_j"))

# parameter plots ======== 
plot(bh_fit, show_density = TRUE, ci_level = 0.95, 
     pars=  c( "theta1[1]", 
               "theta2[1]" ),
     fill_color = "blue")
 
plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "log_F_mean"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "sigma_catch"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "N_recruit_start_log"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "log_S"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "prob"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "pi"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "p"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "p_1", "p_2"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "log_F_dev_y"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95, 
     pars=  c( "F"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
     pars=  c( "prob[1]", "prob[2]","prob[3]", "basal_p_1", "basal_p_2"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,
     pars=  c(  "log_c_1" ),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,
     pars=  c(  "log_c_2" ),
     fill_color = "blue")
 
plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
     pars=  c("log_catch_q"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
     pars=  c("sigma_y_j"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
     pars=  c( "Dir_alpha"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
     pars=  c( "D_sum"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
     pars=  c( "g"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,  
     pars=  c( "D_scale"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,
     pars=  c("N_j_start_log",
              "N_sp_start_log",
              "N_egg_start_log",
              "N_egg_sum_start_log",
              "N_recruit_start_log"),
     fill_color = "blue")

plot(bh_fit, show_density = FALSE, ci_level = 0.95,
     pars=  c("N_egg_sum_start_log",
              "N_recruit_start_log"),
     fill_color = "blue")
 
# Plot Observed vs Predicted ========
## Spawners ==========
pred_N_SP <- summary(bh_fit, pars = c("N_sp"), 
                     probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:24, each=4),
                age = rep(1:4, length.out = nrow(.)))# %>% 
  # filter(!time > 21)

# plot proportions 
# sum to compare with data 
summ_n_sp <- pred_N_SP %>%
  group_by(time) %>%
  summarise(pred_n_sp = sum(mean),
            pred_se = mean(se_mean)) %>% 
   filter(!time >21) %>% 
    cbind(obs = data_list_plot$data_stage_sp)  
   
ggplot(data = summ_n_sp) +
  geom_point(aes(x=time, y = obs), color = "red") +
  geom_line(aes(x=time, y = pred_n_sp)) +
  geom_ribbon(aes(x=time, ymin = pred_n_sp-pred_se,
                  ymax = pred_n_sp+pred_se))
 
# ## recruits ====== 
# pred_N_recruit <- summary(bh_fit, pars = c("N_recruit"), 
#                           probs = c(0.1, 0.9))$summary %>%
#   data.frame() %>%
#   rownames_to_column()  %>%
#   dplyr::mutate(time = rep(1:25, each=4),
#                 age = rep(1:4, length.out = nrow(.))) 
# 
# summ_n_rec <- pred_N_recruit %>%
#   group_by(time) %>%
#   summarise(pred = sum(mean),
#             pred_se = mean(se_mean)) %>% 
#   filter(!time >21) %>% 
#   cbind(obs = data_list_plot$data_stage_recruit) 
# 
# ggplot(data = summ_n_rec) +
#   geom_point(aes(x=time, y = obs), color = "red") +
#   geom_line(aes(x=time, y = pred)) +
#   geom_ribbon(aes(x=time, ymin = pred-pred_se,
#                   ymax = pred+pred_se))+
#   ggtitle(("Recruits, est and observed"))

## harvest ====== 
pred_N_harvest <- summary(bh_fit, pars = c("N_catch"), 
                          probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:24, each=4),
                age = rep(1:4, length.out = nrow(.))) 

summ_n_harvest <- pred_N_harvest %>%
  group_by(time) %>%
  summarise(pred = sum(mean),
            pred_se = mean(se_mean)) %>% 
  filter(!time >21) %>% 
  cbind(obs = data_list_plot$data_stage_harvest) 

ggplot(data = summ_n_harvest) +
  geom_point(aes(x=time, y = obs), color = "red") +
  geom_line(aes(x=time, y = pred)) +
  geom_ribbon(aes(x=time, ymin = pred-pred_se,
                  ymax = pred+pred_se))+
  ggtitle(("Harvest, est and observed"))

## retuns ===========

pred_N_byr <- summary(bh_fit, pars = c("N_brood_year_return"), 
                    probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = 1:nrow(.)) %>% 
  cbind(obs = data_list_stan$data_stage_return)  

ggplot(data = pred_N_byr) +
  geom_point(aes(x=time, y = obs), color = "red") +
  geom_line(aes(x=time, y = mean)) +
  geom_ribbon(aes(x=time, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5)+
  ggtitle(("Brood year returns, est and observed"))

## Juveniles ====== 
pred_N_j <- summary(bh_fit, pars = c("N_j_predicted"), 
                    probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = 1:nrow(.))

# plot proportions 
# sum to compare with data 
summ_n_j <- pred_N_j %>%
  # dplyr::mutate(mean_J_Q = mean*catch_q$mean,
  #               se_mean = se_mean*catch_q$mean) %>% 
  cbind(obs = data_list_stan$data_stage_j) # %>%
 # filter(!time < 7)

ggplot(data = summ_n_j) +
  geom_point(aes(x=time, y = obs), color = "red") +
  geom_line(aes(x=time, y = mean)) +
  geom_ribbon(aes(x=time, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5)+
  ggtitle(("Juveniles, est and observed"))

# plot pi  =================
pi_df <- summary(bh_fit, pars = c("pi"), 
                   probs = c(0.1, 0.9))$summary %>% 
  data.frame() %>%
  rownames_to_column() %>% 
  cbind( data.frame(obs= data_list_plot$pi)) 

ggplot(data= pi_df) +
  geom_point(aes(x=rowname, y = mean  )) +
  geom_point(aes(x=rowname, y = obs), color = "red", alpha = 0.5) 

# plot d scale =================
D_scale_df <- summary(bh_fit, pars = c("D_scale"), 
                        probs = c(0.1, 0.9))$summary %>% 
  data.frame() %>%
  rownames_to_column() %>% 
  cbind( data.frame(obs= data_list_plot$D_scale)) 

ggplot(data= D_scale_df) +
  geom_point(aes(x=rowname, y = mean  ),size = 2 ) +
  geom_errorbar(aes(x = rowname, ymin = X10., ymax = X90.), width = 0.1,alpha = 0.5) + 
  geom_point(aes(x=rowname, y = obs), color = "red", size = 2 ) +
  labs(caption= "Red is observed, black is predicted")

# plot dir alpha  =================
dir_alpha_df <- summary(bh_fit, pars = c("Dir_alpha"), 
                 probs = c(0.1, 0.9))$summary %>% 
  data.frame() %>%
  rownames_to_column() %>% 
  cbind( data.frame(obs= data_list_plot$Dir_alpha)) 

ggplot(data= dir_alpha_df) +
  geom_point(aes(x=rowname, y = mean  ),size = 2 ) +
  geom_errorbar(aes(x = rowname, ymin = X10., ymax = X90.), width = 0.1,alpha = 0.5) + 
  geom_point(aes(x=rowname, y = obs), color = "red", size = 2 ) +
  labs(caption= "Red is observed, black is predicted")

# plot PROB  =================
prob_df <- summary(bh_fit, pars = c("prob"), 
                      probs = c(0.1, 0.9))$summary %>% 
  data.frame() %>%
  rownames_to_column() %>% 
  cbind( data.frame(obs= data_list_plot$prob)) 

ggplot(data= prob_df) +
  geom_point(aes(x=rowname, y = mean  ),size = 2 ) +
  geom_errorbar(aes(x = rowname, ymin = X10., ymax = X90.), width = 0.1,alpha = 0.5) + 
  geom_point(aes(x=rowname, y = obs), color = "red", size = 2 ) +
  labs(caption= "Red is observed, black is predicted")

# plot G  =================
g_df <- summary(bh_fit, pars = c("g"), 
                   probs = c(0.1, 0.9))$summary %>% 
  data.frame() %>%
  rownames_to_column() %>% 
  dplyr::mutate(time = rep(1:20, each=4),
                age = rep(1:4, length.out = nrow(.))) %>%
  dplyr::rename(pred = "mean") %>%
  left_join(data.frame(  obs= data_list_plot$g) %>% 
           dplyr::mutate(time = 1:nrow(.)) %>% 
           gather(1:4, key = "age",value = "obs") %>% 
           mutate(age = case_when(age == "obs.1" ~ 1,
                                  age == "obs.2" ~ 2,
                                  age == "obs.3" ~ 3,
                                  age == "obs.4" ~ 4)))

ggplot(data= g_df) +
  geom_line(aes(x=time, y = pred, group = age )) +
  geom_point(aes(x=time, y = obs, group = age ), color = "red", alpha = 0.5) +
  geom_line(aes(x=time, y = obs, group = age ), color = "red", alpha = 0.5) +
  facet_wrap(~age, scales = "free")+
  labs(caption= "Red is observed, black is predicted")

# plot Q obs age comp through time =================
age_comp_Q <- summary(bh_fit, pars = c("q"), 
                      probs = c(0.1, 0.9))$summary %>% 
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:21, each=4),
                age = rep(1:4, length.out = nrow(.))) %>%
  dplyr::rename(pred = "mean") %>%
  dplyr::select(time,age,pred)  %>%
left_join(data.frame(data_list_plot$o_run_comp) %>% 
            dplyr::mutate(time = 1:nrow(.)) %>% 
            gather(1:4, key = "age",value = "obs") %>% 
            mutate(age = case_when(age == "X1" ~ 1,
                                   age == "X2" ~ 2,
                                   age == "X3" ~ 3,
                                   age == "X4" ~ 4)))
  
ggplot(data= age_comp_Q) +
  geom_line(aes(x=time, y = pred, group = age )) +
  geom_point(aes(x=time, y = obs, group = age ), color = "red", alpha = 0.5) +
  geom_line(aes(x=time, y = obs, group = age ), color = "red", alpha = 0.5) +
  facet_wrap(~age, scales = "free")+
  labs(caption= "Red is observed, black is predicted")
 
# plot P through time =================
# proportion of maturing indviduals

## or age comp through time: =====
age_comp_P <- summary(bh_fit, pars = c("p"), 
                      probs = c(0.1, 0.9))$summary %>% 
  data.frame() %>%
  rownames_to_column()  %>%
  dplyr::mutate(time = rep(1:20, each=4),
                age = rep(1:4, length.out = nrow(.))) %>%
  dplyr::rename(pred = "mean") %>%
  dplyr::select(time,age,pred)  %>%
  arrange(time,age) %>% 
  cbind( t<-data.frame(data_list_plot$p) %>% 
           dplyr::mutate(time = 1:nrow(.)) %>% 
           gather(1:4, key = "age",value = "obs") %>% 
           arrange(time,age)) %>%
  dplyr::select(1:3,6)

ggplot(data= age_comp_P) +
  geom_line(aes(x=time, y = pred, group = age )) +
  geom_line(aes(x=time, y = obs, group = age ), color = "red", alpha = 0.5) +
  facet_wrap(~age , scales = "free")

# plot theta  =================
theta1 <- summary(bh_fit, pars = c("theta1"), 
                   probs = c(0.1, 0.9))$summary %>% 
  data.frame() %>%
  rownames_to_column() %>% 
  cbind( data.frame(obs= data_list_plot$theta1)) 

ggplot(data= theta1) +
  geom_point(aes(x=rowname, y = mean  ),size = 2 ) +
  geom_errorbar(aes(x = rowname, ymin = X10., ymax = X90.), width = 0.1,alpha = 0.5) + 
  geom_point(aes(x=rowname, y = obs), color = "red", size = 2 ) +
  labs(caption= "Red is observed, black is predicted")

theta2 <- summary(bh_fit, pars = c("theta2"), 
                  probs = c(0.1, 0.9))$summary %>% 
  data.frame() %>%
  rownames_to_column() %>% 
  cbind( data.frame(obs= data_list_plot$theta2)) 

ggplot(data= theta2) +
  geom_point(aes(x=rowname, y = mean  ),size = 2 ) +
  geom_errorbar(aes(x = rowname, ymin = X10., ymax = X90.), width = 0.1,alpha = 0.5) + 
  geom_point(aes(x=rowname, y = obs), color = "red", size = 2 ) +
  labs(caption= "Red is observed, black is predicted")


# plot basal p  =================
## basal p1 ============
basal_p_1 <- summary(bh_fit, pars = c("basal_p_1"), 
                  probs = c(0.1, 0.9))$summary %>% 
  data.frame() %>%
  rownames_to_column() %>% 
  cbind( data.frame(obs= data_list_plot$basal_p_1)) 

ggplot(data= basal_p_1) +
  geom_point(aes(x=rowname, y = mean  ),size = 2 ) +
  geom_errorbar(aes(x = rowname, ymin = X10., ymax = X90.), width = 0.1,alpha = 0.5) + 
  geom_point(aes(x=rowname, y = obs), color = "red", size = 2 ) +
  labs(caption= "Red is observed, black is predicted")

## basal p2 ============
basal_p_2 <- summary(bh_fit, pars = c("basal_p_2"), 
                  probs = c(0.1, 0.9))$summary %>% 
  data.frame() %>%
  rownames_to_column() %>% 
  cbind( data.frame(obs= data_list_plot$basal_p_2)) 

ggplot(data= basal_p_2) +
  geom_point(aes(x=rowname, y = mean  ),size = 2 ) +
  geom_errorbar(aes(x = rowname, ymin = X10., ymax = X90.), width = 0.1,alpha = 0.5) + 
  geom_point(aes(x=rowname, y = obs), color = "red", size = 2 ) +
  labs(caption= "Red is observed, black is predicted")

# estimated fishing mortality ======
fishing <- summary(bh_fit, pars = c("F"), 
                   probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>% 
  mutate(mean =  (mean),
         time = 1:nrow(.)) %>% 
  left_join( data.frame(obs= data_list_plot$F) %>% 
           dplyr::mutate(time = 1:nrow(.))) #%>%
  # filter(!time>21 & !time <5)
 
ggplot(data = fishing) + 
  geom_line(aes(x=time, y = mean)) + 
  geom_line(aes(x=time, y = obs),color = "red", alpha = 0.5) + 
  ylab("Instantaneous fishing mortality")+
  labs(caption = "red is obs, black is predicted")

# Plot selectivity ======
# S <- summary(bh_fit, pars = c("log_S"), 
#              probs = c(0.1, 0.9))$summary %>%
#   data.frame() %>%
#   rownames_to_column()    %>%
# cbind( data.frame(obs= data_list_plot$log_S) %>% 
# dplyr::mutate(age = 1:nrow(.))) #%>%
# 
# ggplot(data = S) + 
#   geom_point(aes(x=rowname, y = mean)) + 
#   geom_point(aes(x=rowname, y = obs), color = "red") + 
#   ylab("Log Selectivity") 

# plot productivity ======
## p1 =======
p_1 <- summary(bh_fit, pars = c("p_1"), 
                                probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>% 
  dplyr::mutate(time = rep(1:20, length.out = nrow(.))) %>% 
  cbind(obs = data_list_plot$p_1)


ggplot(data = p_1,aes(x=time, y = mean)) + 
  geom_line( ) +
  geom_point(aes(x=time,y=obs), color = "red")+
  geom_ribbon(aes(x=time, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5) + 
  scale_x_continuous(breaks = c(2002,2006,2010, 2015,2020, 2022))
 
## p2 =======
p_2 <- summary(bh_fit, pars = c("p_2"), 
               probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>% 
  dplyr::mutate(time = rep(1:20, length.out = nrow(.))) %>% 
  cbind(obs = data_list_plot$p_2)

ggplot(data = p_2,aes(x=time, y = mean)) + 
  geom_line( ) +
  geom_point(aes(x=time,y=obs), color = "red")+
  geom_ribbon(aes(x=time, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5) + 
  scale_x_continuous(breaks = c(2002,2006,2010, 2015,2020, 2022))

# plot cov effect ======
## cov eff 1 =======
cov_eff1 <- summary(bh_fit, pars = c("cov_eff1"), 
               probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>% 
  dplyr::mutate(time = rep(1:20, length.out = nrow(.))) %>% 
  cbind(obs = data_list_plot$cov_eff1)


ggplot(data = cov_eff1,aes(x=time, y = mean)) + 
  geom_line( ) +
  geom_point(aes(x=time,y=obs), color = "red")+
  geom_ribbon(aes(x=time, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5) + 
  scale_x_continuous(breaks = c(2002,2006,2010, 2015,2020, 2022))

## cov_eff2 =======
cov_eff2 <- summary(bh_fit, pars = c("cov_eff2"), 
               probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>% 
  dplyr::mutate(time = rep(1:20, length.out = nrow(.))) %>% 
  cbind(obs = data_list_plot$cov_eff2)

ggplot(data = cov_eff2,aes(x=time, y = mean)) + 
  geom_line( ) +
  geom_point(aes(x=time,y=obs), color = "red")+
  geom_ribbon(aes(x=time, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5) + 
  scale_x_continuous(breaks = c(2002,2006,2010, 2015,2020, 2022))

# plot  estimated kappas survival ======
kappasurvival_j<- summary(bh_fit, pars = c("kappa_j_survival"), 
                          probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>% 
  dplyr::mutate(time = rep(1:21, length.out = nrow(.))) %>% 
  cbind(obs = data_list_plot$kappa_j)

ggplot(data = kappasurvival_j,aes(x=time, y = mean)) + 
  geom_line( ) +
  geom_point(aes(x=time,y=obs), color = "red") +
  geom_ribbon(aes(x=time, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5) + 
  scale_x_continuous(breaks = c(2002,2006,2010, 2015,2020, 2022))

kappasurvival_marine <- summary(bh_fit, pars = c("kappa_marine_survival"), 
                         probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  %>% 
  dplyr::mutate(time = rep(1:22, length.out = nrow(.))) %>% 
  cbind(obs = data_list_plot$kappa_marine)

ggplot(data = kappasurvival_marine,aes(x=time, y = mean)) + 
  geom_line( ) +
  geom_point(aes(x=time,y=obs), color = "red")+
  geom_ribbon(aes(x=time, ymin = mean-se_mean,
                  ymax = mean+se_mean), alpha = 0.5) + 
  scale_x_continuous(breaks = c(2002,2006,2010, 2015,2020, 2022))

 
 
# Look at whole model summary  ======================  
# data_list - holds simulated values, this is from: simulate_data_age_structure.R

all_summ <- summary(bh_fit)$summary %>%
  data.frame() %>%
  rownames_to_column()  

low_n_eff <- all_summ %>% 
  filter(n_eff <20)
# 
# obs <- data.frame(log_c_1 = data_list_plot$log_c_1, 
#                   log_c_2 = data_list_plot$log_c_2,
#                   log_catch_q = data_list_plot$catch_q,
#                   D_scale = data_list_plot$D_scale,
#                   p_1 = 0.02
#                   
#                   # theta1_1 = data_list_plot$`theta1[1]`, 
#                   # theta1_2 = data_list_plot$`theta1[2]`,
#                   # theta2_1 = data_list_plot$`theta2[1]`, 
#                   # theta2_2 = data_list_plot$`theta2[2]`
#                   ) %>% 
#   gather(1:ncol(.), key = "rowname", value = "obs")
# 
# params <- summary(bh_fit, pars = c("log_c_1","log_c_2","log_catch_q", 
#                                    "D_scale", "p_1"# "theta1", "theta2"
#                                   # "N_catch_start_log", "N_egg_start_log" 
#                                   ), 
#                   probs = c(0.1, 0.9))$summary %>%
#   data.frame() %>%
#   rownames_to_column() %>%
#   # dplyr::mutate(rowname = case_when(rowname == "theta1[1]"~ "theta1_1",
#   #                                   rowname == "theta1[2]"~ "theta1_2",
#   #                                   rowname == "theta2[1]"~ "theta2_1",
#   #                                   rowname == "theta2[2]"~ "theta2_2",
#   #                                   TRUE ~ rowname)) %>%
#   left_join(obs)
# 
# params %>% 
#   ggplot() + 
#   geom_linerange(aes(rowname, ymin = X10.,ymax = X90.)) + 
#   geom_crossbar(aes(rowname, mean, ymin = X10.,ymax = X90.),  fill= 'grey') + 
#   geom_point(aes(rowname, obs), color = "red")+
#   facet_wrap(~rowname, scales = 'free') +
#   labs(caption = "red is observed, black is model")
#  
