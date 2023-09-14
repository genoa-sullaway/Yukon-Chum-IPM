library(tidyverse)
library(tidybayes)
# this is the order stuff goes into model - should be order in posterior 
# sim_yukon_spring alpha = 0.02, beta = 7*10^-6 [used initially!]
# sim_yukon_fall alpha = 0.02, beta = 9.3*10^-6
# sim_kusko   alpha = 0.05, beta = 7*10^-6

# Observed ======================  
obs_df <- data.frame(id = c("observed","observed",
                            "observed","observed",
                            "observed","observed",
                            "observed","observed",
                             "observed","observed",
                             "observed","observed",
                            "observed"), 
                     variable = c("alpha_1[1]", "log_beta_1[1]",
                                  "alpha_2[1]", "log_beta_2[1]",
                                  
                                  "alpha_1[2]", "log_beta_1[2]", 
                                  "alpha_2[2]", "log_beta_2[2]", 
                                  
                                  "alpha_1[3]", "log_beta_1[3]",
                                  "alpha_2[3]", "log_beta_2[3]",
                                  
                                  "sigma_y"),
                     
                     mean =c(0.2, log(9.3*10^-6),
                             0.06, log(9.3*10^-6),
                             
                             0.02,log(9.3*10^-6),
                             0.06,log(9.3*10^-6),
                             
                             0.05,log(7*10^-6),
                             0.07,  log(7*10^-6),
                             
                             10),
                     se_mean=c(0,0,
                                 0,0,0,0,
                                 0,0,0,0,
                                 0,0,0))

# Predicted ======================  
# rstanarm::launch_shinystan(bh_fit)
# head(summary_df)
# plot(bh_fit)
# 
# list_of_draws <- extract(bh_fit)
# print(names(list_of_draws))
# 
# extract(bh_fit)


bh_summary <- summary(bh_fit)$summary %>% 
  as.data.frame() %>% 
  mutate(variable = rownames(.)) %>% 
  select(variable, everything()) %>% 
  as_data_frame()
# 
# summary_df <- summary(bh_fit)$summary %>%
#   data.frame() %>%
#   tibble::rownames_to_column("row_names")  %>%
#   dplyr::filter(row_names %in% c("alpha", "beta")) %>%
#   dplyr::mutate(id = "predicted") %>%
#   dplyr::select(id, row_names, mean, se_mean) %>%
#   rbind(obs_df )

bh_summary %>% 
  slice(1:13) %>%
  # filter(variable %in% c('alpha[1]','alpha[2]','alpha[3]',
  #                        'beta[1]','beta[2]','beta[3]')) %>% 
  ggplot() + 
  geom_linerange(aes(variable, ymin = `2.5%`,ymax = `97.5%`)) + 
  geom_crossbar(aes(variable, mean, ymin = `25%`, ymax = `75%`), fill= 'grey') + 
  facet_wrap(~variable, scales = 'free') +
  geom_point(data = obs_df, aes(variable, mean), color = "red" ) #observed
 
 
