library(tidyverse)
library(tidybayes)
# this is the order stuff goes into model - should be order in posterior 
# sim_yukon_spring alpha = 0.02, beta = 7*10^-6 [used initially!]
# sim_yukon_fall alpha = 0.02, beta = 9.3*10^-6
# sim_kusko   alpha = 0.05, beta = 7*10^-6

# Observed ======================  
obs_df <- data.frame(id = c("observed","observed",
                            "observed","observed",
                            "observed","observed"), 
                     variable = c("sigma_y_j", "sigma_y_sp",
                                  "p_1", "p_2",
                                  "c_1", "c_2"), 
                     mean =c(1000, 1000,
                             0.05, 0.15, 
                             10000000,1000000),
                     se_mean=c(0,0,0,0,0,0))

# Predicted ======================  
# rstanarm::launch_shinystan(bh_fit)
# head(summary_df)
# plot(bh_fit)
# 
# list_of_draws <- extract(bh_fit)
# print(names(list_of_draws))
# 
# extract(bh_fit)


# summary_df <- summary(bh_fit)$summary %>%
#   data.frame() %>%
#   tibble::rownames_to_column("row_names")  %>%
#   dplyr::filter(row_names %in% c("alpha", "beta")) %>%
#   dplyr::mutate(id = "predicted") %>%
#   dplyr::select(id, row_names, mean, se_mean) %>%
#   rbind(obs_df )

bh_summary <- summary(bh_fit)$summary %>% 
  as.data.frame() %>% 
  mutate(variable = rownames(.)) %>% 
  select(variable, everything()) %>% 
  as_data_frame()
# 
# posterior with out simulated data points =============
bh_summary %>% 
  slice(1:6) %>%
  # filter(variable %in% c('alpha[1]','alpha[2]','alpha[3]',
  #                        'beta[1]','beta[2]','beta[3]')) %>% 
  ggplot() + 
  geom_linerange(aes(variable, ymin = `2.5%`,ymax = `97.5%`)) + 
  geom_crossbar(aes(variable, mean, ymin = `25%`, ymax = `75%`), fill= 'grey') + 
  facet_wrap(~variable, scales = 'free') 


bh_summary %>% 
  slice(1:6) %>%
  # filter(variable %in% c('alpha[1]','alpha[2]','alpha[3]',
  #                        'beta[1]','beta[2]','beta[3]')) %>% 
  ggplot() + 
  geom_linerange(aes(variable, ymin = `2.5%`,ymax = `97.5%`)) + 
  geom_crossbar(aes(variable, mean, ymin = `25%`, ymax = `75%`), fill= 'grey') + 
  facet_wrap(~variable, scales = 'free') +
  geom_point(data = obs_df, aes(variable, mean), color = "red" ) #observed
 

