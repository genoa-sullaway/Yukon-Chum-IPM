library(tidyverse)
library(tidybayes)

# sim_yukon_fall alpha = 0.02, beta = 9.3*10^-6
# sim_kusko   alpha = 0.05, beta = 7*10^-6
# sim_yukon_spring alpha = 0.02, beta = 7*10^-6 [used initially!]

# test with sim yukon spring 
# Observed ======================  
obs_df <- data.frame(id = c("observed","observed"), 
                     variable = c("alpha", "beta"),
                     mean =c(0.02, 7*10^-6),
                     se_mean = c(0,0))

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
  filter(variable %in% c('alpha','beta')) %>% 
  ggplot() + 
  geom_linerange(aes(variable, ymin = `2.5%`,ymax = `97.5%`)) + 
  geom_crossbar(aes(variable, mean, ymin = `25%`, ymax = `75%`), fill= 'grey') + 
  facet_wrap(~variable, scales = 'free') +
  geom_point(data = obs_df, aes(variable, mean), color = "red" ) #observed
 
 
