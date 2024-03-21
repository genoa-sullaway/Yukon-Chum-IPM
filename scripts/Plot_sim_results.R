 library(tidyverse)
library(tidybayes)
library(here)

# Observed ======================  
# data_list - holds simulated values, this is from: simulate_data_age_structure.R

bh_fit<- read_rds("output/stan_fit_SIMULATED_OUTPUT_statespace.RDS")
print(names(bh_summary))
bh_summary <- summary(bh_fit) %>% 
  as.data.frame(bh_fit) %>% 
  mutate(variable = rownames(.)) %>% 
  select(variable, everything()) %>% 
  as_data_frame()

# posterior without simulated data points =============

bh_summary %>% 
  slice(1:6) %>%
  ggplot() + 
  geom_linerange(aes(variable, ymin = `2.5%`,ymax = `97.5%`)) + 
  geom_crossbar(aes(variable, mean, ymin = `25%`, ymax = `75%`), fill= 'grey') + 
  facet_wrap(~variable, scales = 'free') +
  geom_point(data = obs_df, aes(variable, mean), color = "red" ) #observed

bh_summary %>% 
  slice(1:6) %>% 
  ggplot() + 
  geom_linerange(aes(variable, ymin = `2.5%`,ymax = `97.5%`)) + 
  geom_crossbar(aes(variable, mean, ymin = `25%`, ymax = `75%`), fill= 'grey') + 
  facet_wrap(~variable, scales = 'free') 

#  pdf("output/posterior_simulation_plot.pdf")
#  posterior
#  dev.off()
#  
# Plot posterior generated quantities
sim_dat_plot<- sim_dat %>%
  dplyr::rename(pp_log_N_j = "N_j",
                pp_log_N_sp = "N_sp") %>%
  dplyr::select(pp_log_N_j,pp_log_N_sp) %>%
  dplyr::mutate(time = 1:nrow(.)) %>%
  gather(1:2, key = "variable_name", value = "mean")

bh_summary %>% 
  slice(-c(1:9)) %>%
  dplyr::mutate(
    variable_name = str_extract(variable, "(N_sp|kappa_j|kappa_sp|pp_log_N_sp|pp_log_N_j|N_e|N_j)"),  # Extracts 'N_sp' or 'N_kappa' into 'variable' column
    time = as.numeric(str_extract(variable, "(?<=\\[)\\d+(?=\\])"))) %>% # Extracts '[3]' or '[31]' into 'time' column
  filter(variable_name %in% c("pp_log_N_j","pp_log_N_sp")) %>% 
  ggplot() + 
  geom_point(aes(x=time, y =exp(mean), group = variable_name)) +
  geom_errorbar(aes(x=time,  ymin = exp(`2.5%`), ymax = exp(`97.5%`))) + 
  geom_point(data = sim_dat_plot, aes(time, mean), color = "red" )  +
  facet_wrap(~variable_name, scales = 'free')#observed

