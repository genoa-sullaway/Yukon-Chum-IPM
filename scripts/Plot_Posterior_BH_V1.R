library(tidyverse)
library(tidybayes)
# this is the order stuff goes into model - should be order in posterior 
# sim_yukon_spring alpha = 0.02, beta = 7*10^-6 [used initially!]
# sim_yukon_fall alpha = 0.02, beta = 9.3*10^-6
# sim_kusko   alpha = 0.05, beta = 7*10^-6

# Observed ======================  
obs_df <- data.frame(id = c(#"observed","observed",
                            "observed","observed",
                            "observed","observed"), 
                     variable = c(#"sigma_y_j", "sigma_y_sp",
                                  "p_1", "p_2",
                                  "log_c_1", "log_c_2"), 
                     mean =c(#1000, 1000,
                             0.05, 0.15, 
                             log(10000000),log(1000000)),
                     se_mean=c(#0,0,
                               0,0,0,0))

sim_dat <- read_csv("data/Simulated_DatBH.csv")

bh_summary <- summary(bh_fit)$summary %>% 
  as.data.frame() %>% 
  mutate(variable = rownames(.)) %>% 
  select(variable, everything()) %>% 
  as_data_frame()
 
# posterior without simulated data points =============
bh_summary %>% 
  slice(3:6) %>% 
    ggplot() + 
      geom_linerange(aes(variable, ymin = `2.5%`,ymax = `97.5%`)) + 
      geom_crossbar(aes(variable, mean, ymin = `25%`, ymax = `75%`), fill= 'grey') + 
      facet_wrap(~variable, scales = 'free') 

 bh_summary %>% 
  slice(3:6) %>%
  ggplot() + 
  geom_linerange(aes(variable, ymin = `2.5%`,ymax = `97.5%`)) + 
  geom_crossbar(aes(variable, mean, ymin = `25%`, ymax = `75%`), fill= 'grey') + 
  facet_wrap(~variable, scales = 'free') +
  geom_point(data = obs_df, aes(variable, mean), color = "red" ) #observed

 pdf("output/posterior_simulation_plot.pdf")
 posterior
 dev.off()
 
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

