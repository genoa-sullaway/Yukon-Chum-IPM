library(priorsense)
library(here)
library(tidyverse)

fit_all <- read_rds("output/stan_fit_DATA.RDS")

t <- powerscale_sensitivity(fit_all) 
t_a <-t[333,]

powerscale_plot_dens(fit_all, variable = "theta1") 
powerscale_plot_ecdf(fit_all, variable = "theta1")  

powerscale_plot_dens(fit_all, variable = "theta2") 
powerscale_plot_ecdf(fit_all, variable = "theta2")  

powerscale_plot_ecdf(fit_all, variable = "basal_p_2") 

powerscale_plot_ecdf(fit_all, variable = "log_S") 
