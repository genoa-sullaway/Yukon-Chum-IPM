library(priorsense)
library(here)
library(tidyverse)

fit_all <- read_rds("output/stan_fit_DATA.RDS")

t <- powerscale_sensitivity(fit_all) 

a <- powerscale_plot_dens(fit_all, variable = "theta1") 
a
  powerscale_plot_dens(fit_all, variable = "log_c_1")
  powerscale_plot_dens(fit_all, variable = "log_c_2")

b <- powerscale_plot_quantities(fit_all)[[2]]
