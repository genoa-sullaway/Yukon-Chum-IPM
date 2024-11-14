library(priorsense)
library(here)
library(tidyverse)

fit_all<- read_rds("output/stan_fit_DATA.RDS")

posterior <-  (as.matrix(fit_all))   
posterior <-  (fit_all)

posterior <- posterior[!is.na(posterior)]

posterior_samples <- as.matrix(fit)
any(is.na(posterior_samples))
clean_posterior <- posterior_samples[, colSums(is.na(posterior_samples)) == 0]



powerscale_sensitivity(fit_all)


fit<-example_powerscale_model("univariate_normal")
