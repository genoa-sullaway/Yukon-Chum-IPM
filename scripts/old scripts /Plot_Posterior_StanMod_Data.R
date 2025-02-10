library(tidyverse)
library(bayesplot)

#https://mc-stan.org/bayesplot/
bh_fit<-readRDS("output/stan_fit.RDS")

# posterior distributions =================
posterior <- as.matrix(bh_fit)
pars <- (posterior[,1:27])

pars <- (posterior[,20:27])

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(pars,
           #pars = c("cyl", "drat", "am", "wt"),
           prob = 0.8) + plot_title

