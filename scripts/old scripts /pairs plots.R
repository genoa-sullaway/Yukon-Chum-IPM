# divergent transitionts ======== 
partition <- util$partition_div(bh_fit)
div_samples <- partition[[1]]
nondiv_samples <- partition[[2]]

plot(nondiv_samples$a, nondiv_samples$b,
     col=c_dark_trans, pch=16, cex=0.8,
     xlab="a", ylab="b")
points(div_samples$a, div_samples$b,
       col=c_green_trans, pch=16, cex=0.8)

# bayesplotdivergent ========
np_cp <- nuts_params(bh_fit)
posterior_cp <- as.array(bh_fit)

scatter_theta_cp <- mcmc_scatter(
  posterior_cp, 
  pars = c("log_c_1"), 
  # transform = list(tau = "log"), # can abbrev. 'transformations'
  np = np_cp, 
  size = 1
)
scatter_theta_cp


# pairs plot =======

samples <- extract(bh_fit)

par(mfrow=c(1, 1))

plot(samples$p[,1:21], samples$q,
     #col=c_dark_trans, 
     pch=16, cex=0.8,
     xlab="p", ylab="p")



plot(samples$log_c_1, samples$log_c_2,
     #col=c_dark_trans, 
     pch=16, cex=0.8,
     xlab="log_catch_q", ylab="log_catch_q")

plot(samples$log_c_1, samples$log_c_1,
     #col=c_dark_trans, 
     pch=16, cex=0.8,
     xlab="p1[1]", ylab="p1[2]")



plot(samples$p_2, samples$p_2,
     #col=c_dark_trans, 
     pch=16, cex=0.8,
     xlab="p1[1]", ylab="p1[2]")
