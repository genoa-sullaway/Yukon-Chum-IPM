# test fishing mortality 


log_f = rnorm(1000, 0.5, 0.1)
F = exp(log_f)

fish = 500

harvst = 500* (1-exp(-F))

hist(harvst)


mean(F)
hist(F)