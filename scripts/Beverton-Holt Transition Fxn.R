capacity <- 1000

basal.prod <- 0.5


max.prod <- function(base.prod=0.5, covar=1, coef=0.5) {
  out <- 1/(1+exp(-base.prod - coef*covar))
  return(out)
}

surv <- function(base.prod=base.prod, covar=1, coef=0.5, capacity=capacity, N=N) {
  max.p <- max.prod(base.prod=base.prod, covar=covar, coef=coef)
  
  survival <- max.p/(1+(max.p*N/capacity))
  return(survival)
}


surv(base.prod=0.5, covar=0, coef=0.5, capacity=1000, N=2)
surv(base.prod=0.5, covar=1, coef=0.5, capacity=1000, N=2)


surv(base.prod=0.5, covar=0, coef=0.5, capacity=1000, N=2000)
surv(base.prod=0.5, covar=1, coef=0.5, capacity=1000, N=2000)

# Percent difference in survival at low abundance
base.prod=0.5
capacity=1000 
N=2

coef=0.5

(surv(base.prod=base.prod, covar=1, coef=coef, capacity=capacity, N=N) -
    surv(base.prod=base.prod, covar=0, coef=coef, capacity=capacity, N=N) )/
  surv(base.prod=base.prod, covar=0, coef=coef, capacity=capacity, N=N)

# 17.4% increase in per capita survival

# Percent difference in survival at high abundance
base.prod=0.5
capacity=1000 
N=1000

coef=0.5

(surv(base.prod=base.prod, covar=1, coef=coef, capacity=capacity, N=N) -
    surv(base.prod=base.prod, covar=0, coef=coef, capacity=capacity, N=N) )/
  surv(base.prod=base.prod, covar=0, coef=coef, capacity=capacity, N=N)

# 10.1% increase in per capita survival