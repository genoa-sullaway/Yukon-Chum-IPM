# load model ====== 

bh_fit<- read_rds("output/stan_fit_DATA.RDS")

#functions ===== 
max.prod <- function(base.prod, covar, coef) {
  out <- 1/(1+exp(-base.prod - coef*covar))
  return(out)
}

surv <- function(base.prod=base.prod, covar, coef, capacity=capacity, N=N) {
  max.p <- max.prod(base.prod=base.prod, covar=covar, coef=coef)
  
  survival <- max.p/(1+(max.p*N/capacity))
  return(survival)
}

# load juvenile params ===== 
pars <- summary(bh_fit, pars = c("c_1", "basal_p_1","theta1[1]","theta1[2]"),
             probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column() %>%
  dplyr::select(1:2) %>%
  spread(rowname, mean)

pred_N_j <- summary(bh_fit, pars = c("N_j"), 
                    probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  
 
## #spawner size   low abund =====
surv(base.prod=pars$basal_p_1, covar=0, coef=pars$`theta1[1]`, capacity=pars$c_1, N=min(pred_N_j$mean))
surv(base.prod=pars$basal_p_1, covar=1, coef=pars$`theta1[1]`, capacity=pars$c_1, N=min(pred_N_j$mean))
 
## juveniles high =====
surv(base.prod=pars$basal_p_1, covar=0, coef=pars$`theta1[1]`, capacity=pars$c_1, N=max(pred_N_j$mean))
surv(base.prod=pars$basal_p_1, covar=1, coef=pars$`theta1[1]`, capacity=pars$c_1, N=max(pred_N_j$mean))
 
# Percent difference in survival at low abundance
surv(base.prod=pars$basal_p_1, covar=1, coef=pars$`theta1[1]`, capacity=pars$c_1, N=min(pred_N_j$mean)) -
  surv(base.prod=pars$basal_p_1, covar=0, coef=pars$`theta1[1]`, capacity=pars$c_1, N=min(pred_N_j$mean))/
  surv(base.prod=pars$basal_p_1, covar=0, coef=pars$`theta1[1]`, capacity=pars$c_1, N=min(pred_N_j$mean))

# 33% decrease in per capita survival

# Percent difference in survival at high abundance
surv(base.prod=pars$basal_p_1, covar=1, coef=pars$`theta1[1]`, capacity=pars$c_1, N=max(pred_N_j$mean)) -
  surv(base.prod=pars$basal_p_1, covar=0, coef=pars$`theta1[1]`, capacity=pars$c_1, N=max(pred_N_j$mean))/
  surv(base.prod=pars$basal_p_1, covar=0, coef=pars$`theta1[1]`, capacity=pars$c_1, N=max(pred_N_j$mean))

# 34% decrease in per capita survival

## snowmelt low abund ===== 
# Percent difference in survival at low abundance
surv(base.prod=pars$basal_p_1, covar=1, coef=pars$`theta1[2]`, capacity=pars$c_1, N=min(pred_N_j$mean)) -
  surv(base.prod=pars$basal_p_1, covar=0, coef=pars$`theta1[2]`, capacity=pars$c_1, N=min(pred_N_j$mean))/
  surv(base.prod=pars$basal_p_1, covar=0, coef=pars$`theta1[2]`, capacity=pars$c_1, N=min(pred_N_j$mean))



# test juv to return stage ===========
# load return params ===== 
return_pars <- summary(bh_fit, pars = c("c_2", "basal_p_2","theta2[1]","theta2[2]","theta2[3]"),
                probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column() %>%
  dplyr::select(1:2) %>%
  spread(rowname, mean)

pred_N_return <- summary(bh_fit, pars = c("N_brood_year_return"), 
                    probs = c(0.1, 0.9))$summary %>%
  data.frame() %>%
  rownames_to_column()  

# Fullness =====
## return low =====
cov_0 = surv(base.prod=return_pars$basal_p_2, covar=0, coef=return_pars$`theta2[1]`, capacity=return_pars$c_2, N=min(pred_N_return$mean))
cov_1 = surv(base.prod=return_pars$basal_p_2, covar=1, coef=return_pars$`theta2[1]`, capacity=return_pars$c_2, N=min(pred_N_return$mean))

(cov_1-cov_0)/cov_0

## juveniles high =====
surv(base.prod=return_pars$basal_p_2, covar=0, coef=return_pars$`theta2[1]`, capacity=return_pars$c_2, N=max(pred_N_return$mean))
surv(base.prod=return_pars$basal_p_2, covar=1, coef=return_pars$`theta2[1]`, capacity=return_pars$c_2, N=max(pred_N_return$mean))

# Percent difference in survival at low abundance
(surv(base.prod=return_pars$basal_p_2, covar=1, coef=return_pars$`theta2[1]`, capacity=return_pars$c_2, N=min(pred_N_return$mean))-
  surv(base.prod=return_pars$basal_p_2, covar=0, coef=return_pars$`theta2[1]`, capacity=return_pars$c_2, N=min(pred_N_return$mean)))/
  surv(base.prod=return_pars$basal_p_2, covar=0, coef=return_pars$`theta2[1]`, capacity=return_pars$c_2, N=min(pred_N_return$mean))

# 53% decrease in per capita survival
# Percent difference in survival at high abundance
(surv(base.prod=return_pars$basal_p_2, covar=1, coef=return_pars$`theta2[1]`, capacity=return_pars$c_2, N=max(pred_N_return$mean))-
  surv(base.prod=return_pars$basal_p_2, covar=0, coef=return_pars$`theta2[1]`, capacity=return_pars$c_2, N=max(pred_N_return$mean)))/
  surv(base.prod=return_pars$basal_p_2, covar=0, coef=return_pars$`theta2[1]`, capacity=return_pars$c_2, N=max(pred_N_return$mean))
 
# AI Temp  =====
# Percent difference in survival at low abundance
(surv(base.prod=return_pars$basal_p_2, covar=1, coef=return_pars$`theta2[2]`, capacity=return_pars$c_2, N=min(pred_N_return$mean))-
  surv(base.prod=return_pars$basal_p_2, covar=0, coef=return_pars$`theta2[2]`, capacity=return_pars$c_2, N=min(pred_N_return$mean)))/
  surv(base.prod=return_pars$basal_p_2, covar=0, coef=return_pars$`theta2[2]`, capacity=return_pars$c_2, N=min(pred_N_return$mean))

# Percent difference in survival at high abundance
(surv(base.prod=return_pars$basal_p_2, covar=1, coef=return_pars$`theta2[2]`, capacity=return_pars$c_2, N=max(pred_N_return$mean))-
  surv(base.prod=return_pars$basal_p_2, covar=0, coef=return_pars$`theta2[2]`, capacity=return_pars$c_2, N=max(pred_N_return$mean)))/
  surv(base.prod=return_pars$basal_p_2, covar=0, coef=return_pars$`theta2[2]`, capacity=return_pars$c_2, N=max(pred_N_return$mean))

#Chum  =====
# Percent difference in survival at low abundance
surv(base.prod=return_pars$basal_p_2, covar=0, coef=return_pars$`theta2[3]`, capacity=return_pars$c_2, N=min(pred_N_return$mean))
surv(base.prod=return_pars$basal_p_2, covar=1, coef=return_pars$`theta2[3]`, capacity=return_pars$c_2, N=min(pred_N_return$mean))

(surv(base.prod=return_pars$basal_p_2, covar=1, coef=return_pars$`theta2[3]`, capacity=return_pars$c_2, N=min(pred_N_return$mean))-
  surv(base.prod=return_pars$basal_p_2, covar=0, coef=return_pars$`theta2[3]`, capacity=return_pars$c_2, N=min(pred_N_return$mean)))/
  surv(base.prod=return_pars$basal_p_2, covar=0, coef=return_pars$`theta2[3]`, capacity=return_pars$c_2, N=min(pred_N_return$mean))
 
# Percent difference in survival at high abundance
(surv(base.prod=0.6, covar=1, coef=return_pars$`theta2[3]`, capacity=return_pars$c_2, N=max(pred_N_return$mean))-
  surv(base.prod=0.6, covar=0, coef=return_pars$`theta2[3]`, capacity=return_pars$c_2, N=max(pred_N_return$mean)))/
  surv(base.prod=0.6, covar=0, coef=return_pars$`theta2[3]`, capacity=return_pars$c_2, N=max(pred_N_return$mean))
 
