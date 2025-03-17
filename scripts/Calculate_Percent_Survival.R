library(tidyverse)
library(here)
library(rstan)

# Load model and perform survival analysis
 
  bh_fit <- read_rds("output/stan_fit_DATA.RDS")
  
  # Define functions
  max.prod <- function(base.prod, covar, coef) {
    return(1/(1+exp(-base.prod - coef*covar)))
  }
  
  surv <- function(base.prod, covar, coef, capacity, N) {
    max.p <- max.prod(base.prod=base.prod, covar=covar, coef=coef)
    return(max.p/(1+(max.p*N/capacity)))
  }
  
  # Calculate percent change in survival
  calc_percent_change <- function(base.prod, coef, capacity, N) {
    surv_0 <- surv(base.prod=base.prod, covar=0, coef=coef, capacity=capacity, N=N)
    surv_1 <- surv(base.prod=base.prod, covar=1, coef=coef, capacity=capacity, N=N)
    percent_change <- (surv_1 - surv_0)/surv_0 * 100
    return(list(surv_0=surv_0, surv_1=surv_1, percent_change=percent_change))
  }
  
  # Load parameters
  extract_params <- function(fit, param_names) {
    summary(fit, pars = param_names, probs = c(0.1, 0.9))$summary %>%
      data.frame() %>%
      rownames_to_column() %>%
      dplyr::select(1:2) %>%
      spread(rowname, mean)
  }
  
  # Extract juvenile parameters
   juv_pars <- extract_params(bh_fit, c("c_1", "basal_p_1", "theta1[1]", "theta1[2]","theta1[3]", "theta1[4]"))
  # juv_pars<- summary(bh_fit, pars = c("c_1", "basal_p_1", "theta1[1]", "theta1[2]"), 
  #         probs = c(0.1, 0.9))$summary %>% 
  #   data.frame() %>%
  #   rownames_to_column() %>%
  #   dplyr::select(1:2) %>%
  #   spread(rowname, mean)
  
  # Extract return parameters
  return_pars <- extract_params(bh_fit, c("c_2", "basal_p_2", "theta2[1]", "theta2[2]", "theta2[3]"))
  
  # Get abundance values
  pred_N_j <- summary(bh_fit, pars = c("N_j"), probs = c(0.1, 0.9))$summary %>%
    data.frame() %>%
    rownames_to_column()
  
  pred_N_return <- summary(bh_fit, pars = c("N_brood_year_return"), probs = c(0.1, 0.9))$summary %>%
    data.frame() %>%
    rownames_to_column()
  
  # Create empty results dataframe
  results <- data.frame(
    stage = character(),
    covariate = character(),
    abundance = character(),
    survival_covar0 = numeric(),
    survival_covar1 = numeric(),
    percent_change = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Juvenile stage analyses
  covariates <- c("Spawner Size", "Snow Pack", "SST CDD", "Pollock")
 
  coef_names <- c("theta1[1]", "theta1[2]","theta1[3]", "theta1[4]")
  
  for (i in 1:length(covariates)) {
    # Low abundance
    low_result <- calc_percent_change(
      base.prod = juv_pars$basal_p_1,
      coef = juv_pars[[coef_names[i]]],
      capacity = juv_pars$c_1,
      N = min(pred_N_j$mean)
    )
    
    results <- rbind(results, data.frame(
      stage = "Juvenile",
      covariate = covariates[i],
      abundance = "Low",
      survival_covar0 = low_result$surv_0,
      survival_covar1 = low_result$surv_1,
      percent_change = low_result$percent_change
    ))
    
    # High abundance
    high_result <- calc_percent_change(
      base.prod = juv_pars$basal_p_1,
      coef = juv_pars[[coef_names[i]]],
      capacity = juv_pars$c_1,
      N = max(pred_N_j$mean)
    )
    
    results <- rbind(results, data.frame(
      stage = "Juvenile",
      covariate = covariates[i],
      abundance = "High",
      survival_covar0 = high_result$surv_0,
      survival_covar1 = high_result$surv_1,
      percent_change = high_result$percent_change
    ))
  }
  
  # Return stage analyses
  covariates <- c("Fullness", "AI Temp", "Chum")
  coef_names <- c("theta2[1]", "theta2[2]", "theta2[3]")
  
  for (i in 1:length(covariates)) {
    # Low abundance
    low_result <- calc_percent_change(
      base.prod = return_pars$basal_p_2,
      coef = return_pars[[coef_names[i]]],
      capacity = return_pars$c_2,
      N = min(pred_N_return$mean)
    )
    
    results <- rbind(results, data.frame(
      stage = "Return",
      covariate = covariates[i],
      abundance = "Low",
      survival_covar0 = low_result$surv_0,
      survival_covar1 = low_result$surv_1,
      percent_change = low_result$percent_change
    ))
    
    # High abundance
    high_result <- calc_percent_change(
      base.prod = return_pars$basal_p_2,
      coef = return_pars[[coef_names[i]]],
      capacity = return_pars$c_2,
      N = max(pred_N_return$mean)
    )
    
    results <- rbind(results, data.frame(
      stage = "Return",
      covariate = covariates[i],
      abundance = "High",
      survival_covar0 = high_result$surv_0,
      survival_covar1 = high_result$surv_1,
      percent_change = high_result$percent_change
    ))
  }
 
  # Return the consolidated results dataframe
  # Format the percent_change for better readability
  results$percent_change <- round(results$percent_change, 2)
  
  # Optional: Save results to CSV
  write.csv(results, "output/survival_percent_diff.csv", row.names = FALSE)
  
 