library(tidyverse)
library(here)
library(rstan)

# Load model =======
  bh_fit <- read_rds("output/stan_fit_DATA.RDS")
  
# Create empty results dataframe ===========
  results <- data.frame(
    stage = character(),
    covariate = character(),
    abundance = character(),
    survival_covar0 = numeric(),
    survival_covar1 = numeric(),
    percent_change = numeric(),
    stringsAsFactors = FALSE
  )
  
  specific_results <- data.frame(
    stage = character(),
    covariate = character(),
    abundance = character(),
    survival_covar0 = numeric(),
    survival_covar1 = numeric(),
    percent_change = numeric(),
    stringsAsFactors = FALSE
  )
# Define functions ==========
  max.prod <- function(base.prod, covar, coef) {
    return(1/(1+exp(-base.prod - coef*covar)))
  }
  
  surv <- function(base.prod, covar, coef, capacity, N) {
    max.p <- max.prod(base.prod=base.prod, covar=covar, coef=coef)
    return(max.p/(1+(max.p*N/capacity)))
  }
  
  # Calculate percent change in survival
  calc_percent_change <- function(base.prod, coef, capacity, N, year = NULL, coef_input = NULL) {
    # Default behavior: use covar = 0 and covar = 1
    surv_0 <- surv(base.prod = base.prod, covar = 0, coef = coef, capacity = capacity, N = N)
 
    # If year and dataframe are provided, get the covar value from the dataframe
    if (!is.null(year) && !is.null(coef_input)) {
      if (nrow(coef_input) > 0) {
        custom_covar <- coef_input$coeff_value  # Assuming 'SD' is the column name for the covariance value
        surv_1 <- surv(base.prod = base.prod, covar = custom_covar, coef = coef, capacity = capacity, N = N)
      } else {
        warning(paste("No data found for year", year))
        surv_1 <- surv(base.prod = base.prod, covar = 1, coef = coef, capacity = capacity, N = N)
      }
    } else {
      # Use the default covar = 1 if year or df is not provided
      surv_1 <- surv(base.prod = base.prod, covar = 1, coef = coef, capacity = capacity, N = N)
    }
    
    percent_change <- (surv_1 - surv_0) / surv_0 * 100
    return(list(surv_0 = surv_0, surv_1 = surv_1, percent_change = percent_change))
  }
  
  # Load parameters
  extract_params <- function(fit, param_names) {
    # Extract all draws for each parameter
    all_draws <- rstan::extract(fit, pars = param_names)
    
    # create an empty data frame to store the results
    result_df <- data.frame(matrix(ncol = length(param_names), nrow = nrow(all_draws[[1]])))
    colnames(result_df) <- param_names
    
    # add data to data frame 
    for (i in 1:length(param_names)) { 
        result_df[,i]  <- all_draws[[i]]
    }
    
    return(result_df)
  }
  
  # summarise 
  calculate_credible_intervals <- function(results_df) {
    # Create a unique grouping identifier that properly respects stage and covariate
    results_df$group_id <- paste(results_df$stage, results_df$covariate, sep = "_")
    
    # Get unique groups
    unique_groups <- unique(results_df$group_id)
    
    # Initialize results dataframe
    ci_results <- data.frame(
      stage = character(),
      covariate = character(),
      mean_percent_change = numeric(),
      median_percent_change = numeric(),
      lower_50 = numeric(),
      upper_50 = numeric(),
      lower_95 = numeric(),
      upper_95 = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Calculate intervals for each group
    for (group in unique_groups) {
      # Filter data for current group
      group_data <- results_df[results_df$group_id == group, ]
      
      # Extract stage and covariate
      stage <- unique(group_data$stage)[1]
      covariate <- unique(group_data$covariate)[1]
      
      # Calculate statistics for percent_change
      percent_changes <- group_data$percent_change
      mean_val <- mean(percent_changes)
      median_val <- median(percent_changes)
      
      # Calculate credible intervals
      # 50% CI (25% to 75%)
      lower_50 <- quantile(percent_changes, 0.20)
      upper_50 <- quantile(percent_changes, 0.80)
      
      # 95% CI (2.5% to 97.5%)
      lower_95 <- quantile(percent_changes, 0.025)
      upper_95 <- quantile(percent_changes, 0.975)
      
      # Add to results
      ci_results <- rbind(ci_results, data.frame(
        stage = stage,
        covariate = covariate,
        mean_percent_change = mean_val,
        median_percent_change = median_val,
        lower_50 = lower_50,
        upper_50 = upper_50,
        lower_95 = lower_95,
        upper_95 = upper_95
      ))
    }
    
    return(ci_results)
  }

# Extract juvenile parameters =======
   juv_pars <- extract_params(fit = bh_fit, 
                              param_names = c("c_1", "basal_p_1", "theta1[1]", "theta1[2]","theta1[3]", "theta1[4]"))
  
# Extract return parameters =======
  return_pars <- extract_params(fit = bh_fit, 
                                param_names = c("c_2", "basal_p_2", "theta2[1]", "theta2[2]", "theta2[3]"))
  
  # Get mean abundance values that go into the calculation ==============
  # could adjust to get a specific year
  pred_N_j <-  mean(as.matrix(rstan::extract(bh_fit,  "N_j")[[1]]))
  
  pred_N_return <- mean(as.matrix(rstan::extract(bh_fit,  "N_brood_year_return")[[1]]))
  
  # Juvenile stage analyses general =========
  # mean abundance ========
  covariates <- c("Spawner Size", "Snow Pack", "SST CDD", "Pollock")
   
  coef_names <- c("theta1[1]", "theta1[2]","theta1[3]", "theta1[4]")
  
  for (i in 1:length(covariates)) {
    low_result <- calc_percent_change(
      base.prod = juv_pars$basal_p_1,
      coef = juv_pars[[coef_names[i]]],
      capacity = juv_pars$c_1,
      N = pred_N_j,
      year = NULL, 
      coef_input = NULL
    )
    
    results <- rbind(results, data.frame(
      stage = "Juvenile",
      covariate = covariates[i],
      abundance = "Mean",
      survival_covar0 = low_result$surv_0,
      survival_covar1 = low_result$surv_1,
      percent_change = low_result$percent_change
    ))

  }
  
  # Return stage analyses ===========
  covariates <- c("Fullness",
                  "AI Temp", "Chum")
  coef_names <- c("theta2[1]", "theta2[2]", "theta2[3]")
  
  for (i in 1:length(covariates)) {
    # Mean abundance
    low_result <- calc_percent_change(
      base.prod = return_pars$basal_p_2,
      coef = return_pars[[coef_names[i]]],
      capacity = return_pars$c_2,
      N = pred_N_return,
      year = NULL, 
      coef_input = NULL
    )
    
    results <- rbind(results, data.frame(
      stage = "Return",
      covariate = covariates[i],
      abundance = "Mean",
      survival_covar0 = low_result$surv_0,
      survival_covar1 = low_result$surv_1,
      percent_change = low_result$percent_change
    ))
  }
 
  # summarise Mean and CI for each covariate among posterior draws.
  ci_df <- calculate_credible_intervals(results_df = results)
  
  
  # Return the consolidated results dataframe
  # Format the percent_change for better readability
  ci_df$mean_percent_change <- round(ci_df$mean_percent_change, 2)
  
  # Optional: Save results to CSV
  write.csv(ci_df, "output/survival_percent_diff.csv" )
  
  
  # load covariate data ========== 
stage_a_cov <- read_csv("data/processed_covariates/stage_a_all.csv") %>%
    filter(brood_year >= year_min, 
           brood_year <= year_max_brood) %>%
    dplyr::mutate(SST_CDD_NBS = as.numeric(scale(SST_CDD_NBS)), 
                  yukon_mean_discharge = as.numeric(scale(yukon_mean_discharge)),
                  fall_snow_cummulative = as.numeric(scale(fall_snow_cummulative)), 
                  pollock_recruit_scale = as.numeric(scale(Recruit_age_1_millions)))  
  
  # the temp in 2001 is gonna effect fish from brood year 1999
  stage_b_cov <- read_csv("data/processed_covariates/stage_b_all.csv") %>%
    dplyr::rename(full_index=full_index_scale) %>% 
    filter(brood_year >= year_min, 
           brood_year <= year_max_brood) %>% 
    dplyr::mutate( SST_CDD_Aleut = as.numeric(scale(SST_CDD_Aleut)),
                   Chum_hatchery= as.numeric(scale(Chum_hatchery)),
                   Pink_hatchery= as.numeric(scale(Pink_hatchery))
                   # full_index = as.numeric(scale(full_index))
    )  
  
## Juvenile stage analyses specific years ========= 
  covariates <- c("Spawner Size")
  coef_names <- c("theta1[1]")
  
   #specific interest in spawner size in BY 2015
  for (i in 1:length(covariates)) {
    low_result <- calc_percent_change(
      base.prod = juv_pars$basal_p_1,
      coef = juv_pars[[coef_names[i]]],
      capacity = juv_pars$c_1,
      N = pred_N_j,
      year = 2019, 
      coef_input = stage_a_cov %>% 
        dplyr::select(brood_year,mean_size) %>% 
        filter(brood_year == 2019) %>%
        dplyr::rename(coeff_value = mean_size)
    )
    
    specific_results <- rbind(specific_results, data.frame(
      stage = "Juvenile",
      covariate = covariates[i],
      abundance = "Mean",
      survival_covar0 = low_result$surv_0,
      survival_covar1 = low_result$surv_1,
      percent_change = low_result$percent_change
    ))
    
  }
  
  ## Return stage analyses ===========
  covariates <- c("AI Temp")
  coef_names <- c("theta2[2]")
  
  for (i in 1:length(covariates)) {
    # Mean abundance
    low_result <- calc_percent_change(
      base.prod = return_pars$basal_p_2,
      coef = return_pars[[coef_names[i]]],
      capacity = return_pars$c_2,
      N = pred_N_return,
      year = 2019, 
      coef_input = stage_b_cov %>% 
        dplyr::select(brood_year,SST_CDD_Aleut) %>% 
        filter(brood_year == 2019) %>%
        dplyr::rename(coeff_value = SST_CDD_Aleut)
    )
    
    specific_results <- rbind(specific_results, data.frame(
      stage = "Return",
      covariate = covariates[i],
      abundance = "Mean",
      survival_covar0 = low_result$surv_0,
      survival_covar1 = low_result$surv_1,
      percent_change = low_result$percent_change
    ))
  }
  
  # summarise Mean and CI for each covariate among posterior draws.
  specific_ci_df <- calculate_credible_intervals(results_df = specific_results)
  
  # Return the consolidated results data frame
  # Format the percent_change for better readability
  specific_ci_df$mean_percent_change <- round(specific_ci_df$mean_percent_change, 2)
  
  # Optional: Save results to CSV
  write.csv(specific_ci_df, "output/survival_percent_diff_specific_years.csv" )
  
  
  
  