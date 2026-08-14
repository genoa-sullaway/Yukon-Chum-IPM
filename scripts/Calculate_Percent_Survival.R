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
    surv_neg_1 = numeric(),
    percent_change = numeric(),
    percent_change_decrease = numeric(),
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
  # Calculate percent change in survival
  calc_percent_change <- function(base.prod, coef, capacity, N, year = NULL, coef_input = NULL) {
    
    # Survival at mean covariate value (0 SD)
    surv_0 <- surv(
      base.prod = base.prod,
      covar = 0,
      coef = coef,
      capacity = capacity,
      N = N
    )
    
    # Survival at -1 SD
    surv_neg_1 <- surv(
      base.prod = base.prod,
      covar = -1,
      coef = coef,
      capacity = capacity,
      N = N
    )
    
    # Survival at +1 SD
    if (!is.null(year) && !is.null(coef_input)) {
      if (nrow(coef_input) > 0) {
        custom_covar <- coef_input$coeff_value
        
        surv_1 <- surv(
          base.prod = base.prod,
          covar = custom_covar,
          coef = coef,
          capacity = capacity,
          N = N
        )
      } else {
        warning(paste("No data found for year", year))
        
        surv_1 <- surv(
          base.prod = base.prod,
          covar = 1,
          coef = coef,
          capacity = capacity,
          N = N
        )
      }
    } else {
      surv_1 <- surv(
        base.prod = base.prod,
        covar = 1,
        coef = coef,
        capacity = capacity,
        N = N
      )
    }
    
    # Percent change from mean to +1 SD
    percent_change <- (surv_1 - surv_0) / surv_0 * 100
    
    # Percent change from mean to -1 SD
    percent_change_decrease <- (surv_neg_1 - surv_0) / surv_0 * 100
    
    return(list(
      surv_0 = surv_0,
      surv_1 = surv_1,
      surv_neg_1 = surv_neg_1,
      percent_change = percent_change,
      percent_change_decrease = percent_change_decrease
    ))
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
    results_df$group_id <- paste(results_df$stage, 
                                 results_df$covariate, sep = "_")
    
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
      mean_percent_change_decrease = numeric(),
      median_percent_change_decrease = numeric(),
      lower_50_decrease = numeric(),
      upper_50_decrease = numeric(),
      lower_95_decrease = numeric(),
      upper_95_decrease = numeric(),
      survival_neg_1 = numeric(),
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
      percent_changes_decrease <- group_data$percent_change_decrease
    
      mean_val_decrease <- mean(percent_changes_decrease)
      median_val_decrease <- median(percent_changes_decrease)
      
      lower_50_decrease <- quantile(percent_changes_decrease, 0.25)
      upper_50_decrease <- quantile(percent_changes_decrease, 0.75)
      
      lower_95_decrease <- quantile(percent_changes_decrease, 0.025)
      upper_95_decrease <- quantile(percent_changes_decrease, 0.975)
      
      mean_val <- mean(percent_changes)
      median_val <- median(percent_changes)
      
      # Add in mean survival at -1 SD and +1 SD. 
      surv_neg_1 <- group_data$survival_covarneg1
      survival_neg_1<- mean(surv_neg_1)
      
      survival_covar1 <- group_data$survival_covar1
      survival_1 <- mean(survival_covar1)
      
      # Calculate credible intervals
      # 50% CI (25% to 75%)
      lower_50 <- quantile(percent_changes, 0.25)
      upper_50 <- quantile(percent_changes, 0.75)
      
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
        upper_95 = upper_95, 
        survival_neg_1 = survival_neg_1,
        survival_1 = survival_1,
        mean_percent_change_decrease = mean_val_decrease,
        median_percent_change_decrease = median_val_decrease,
        lower_50_decrease = lower_50_decrease,
        upper_50_decrease = upper_50_decrease,
        lower_95_decrease = lower_95_decrease,
        upper_95_decrease = upper_95_decrease
      ))
    }
    
    return(ci_results)
  }

# Extract juvenile parameters =======
   juv_pars <- extract_params(fit = bh_fit, 
                              param_names = c("c_1", "basal_p_1", "theta1[1]", "theta1[2]","theta1[3]", "theta1[4]"))
  
# Extract return parameters =======
  return_pars <- extract_params(fit = bh_fit, 
                                param_names = c("c_2", "basal_p_2", "theta2[1]", "theta2[2]", "theta2[3]","theta2[4]"))
  
  # Get mean abundance values that go into the calculation ==============
  # could adjust to get a specific year
  pred_N_j <-  mean(as.matrix(rstan::extract(bh_fit,  "N_j")[[1]]))
  
  pred_N_return <- mean(as.matrix(rstan::extract(bh_fit,  "N_brood_year_return")[[1]]))
  
  # Juvenile stage analyses general =========
  # mean abundance ========
  covariates <- c("Spawner Size", "River Discharge", "SST CDD", "Pollock")
   
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
      survival_covarneg1 = low_result$surv_neg_1,
      percent_change = low_result$percent_change,
      percent_change_decrease = low_result$percent_change_decrease
    ))
  }
  
  # Return stage analyses ===========
  covariates <- c("Fullness",
                  "GOA Temp", "All Chum", "All Pink")
  coef_names <- c("theta2[1]", "theta2[2]", "theta2[3]","theta2[4]")
  
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
      survival_covarneg1 = low_result$surv_neg_1,
      percent_change = low_result$percent_change,
      percent_change_decrease = low_result$percent_change_decrease
    ))
  }

  # summarise Mean and CI for each covariate among posterior draws.
  ci_df <- calculate_credible_intervals(results_df = results)
  
  # Return the consolidated results dataframe
  # Format the percent_change for better readability
  ci_df$mean_percent_change <- round(ci_df$mean_percent_change, 2)
  
  #  Save results to CSV
  write.csv(ci_df, "output_sullaway_etal/survival_percent_diff.csv" )
  # save the decrease DF too, but seperately  
  ci_decrease <- ci_df %>%
    select(
      stage,
      covariate,
      mean_percent_change = mean_percent_change_decrease,
      median_percent_change = median_percent_change_decrease,
      lower_50 = lower_50_decrease,
      upper_50 = upper_50_decrease,
      lower_95 = lower_95_decrease,
      upper_95 = upper_95_decrease
    )
  
  write.csv(
    ci_decrease,
    "output_sullaway_etal/survival_percent_diff_decrease.csv",
    row.names = FALSE
  )
  
#   ## load covariate data ========== 
stage_a_cov <- read_csv("data/processed_covariates/stage_a_all.csv") %>%
    filter(brood_year >= year_min,
           brood_year <= year_max_brood) %>%
     dplyr::mutate(SST_CDD_NBS = as.numeric(scale(SST_CDD_NBS)))
  
  # the temp in 2001 is gonna effect fish from brood year 1999
  stage_b_cov <- read_csv("data/processed_covariates/stage_b_all.csv") %>%
    dplyr::rename(full_index=full_index_scale) %>%
    filter(brood_year >= year_min,
           brood_year <= year_max_brood) 
  
# get 1 SD for each covariate ====
cov_a_sd <- stage_a_cov %>% #cbind(stage_a_cov, stage_b_cov) %>%
              gather(1:ncol(.), key = "id", value = "value") %>%
    group_by(id) %>%
    dplyr::summarise(sd = round(sd(value),6))

  cov_b_sd <- stage_b_cov %>% #cbind(stage_a_cov, stage_b_cov) %>%
    gather(1:ncol(.), key = "id", value = "value") %>%
    group_by(id) %>%
    dplyr::summarise(sd = round(sd(value),6))


