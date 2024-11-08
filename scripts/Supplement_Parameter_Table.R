# make table for supplement that has complete model output:
# parameters, parameter descriotion, Rhat, Neff, symbol,mean estiamte with CIs

# Load required libraries
library(tidyverse)
library(knitr)
library(kableExtra)
library(rstan)

fit<- read_rds("output/stan_fit_DATA.RDS")

# Function to create summary statistics from posterior samples
create_parameter_summary <- function(fit, parameter_name, notation, prior) {
  # Extract posterior summary
  posterior_summary <- summary(fit)$summary
  
  # Get statistics for the parameter
  param_stats <- posterior_summary[parameter_name, ]
  
  data.frame(
    Parameter = parameter_name,
    Notation = notation,
    Estimate = sprintf("%.2f", param_stats["mean"]),
    CI_95 = sprintf("[%.2f, %.2f]", 
                    param_stats["2.5%"],
                    param_stats["97.5%"]),
    ESS = sprintf("%.0f", param_stats["n_eff"]),  # Effective Sample Size
    Rhat = sprintf("%.3f", param_stats["Rhat"]),  # R-hat convergence diagnostic
    Prior = prior,
    stringsAsFactors = FALSE
  )
}

# Create summary for multiple parameters ==============
# each param will need to be manual 

# real  log_c_1;
# real  log_c_2;  
#  
# real log_sigma_sp; 
# real log_sigma_catch; 
#   
# real theta1 [ncovars1];  
# real theta2 [ncovars2];
#  
# real <lower=0, upper=1> D_scale;    
# real <lower=0> g[nByrs,A];  
# vector<lower=0, upper=1> [A] pi;  
# 
# real log_catch_q;  
# vector [A] log_S;  
# vector [nRyrs_T]  log_F;   
# 
# real <lower=0, upper = 1> basal_p_1; 
# real <lower=0, upper = 1> basal_p_2;  

summary_df <- rbind(
  ### Carrying capacity 1   =======
  create_parameter_summary(
    fit = fit,
    parameter_name = "log_c_1",
    notation = "$\\omega$",
    prior = "Normal(0.5, 0.25)"
  ),
  
  ### Carrying capacity 2   =======
  create_parameter_summary(
    fit = fit,
    parameter_name = "log_c_2",
    notation = "$K$",
    prior = "LogNormal(5, 1)"
  ),
  
  ### sigma sp  =======
  create_parameter_summary(
    fit = fit,
    parameter_name = "log_sigma_sp",
    notation = "$N_0$",
    prior = "LogNormal(3, 0.5)"
  ),
  
  ### sigma catch =======
  create_parameter_summary(
    fit = fit,
    parameter_name = "log_sigma_catch",
    notation = "$\\sigma_p$",
    prior = "Half-Cauchy(0, 2.5)"
  ),
  
  ### theta 1 =======
  create_parameter_summary(
    fit = fit,
    parameter_name = "theta1",
    notation = "$\\sigma_o$",
    prior = "Half-Cauchy(0, 2.5)"
  ),
  
  ### theta 2 =======
  create_parameter_summary(
    fit = fit,
    parameter_name = "theta2",
    notation = "$\\sigma_o$",
    prior = "Half-Cauchy(0, 2.5)"
  ),
  
  ### D_scale =======
  create_parameter_summary(
    fit = fit,
    parameter_name = "D_scale",
    notation = "$\\sigma_o$",
    prior = "Half-Cauchy(0, 2.5)"
  ),
  
  ### G ======= 
  create_parameter_summary(
    fit = fit,
    parameter_name = "g",
    notation = "$\\sigma_o$",
    prior = "Half-Cauchy(0, 2.5)"
  ),
  
  ### pi ======= 
  create_parameter_summary(
    fit = fit,
    parameter_name = "pi",
    notation = "$\\sigma_o$",
    prior = "Half-Cauchy(0, 2.5)"
  ))

# Add parameter grouping
summary_df$Group <- c("Population dynamics", "Population dynamics", 
                      "Initial conditions", "Error terms", "Error terms")

# Create formatted table with parameter grouping
table <- summary_df %>%
  # Reorder columns to include grouping
  select(Group, Parameter, Notation, Estimate, CI_95, ESS, Rhat, Prior) %>%
  # Create the table
  kable(format = "latex", 
        escape = FALSE,
        col.names = c("Group", "Parameter", "Notation", "Estimate", 
                      "95% CI", "ESS", "R-hat", "Prior"),
        align = c("l", "l", "c", "c", "c", "r", "c", "l"),
        booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  # Add grouping
  pack_rows(index = table(summary_df$Group)) %>%
  # Add footnote explaining diagnostics
  footnote(
    c("ESS: Effective Sample Size (higher is better, should be >1000)",
      "R-hat: Gelman-Rubin convergence diagnostic (should be <1.01)"),
    threepartslong = TRUE
  )

# Print table
print(table)

# Optional: Add color coding for diagnostics
table_with_colors <- summary_df %>%
  select(Group, Parameter, Notation, Estimate, CI_95, ESS, Rhat, Prior) %>%
  mutate(
    ESS = cell_spec(ESS, "latex", 
                    color = ifelse(as.numeric(ESS) > 1000, "darkgreen", "red")),
    Rhat = cell_spec(Rhat, "latex",
                     color = ifelse(as.numeric(Rhat) < 1.01, "darkgreen", "red"))
  ) %>%
  kable(format = "latex", 
        escape = FALSE,
        col.names = c("Group", "Parameter", "Notation", "Estimate", 
                      "95% CI", "ESS", "R-hat", "Prior"),
        align = c("l", "l", "c", "c", "c", "r", "c", "l"),
        booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  pack_rows(index = table(summary_df$Group)) %>%
  footnote(
    c("ESS: Effective Sample Size (higher is better, should be >1000)",
      "R-hat: Gelman-Rubin convergence diagnostic (should be <1.01)"),
    threepartslong = TRUE
  )

# Print colored table
print(table_with_colors)

# save ========

# For Word paper
save_publication_table(summary_df, "stan_results", format = "word")