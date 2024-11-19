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
    parameter_name = "theta1[1]",
    notation = "$\\sigma_o$",
    prior = "Half-Cauchy(0, 2.5)"
  ),
  
  ### theta 2 =======
  create_parameter_summary(
    fit = fit,
    parameter_name = "theta2[1]",
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
  
  # F
  create_parameter_summary(
    fit = fit,
    parameter_name = "log_F_mean",
    notation = "$K$",
    prior = "LogNormal(5, 1)"
  ),
  
  # F SD
  create_parameter_summary(
    fit = fit,
    parameter_name = "log_F_dev_y[1]",
    notation = "$K$",
    prior = "LogNormal(5, 1)"
  ),
  
    # selectivity
  create_parameter_summary(
    fit = fit,
    parameter_name = "log_S[1]",
    notation = "$K$",
    prior = "LogNormal(5, 1)"
  ))

# Add parameter grouping
summary_df$Group <- c("Carrying capacity","Carrying capacity",
                      "Observation Error", "Observation Error", 
                      "Covariate Coefficient", "Covariate Coefficient", 
                      "Age Structure Variability", "Mean Instantaneous Fishing Mortality",
                      "Fishing Mortality Deviations", 
                      "Selectivity")

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
    c("ESS: Effective Sample Size",
      "R-hat: Gelman-Rubin convergence diagnostic")#,
   # threepartslong = TRUE
  )

# Print table
print(table)
 

# save ========

# For Word paper
write.csv(table, "output/Supplemental_Table_Parameter_Estimates.csv", row.names = FALSE)
# save_publication_table(table, "Supplemental_Table_Parameter_Estimates", format = "word")



kable(summary_df, format = "html", escape = FALSE) %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  write_html("Supplemental_Table_Parameter_Estimates.html")

 
# Save the HTML table to a file

kable(summary_df, "latex") %>%
  kable_styling(latex_options = "striped") %>%
  save_kable("output/Supplemental_Table_Parameter_Estimates.png")

library(kableExtra)

kable(summary_df, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  row_spec(1, color = "red") %>%
  as_image()
 
