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

# Call Function ==============
# each param will need to be manual 
summary_df <- rbind( 
  
  ### basal survival 1 =======
  create_parameter_summary(
    fit = fit,
    parameter_name = "basal_p_1",
    notation = "$\\sigma_o$",
    prior = "Beta(1,1)"
  ),

  ### basal survival 2 =======
  create_parameter_summary(
    fit = fit,
    parameter_name = "basal_p_2",
    notation = "$\\sigma_o$",
    prior = "Beta(1,1)"
  ),
 
  ### theta 1 =======
  create_parameter_summary(
    fit = fit,
    parameter_name = "theta1[1]",
    notation = "$\\sigma_o$",
    prior = "Beta(0,0.1)"
  ),
  create_parameter_summary(
    fit = fit,
    parameter_name = "theta1[2]",
    notation = "$\\sigma_o$",
    prior = "Beta(0,0.1)"
  ),
  create_parameter_summary(
    fit = fit,
    parameter_name = "theta1[3]",
    notation = "$\\sigma_o$",
    prior = "Beta(0,0.1)"
  ),
  create_parameter_summary(
    fit = fit,
    parameter_name = "theta1[4]",
    notation = "$\\sigma_o$",
    prior = "Beta(0,0.1)"
  ),
  
  ### theta 2 =======
  create_parameter_summary(
    fit = fit,
    parameter_name = "theta2[1]",
    notation = "$\\sigma_o$",
    prior = "Beta(0,0.1)"
  ),
  create_parameter_summary(
    fit = fit,
    parameter_name = "theta2[2]",
    notation = "$\\sigma_o$",
    prior = "Beta(0,0.1)"
  ),
  create_parameter_summary(
    fit = fit,
    parameter_name = "theta2[3]",
    notation = "$\\sigma_o$",
    prior = "Beta(0,0.1)"
  ),
  # alpha ====== 
  create_parameter_summary(
    fit = fit,
    parameter_name = "alpha[1]",
    notation = "$\\sigma_o$",
    prior = "Normal(7.5,0.5)"),
  create_parameter_summary(
    fit = fit,
    parameter_name = "alpha[2]",
    notation = "$\\sigma_o$",
    prior = "Normal(7.5,0.5)"),
  create_parameter_summary(
    fit = fit,
    parameter_name = "alpha[3]",
    notation = "$\\sigma_o$",
    prior = "Normal(7.5,0.5)"),
  create_parameter_summary(
    fit = fit,
    parameter_name = "alpha[4]",
    notation = "$\\sigma_o$",
    prior = "Normal(7.5,0.5)"),
  
  # Mean F ===== 
  create_parameter_summary(
    fit = fit,
    parameter_name = "log_F_mean",
    notation = "$K$",
    prior = "Normal(0,1)"
  ),
  
  # F SD ====== 
  create_parameter_summary(
    fit = fit,
    parameter_name = "log_F_dev_y[1]",
    notation = "$K$",
    prior = "Normal(0,1)"),
  
  # catch Q ======= 
  create_parameter_summary(
    fit = fit,
    parameter_name = "log_catch_q",
    notation = "$\\sigma_o$",
    prior = "Normal(-5,1)"),

  # Pi ======= 
  create_parameter_summary(
    fit = fit,
    parameter_name = "pi[1]",
    notation = "$\\sigma_o$",
    prior = "Beta(1,1)"),
  
  create_parameter_summary(
    fit = fit,
    parameter_name = "pi[2]",
    notation = "$\\sigma_o$",
    prior = "Beta(1,1)"),
  
  create_parameter_summary(
    fit = fit,
    parameter_name = "pi[3]",
    notation = "$\\sigma_o$",
    prior = "Beta(1,1)"),
  
  create_parameter_summary(
    fit = fit,
    parameter_name = "pi[4]",
    notation = "$\\sigma_o$",
    prior = "Beta(1,1)"),
 
  ## D_scale =======
  create_parameter_summary(
    fit = fit,
    parameter_name = "D_scale",
    notation = "$\\sigma_o$",
    prior = "Beta(1,1)"),
  ## D_scale =======
  create_parameter_summary(
    fit = fit,
    parameter_name = "log_c_1",
    notation = "$\\sigma_o$",
    prior = "XX"),
  ## D_scale =======
  create_parameter_summary(
    fit = fit,
    parameter_name = "log_c_2",
    notation = "$\\sigma_o$",
    prior = "XX")
)

# Add parameter grouping
# summary_df$Group <- c(
#                       "Observation Error", "Observation Error",
#                       "Observation Error", "Observation Error",
#                       "Basal Productivity - Juvenile",  "Basal Productivity - Marine",
#                       "Covariate Coefficient- Juvenile", "Covariate Coefficient- Juvenile", 
#                       "Covariate Coefficient- Juvenile", "Covariate Coefficient- Juvenile", "Covariate Coefficient- Juvenile", 
#                       "Covariate Coefficient- Marine", "Covariate Coefficient- Marine",
#                       "Covariate Coefficient- Marine", "Covariate Coefficient- Marine",
#                       "Selectivity", "Selectivity", 
#                       "Selectivity", "Selectivity", 
#                       "Mean Instantaneous Fishing Mortality", "Fishing Mortality Deviations", 
#                       "Juvenile Abundance Constant", 
#                       "Mean Age at Maturity",  "Mean Age at Maturity", 
#                       "Mean Age at Maturity",  "Mean Age at Maturity", 
#                       "Age Structure Variability")

# Create formatted table with parameter grouping
table <- summary_df %>%
  # Reorder columns to include grouping
  dplyr::select( Parameter,# Notation, 
         Estimate, CI_95, ESS, Rhat) #%>%
  # Create the table
  # kable(format = "latex", 
  #       escape = FALSE,
  #       col.names = c("Group", "Parameter", "Notation", "Estimate", 
  #                     "95% CI", "ESS", "R-hat", "Prior"),
  #       align = c("l", "l", "c", "c", "c", "r", "c", "l"),
  #       booktabs = TRUE) %>%
  # kable_styling(latex_options = c("striped", "hold_position")) %>%
  # # Add grouping
  # pack_rows(index = table(summary_df$Group)) %>%
  # # Add footnote explaining diagnostics
  # footnote(
  #   c("ESS: Effective Sample Size",
  #     "R-hat: Gelman-Rubin convergence diagnostic")#,
  #  # threepartslong = TRUE
  # )

# Print table

#print(
  View(table)
  
# save ========

# For Word paper
# write.csv(table, "output/Supplemental_Table_Parameter_Estimates_forCOPY.csv")#, row.names = FALSE)
# save_publication_table(table, "Supplemental_Table_Parameter_Estimates", format = "word")

# 
# 
# kable(summary_df, format = "html", escape = FALSE) %>%
#   kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
#   write_html("Supplemental_Table_Parameter_Estimates.html")
# 
#  
# # Save the HTML table to a file
# 
# kable(summary_df, "latex") %>%
#   kable_styling(latex_options = "striped") %>%
#   save_kable("output/Supplemental_Table_Parameter_Estimates.png")
# 
# library(kableExtra)
# 
# kable(summary_df, "latex", booktabs = T) %>%
#   kable_styling(latex_options = c("striped", "scale_down")) %>%
#   row_spec(1, color = "red") %>%
#   as_image()
#  
