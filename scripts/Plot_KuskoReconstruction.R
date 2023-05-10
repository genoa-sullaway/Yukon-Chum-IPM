# Start ==================================================================================================
#Project Name: AYK Chum Salmon Model
#Creator: Genoa Sullaway
#Date: Start May 2023
#
# Purpose: Compare predicted and observed from Kuskokwim Run Reconstruction model - output, optim is from Optimization_Kusko_reconstruction.R
# Load Packages =========================================================================================
library(tidyverse)
library(here)

ln_q_vec_par <- exp(optim_output$par[1])
baranov_sigma_par <- exp(optim_output$par[2])
escapement_slope_par <- exp(optim_output$par[3:11])

escapement_sigma_par <- exp(optim_output$par[12])
N_sigma_par <- exp(optim_output$par[13])
