library(tidyverse)
library(here)

sockeye <- read_csv("data/Sockeye_Index_Covariate_A.csv") %>%
  filter(Stratum == "Stratum_1") %>%
  dplyr::mutate(brood_year = Time - 1,
         sockeye_juv_index = as.numeric(scale(Estimate-1))) %>% 
  dplyr::select(brood_year, sockeye_juv_index)

write_csv(sockeye,"data/processed_covariates/Cov_A_Sockeye_JuvIndex.csv")