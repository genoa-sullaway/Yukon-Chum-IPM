library(here)
library(tidyverse)

# Kusko Data Tidy ========================================================================

estimated_parameters <- readRDS("output/optim_output_par_data2021.RDS")
pred_N_Kusko<-data.frame(year = c(1988:(upper_year-1)),   
                   ssb= as.vector(c(estimated_parameters[2:35])))  

# Juv Index Tidy ========================================================================
juv_index<- read.csv("data/Juv_Index_CC_aug2023/index2.csv")  %>%
            dplyr::select(Time, Estimate, Std..Error.for.Estimate) %>%
            dplyr::rename(year = "Time",
                           r = "Estimate",
                           se = "Std..Error.for.Estimate")


# Join everything ========================================================================
df<- left_join(pred_N_Kusko, juv_index) %>% 
  filter(!is.na(r))

write_csv(df,"data/input_dat_stan.csv")


