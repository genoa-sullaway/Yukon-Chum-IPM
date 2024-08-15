library(mgcv)
library(tidyverse)

# process stomach fullness, make fullness index. 

#Fullness <- read_csv("data/NBS_JChumFullness.xlsx") %>%

fullness_df <- Fullness %>% 
  dplyr::rename(fullness =`Stomach_fullness_index(o/ooo)`,
                Lat = `EQ Latitude`,
                Lon = `EQ Longitude`,
                stomach_weight = `Stomach_Weight(g)`,
                                ) %>%
  dplyr::mutate(num_stomachs_scale = as.numeric(scale(Number_of_Stomachs)),
                SampleYear = as.factor(SampleYear))

# plot

# model
# stomach fullness index with a tweedie

full_mod <- mgcv::gam(fullness ~ SampleYear + s(Lat,Lon) + , weights = num_stomachs_scale  )
