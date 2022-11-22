library(here)
library(tidyverse)

# load 2018 lipid data 
lipid <- read.csv("data/2018 Copepod data QA CP.csv") %>%
  dplyr::select(Latitude, Longitude, estimated...lipid..dry.weight) %>%
  dplyr::rename("proportion_lipid_dw" = estimated...lipid..dry.weight)
 
 
#Calculate DW from NBS abundance 