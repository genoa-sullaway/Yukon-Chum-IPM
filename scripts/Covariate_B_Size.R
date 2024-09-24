library(here)
library(tidyverse)
library(viridis)

asl_dat <-read.csv("data/ASL_summary_byFWSWage.csv") %>%
  filter(ASLProjectType %in% c("escapement"),
         Species == "chum", 
         SASAP.Region %in% c("Yukon"))  

t <-data.frame(unique(asl_dat$LocationID)) 