# Load required libraries
library(tidyverse)
library(here)
library(purrr)
library(readr)
# source: https://akclimate.org/data/time-series-data/
circle_snow <- readxl::read_xlsx("data/Circle_Snow_Depth.xlsx") %>%
   separate(Date, into = c("year", "month"), sep = "-") %>% 
   filter(month %in% c("01","02","03"))

write_csv(circle_snow, "data/circle_snow_processed.csv")
