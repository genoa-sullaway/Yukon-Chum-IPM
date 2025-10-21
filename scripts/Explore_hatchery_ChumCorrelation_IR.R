# Correlation 
library(tidyverse)
library(here)
library(readxl)
 
# Load Chum sum NE Pacific data ==== 
allchum <- read_xlsx("data/drivers_dataset_archive.xlsx") %>%
  dplyr::select(Year, chum_summed_natural_and_hatchery_catch_and_escapement_millions_of_fish) %>%
 filter(Year >1999) %>% 
   dplyr::rename(chum_nat_hatch = "chum_summed_natural_and_hatchery_catch_and_escapement_millions_of_fish") %>%
  dplyr::mutate(Year = as.numeric(Year),
                chum_nat_hatch = as.numeric(scale(as.numeric(chum_nat_hatch))))

# Load hatchery only data === not my covariate becauze that is rolling avg 
hatchery <- read_excel("data/NPAFC_Hatchery_Stat-1952-2022.xlsx") %>%
  janitor::row_to_names(row_number = 1) %>%
  data.frame() %>%
  filter(Species == "Chum",
         Reporting.Area == "Whole country") %>%
  dplyr::select(c(1:5, 54:76)) %>%
  gather(c(6:28), key = "Year", value = "Releases") %>%
  separate(Year, into = c("delete", "Year"), sep = 1) %>%
  dplyr::select(-delete) %>%  
  group_by(Year) %>%
  dplyr::summarise(Chum_hatchery = sum(Releases)) %>%
  dplyr::mutate(
    Year = as.numeric(Year),
    Chum_hatchery = as.numeric(scale(Chum_hatchery)))

# hatchery <- read_csv("data/processed_covariates/stage_b_all.csv") %>% 
#   dplyr::select(Year, Chum_hatchery) %>%
#   dplyr::mutate(Year = as.numeric(Year),
#                 Chum_hatchery = as.numeric(scale(Chum_hatchery))
#                 )

join <- left_join(allchum,hatchery) %>%
  gather(c(2:3),key = "id", value = "value" )

ggplot(data = join) +
  geom_line(aes(x=Year, y = value, group =id, color = id))

corjoin <- left_join(allchum,hatchery)  

cor.test(corjoin$chum_nat_hatch, corjoin$Chum_hatchery)



