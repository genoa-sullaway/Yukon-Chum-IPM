# Source: Ak Fin: https://shinyfin.psmfc.org/

library(here)
library(tidyverse)
library(lubridate)

bs_sst <- read_csv("data/BS-SST-2024-02-05.csv") # daily mean temp for NBS and SEBS since 1985! 
#goa_sst <- read_csv("data/GOA-SST-2024-02-05.csv")

#adults unsure about goa vs bs for staging before returning to river, they have to go to BS eventually but papers show them in GOA for most of adult life 
sst_b <- bs_sst %>% 
   dplyr::mutate(year = year(date),
          month = month(date),
          day = day(date)) %>%
  filter(Ecosystem_sub == "Southeastern Bering Sea", 
         !month > 6) %>%
  group_by(year) %>%
  summarise(CDD = sum(meansst))

ggplot(data = sst_b) +
  geom_point(aes(x=year, y = CDD)) +
  geom_line(aes(x=year, y = CDD)) +
  theme_classic() +
  geom_hline(yintercept = mean(sst_b$CDD), linetype =2) +
  ylab("SEBS Cumulative Degree Days Jan - June") +
  xlab("Year")


write_csv(sst_a,"data/processed_covariates/Stage_A_CDD.csv")
