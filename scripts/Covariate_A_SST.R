# Source: Ak Fin: https://shinyfin.psmfc.org/

library(here)
library(tidyverse)
library(lubridate)

sst <- read_csv("data/BS-SST-2024-02-05.csv") # daily mean temp for NBS and SEBS since 1985! 

# for juvenile stage will do cumulative degree days for mean marine entry time until survey in the NBS
sst_a <- sst %>% 
   dplyr::mutate(year = year(date),
          month = month(date),
          day = day(date)) %>%
  filter(Ecosystem_sub == "Northern Bering Sea", 
         month %in% c(6,7,8,9),
         case_when(month == 6 ~ !day < 15,
                   month == 9 ~ !day > 15,
                   TRUE ~ TRUE)) %>%
  group_by(year) %>%
  summarise(CDD = sum(meansst))

ggplot(data = sst_a) +
  geom_point(aes(x=year, y = CDD)) +
  geom_line(aes(x=year, y = CDD)) +
  theme_classic() +
  geom_hline(yintercept = mean(sst_a$CDD), linetype =2) +
  ylab("NBS Cumulative Degree Days") +
  xlab("Year")


write_csv(sst_a,"data/processed_covariates/Stage_A_CDD.csv")
