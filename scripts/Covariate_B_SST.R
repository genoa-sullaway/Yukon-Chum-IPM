# Source: Ak Fin: https://shinyfin.psmfc.org/

library(here)
library(tidyverse)
library(lubridate)

bs_sst <- read_csv("data/BS-SST-2024-02-05.csv") # daily mean temp for NBS and SEBS since 1985! 
goa_sst <- read_csv("data/GOA-SST-2024-02-05.csv")
alut_sst <- read_csv("data/AI-SST-2024-07-25.csv")

unique(alut_sst$Ecosystem_sub)

# GOA temp ============
#adults unsure about goa vs bs for staging before returning to river, they have to go to BS eventually but papers show them in GOA for most of adult life 
CDD_aleut <- alut_sst %>% 
  dplyr::mutate(year = year(date),
                month = month(date),
                day = day(date)) %>%
  filter(Ecosystem_sub == "Eastern Aleutians"#, 
         
         #!month > 6
  ) %>%
  group_by(year) %>%
  summarise(CDD = sum(meansst)) %>%
  filter(!year == 2024,
         !year < 2000)

ggplot(data = CDD_aleut) +
  geom_point(aes(x=year, y = CDD)) +
  geom_line(aes(x=year, y = CDD)) +
  theme_classic() +
  geom_hline(yintercept = mean(CDD_b$CDD), linetype =2) +
  ylab("Aleutian Cumulative Degree Days Jan - June") +
  xlab("Year")

write_csv(CDD_b,"data/processed_covariates/Stage_B_CDD.csv")

# GOA temp ============
#adults unsure about goa vs bs for staging before returning to river, they have to go to BS eventually but papers show them in GOA for most of adult life 
CDD_b <- goa_sst %>% 
  dplyr::mutate(year = year(date),
                month = month(date),
                day = day(date)) %>%
  filter(Ecosystem_sub == "Western Gulf of Alaska"#, 
         
         #!month > 6
         ) %>%
  group_by(year) %>%
  summarise(CDD = sum(meansst)) %>%
  filter(!year == 2024,
         !year < 2000)

ggplot(data = CDD_b) +
  geom_point(aes(x=year, y = CDD)) +
  geom_line(aes(x=year, y = CDD)) +
  theme_classic() +
  geom_hline(yintercept = mean(CDD_b$CDD), linetype =2) +
  ylab("GOA Cumulative Degree Days Jan - June") +
  xlab("Year")

write_csv(CDD_b,"data/processed_covariates/Stage_B_CDD.csv")
 
# sst_b <- goa_sst %>%
#   dplyr::mutate(year = year(date),
#                 month = month(date),
#                 day = day(date)) %>%
#   filter(Ecosystem_sub == "Western Gulf of Alaska"
#         ) %>%
#   group_by(year) %>%
#   summarise(CDD = sum(meansst)) %>%
#   filter(!year == 2024,
#          !year < 2000)
# 
# ggplot(data = sst_b) +
#   geom_point(aes(x=year, y = CDD)) +
#   geom_line(aes(x=year, y = CDD)) +
#   theme_classic() +
#   geom_hline(yintercept = mean(sst_b$CDD), linetype =2) +
#   ylab("GOA Cumulative Degree Days Jan - June") +
#   xlab("Year")

# SEBS temp ============
#  
# #adults unsure about goa vs bs for staging before returning to river, they have to go to BS eventually but papers show them in GOA for most of adult life
# sst_b <- bs_sst %>%
#    dplyr::mutate(year = year(date),
#           month = month(date),
#           day = day(date)) %>%
#   filter(Ecosystem_sub == "Southeastern Bering Sea",
#          !month > 6) %>%
#   group_by(year) %>%
#   summarise(CDD = sum(meansst)) %>%
#   filter(!year == 2024,
#          !year < 2000)
# 
# ggplot(data = sst_b) +
#   geom_point(aes(x=year, y = CDD)) +
#   geom_line(aes(x=year, y = CDD)) +
#   theme_classic() +
#   geom_hline(yintercept = mean(sst_b$CDD), linetype =2) +
#   ylab("SEBS Cumulative Degree Days Jan - June") +
#   xlab("Year")
# 
# write_csv(sst_b,"data/processed_covariates/Stage_B_CDD.csv")
