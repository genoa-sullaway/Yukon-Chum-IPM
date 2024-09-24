# Source:  Pollock stock assessment, table 26 -- https://apps-afsc.fisheries.noaa.gov/Plan_Team/2023/EBSPollock.pdf

library(here)
library(tidyverse)
library(readxl)

pollock <- readxl::read_xlsx("data/Covariate_B_pollock_recruitment.xlsx") %>%
  dplyr::mutate(pollock_recruit_scale = as.numeric(scale(Recruit_age_1_millions))) 
 
ggplot(data = pollock) +
  geom_point(aes(x=Year, y = pollock_recruit_scale)) +
  geom_line(aes(x=Year, y = pollock_recruit_scale)) +
  theme_classic() +
  geom_hline(yintercept = 0, linetype =2) +
  ylab("Pollock Recruitment") +
  xlab("Year")


ggplot(data = pollock ) +
  geom_point(aes(x=Year, y = Recruit_age_1_millions)) +
  geom_line(aes(x=Year, y = Recruit_age_1_millions)) +
  theme_classic() +
  # geom_hline(yintercept = 0, linetype =2) +
  ylab("Pollock Recruitment") +
  xlab("Year")


write_csv(pollock,"data/processed_covariates/Stage_A_Pollock_Recruitment.csv")
