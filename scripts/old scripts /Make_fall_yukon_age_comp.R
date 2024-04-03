library(tidyverse)
library(here)

yukon_df <- readxl::read_excel("data/age_comps/Fall_Yukon_Calc_Source4.xlsx") %>%
  select(c(1:5)) %>% 
  gather(2:5, key = "age", value = "abund") %>%
  # mutate(cal_year = case_when(age == "abund_0.3" ~ brood_year +3,
  #                             age == "abund_0.4" ~ brood_year +4,
  #                             age == "abund_0.5" ~ brood_year +5,
  #                             age == "abund_0.6" ~ brood_year +6)) %>%
  group_by(brood_year) %>%
  dplyr::mutate(sum = sum(abund),
                percent = abund/sum,
                source_id = 4,
                location_a = "fall_yukon",
                location_b =NA) %>%
  select(brood_year,source_id, location_a, location_b,age,percent)

write_csv(yukon_df,"data/age_comps/processed_age_comps_Fallyukon_source4.csv")
