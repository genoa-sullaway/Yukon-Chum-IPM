yukon_fall_agesbrood <- readxl::read_excel("data/age_comps/Fall_Yukon_Calc_Source4.xlsx") %>%  # created in make_fall_yukon_age_comp.R - source from JTC report
  dplyr::select(c(1:5))  %>% 
  gather(2:5, key = "age", value = "abund") %>%
  group_by(brood_year) %>%
  dplyr::mutate(sum = sum(abund),
                percent = abund/sum) %>%
  dplyr::select(brood_year,age,percent) %>%
  spread(age, percent) %>% 
  filter(brood_year>1980 & brood_year<2017) 

colMeans(yukon_fall_agesbrood)

yukon_fall_agescal<-readxl::read_excel("data/age_comps/Fall_Yukon_Calc_Source4.xlsx") %>%  # created in make_fall_yukon_age_comp.R - source from JTC report
  dplyr::select(c(1:5)) %>% 
  gather(2:5, key = "age", value = "abund") %>%
  mutate(cal_year = case_when(age == "abund_0.3" ~ brood_year +3,
                              age == "abund_0.4" ~ brood_year +4,
                              age == "abund_0.5" ~ brood_year +5,
                              age == "abund_0.6" ~ brood_year +6)) %>%
  group_by(cal_year) %>%
  dplyr::mutate(sum = sum(abund),
                percent = abund/sum) %>%
  dplyr::select(cal_year,age,percent) %>%
  spread(age, percent) %>% 
  filter(cal_year>1980 & cal_year<2017)

colMeans(yukon_fall_agescal)

