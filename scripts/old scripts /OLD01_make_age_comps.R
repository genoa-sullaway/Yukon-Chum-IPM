# load age structure data that i assimilated from different sources
# see: data/age_comps/chum_age_comp_source_notes.xlsx for the source info and status of compiling age comps

library(tidyverse)
library(readxl)
library(here)

age_comp<-(read_excel("data/age_comps/age_comp_assimilated.xlsx")) %>%
  mutate(percent = as.numeric(percent)) %>%
  as.data.frame()

# plot summer yukon ==================
summer_yukon <- age_comp %>%
  filter(source_id == 5)  %>%
  mutate(age = factor(age, levels = c("0.3", "0.4", "0.5", "0.6")))

ggplot(data =summer_yukon) +
  geom_line(aes(x=year,y=percent, group = age, color = age)) +
  theme_classic() 

  ggplot(data = summer_yukon) +
  geom_bar(aes(x = year, y = percent, color = age, fill =age), 
           position="stack", stat="identity" )+ 
  theme_classic()
   
  # plot fall yukon ==================
 fall_yukon <- age_comp %>%
    filter(source_id == 4)  %>%

 ggplot(data =fall_yukon) +
    geom_line(aes(x=cal_year,y=as.numeric(percent), group = age, color = age)) +
    theme_classic()
  
  ggplot(data = fall_yukon) +
    geom_bar(aes(x = cal_year, y = percent, color = age, fill =age), 
             position="stack", stat="identity" )+ 
    theme_classic()

  ggplot(data =fall_yukon) +
    geom_line(aes(x=brood_year,y=percent, group = age, color = age)) +
    theme_classic() 
  
  ggplot(data = fall_yukon) +
    geom_bar(aes(x = brood_year, y = percent, color = age, fill =age), 
             position="stack", stat="identity" )+ 
    theme_classic()
  