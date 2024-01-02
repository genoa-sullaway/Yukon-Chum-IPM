# Explore zoop data for zoop in chum diet that was prominent in Murphy et al 2016: Distribution, Diet, and Bycatch of Chum Salmon in the Eastern Bering Sea
# chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://meetings.npfmc.org/CommentReview/DownloadFile?p=fd891fa4-b73b-4cd6-922c-199e1d558a60.pdf&fileName=2.%20Distribution%2C%20Diet%20and%20Bycatch%20of%20chum%20salmon%20EBS%20Murphy%20et%20al%2C%202016.pdf
library(here)
library(tidyverse)

zoop <- read_csv(here("data", "Processed_Data", "NBS_Zoop_Process_Final.csv")) %>% 
  group_by(CRUISE,HAUL_ID,YEAR,MONTH,DAY,LAT,LON, TAXON_NAME,TAXA_COARSE) %>% # sum across life stages 
  dplyr::summarise(EST_NUM_PERM3 = sum(EST_NUM_PERM3))

themisto<- zoop %>% filter(stringr::str_detect(TAXA_COARSE, 'Themisto') )

themisto_summ <- themisto %>% 
  group_by(YEAR) %>%
  dplyr::summarise(mean = mean(EST_NUM_PERM3),
         sd = sd(EST_NUM_PERM3)) %>% 
  dplyr::mutate(mean_scale = scale(mean),
         sd = scale(sd))

ggplot(data = themisto_summ, aes(x=YEAR, y = mean_scale)) +
  geom_point()+
  geom_line() +
  geom_errorbar(aes(ymin = mean_scale-sd, ymax = mean_scale+sd))

zoop_multiple<- zoop %>% 
  filter(stringr::str_detect(TAXA_COARSE, paste(c('Themisto', 'Calanus'), collapse = '|')))

zoop_multiple_summ <- zoop_multiple %>% 
  filter(!YEAR <1999) %>% 
  group_by(YEAR) %>%
  dplyr::summarise(mean = mean(EST_NUM_PERM3),
                   sd = sd(EST_NUM_PERM3)) %>% 
  dplyr::mutate(mean_scale = as.numeric(scale(mean)),
                sd = as.numeric(scale(sd)))  


ggplot(data = zoop_multiple_summ, aes(x=YEAR, y = mean_scale)) +
  geom_point()+
  geom_line() +
  geom_errorbar(aes(ymin = mean_scale-sd, ymax = mean_scale+sd))

write_csv(zoop_multiple_summ, "data/zoop_covariate.csv")

### Plot where there is data annually 

zoop_multiple_space <- zoop_multiple %>% 
  filter(!YEAR <1999) %>% 
  group_by(YEAR, LAT, LON) %>%
  dplyr::summarise(mean = mean(EST_NUM_PERM3),
                   sd = sd(EST_NUM_PERM3)) %>% 
  dplyr::mutate(mean_scale = as.numeric(scale(mean)),
                sd = as.numeric(scale(sd)))  

ggplot(data = zoop_multiple_space, aes(x=LAT, y = LON, fill = mean_scale)) +
  geom_point() +
  facet_wrap()




