# Create covariate for stage 1, representing ocean entry marine diet index
# current spatial trim is just eyeballing from vast chum index that curry sent
# current latitude range that I trim zoop index too: 58-65
# Temporal trim to include month 7-10

library(here)
library(tidyverse)

zoop <- read_csv(here("data", "Processed_Data", "NBS_Zoop_Process_Final.csv")) %>% 
  group_by(CRUISE,HAUL_ID,YEAR,MONTH,DAY,LAT,LON, TAXON_NAME,TAXA_COARSE) %>% # sum across life stages 
  dplyr::summarise(EST_NUM_PERM3 = sqrt(sum(EST_NUM_PERM3))) %>%
  filter(!LAT<58,
         !LAT>65,
         !LON> -155,
         MONTH %in% c(7,8,9,10))

names <- data.frame(unique(zoop$TAXA_COARSE))

# Large zoop (Themisto, calanus, large copepods) ================
large_zoop<- zoop %>% 
  filter(stringr::str_detect(TAXA_COARSE, paste(c( 'Themisto', 
                                                    'Calanus', 
                                                    'Copepod_large', 
                                                  'Neocalanus'
                                                  ), collapse = '|')))

large_zoopsumm <- large_zoop %>% 
  filter(!YEAR <1999) %>%  
  group_by(YEAR) %>%
  dplyr::summarise(mean = mean(EST_NUM_PERM3),
                   n= nrow(.), 
                  se= sd(EST_NUM_PERM3)/sqrt(n)) #%>% 
  # dplyr::mutate(mean_scale = as.numeric(scale(mean)),
  #               sd = as.numeric(scale(sd)))  


ggplot(data = large_zoopsumm, aes(x=YEAR, y = mean)) +
  geom_point()+
  geom_line() +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se))

 write_csv(large_zoopsumm, "data/processed_covariates/covariate_large_zooplankton.csv")

 # Gelatinous Zoop - Cnideria ===============
 zoop_cnideria<- zoop %>% 
   filter(stringr::str_detect(TAXA_COARSE, paste(c('Cnidaria_small', 'Cnidaria_large'), collapse = '|')))
 
 zoop_cnideria_summ <- zoop_cnideria %>% 
   filter(!YEAR <1999) %>% 
   group_by(YEAR) %>%
   dplyr::summarise(mean = mean(EST_NUM_PERM3),
                    n= nrow(.), 
                    se = sd(EST_NUM_PERM3)/sqrt(n))  
 
 ggplot(data = zoop_cnideria_summ, 
        aes(x=YEAR, y = mean)) +
   geom_point()+
   geom_line() +
   geom_errorbar(aes(ymin = mean-se, ymax = mean+se))  
   
 write_csv(zoop_cnideria_summ, "data/processed_covariates/covariate_gelatinous_zooplankton.csv")
 
 # plot both zoop and gelatinous on the same plot ===========
 ggplot( ) + 
   geom_line(data = zoop_cnideria_summ, aes(x=YEAR, y = mean ), color = "darkgreen") +
   geom_errorbar(data = zoop_cnideria_summ, aes(x=YEAR,ymin = mean-se, ymax = mean+se),color = "darkgreen") + 
   geom_line(data = large_zoopsumm, aes(x=YEAR, y = mean ), color = "blue") +
   geom_errorbar(data = large_zoopsumm, aes(x=YEAR,ymin = mean-se, ymax = mean+se),color = "blue")  
   

# Themisto ================
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

### Spatial data distribution ============
zoop_multiple_space <- large_zoop %>% 
  filter(!YEAR <1999) %>% 
  group_by(YEAR, LAT, LON) %>%
  dplyr::summarise(mean = mean(EST_NUM_PERM3),
                   sd = sd(EST_NUM_PERM3))  

ggplot(data = zoop_multiple_space, 
       aes(x=LON, y = LAT,
          fill = log(mean), color = log(mean))) +
  geom_point() +
  facet_wrap(~YEAR)

### Temporal data distribution ============
zoop_multiple_space <- large_zoop %>% 
  filter(!YEAR <1999) %>% 
  group_by(YEAR, LAT, LON) %>%
  dplyr::summarise(mean = mean(EST_NUM_PERM3),
                   sd = sd(EST_NUM_PERM3))  

hist(as.numeric(zoop_multiple_space$MONTH))

ggplot(data = zoop_multiple_space, aes(x=LON, y = LAT,
                                       fill = log(mean), color = log(mean))) +
  geom_point() +
  facet_wrap(~YEAR)
