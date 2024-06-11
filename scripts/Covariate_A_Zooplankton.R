# Create covariate for stage 1, representing ocean entry marine diet index
# current spatial trim is just eyeballing from vast chum index that curry sent
# current latitude range that I trim zoop index too: 58-65
# Temporal trim to include month 7-10

library(here)
library(tidyverse)
library(dplyr)
library(lubridate)
library(mgcv)
 
DF  <- read.csv(here("data", "Processed_Data", "NBS_Zoop_Process_Final.csv")) 

  zoop <- DF %>% 
    group_by(DATA_SOURCE,CRUISE,HAUL_ID,YEAR,MONTH,DAY,LAT,LON, TAXON_NAME,TAXA_COARSE) %>% # sum across life stages 
    dplyr::summarise(EST_NUM_PERM3 =  sum(EST_NUM_PERM3)) %>%
    filter(!LAT<58,
           !LAT>65,
           !LON> -155,
           !LON< -172,
           MONTH %in% c(7,8,9,10)) %>% 
    unite("date", c(YEAR, MONTH, DAY), sep = "/", remove = FALSE) %>%
    dplyr::mutate(date = as.Date(date, "%Y/%m/%d"),
                  DOY = yday(date)) 

names <- data.frame(unique(zoop$TAXA_COARSE))

# Large zoop (Themisto, calanus, large copepods) ================
large_zoop<- zoop %>% 
  filter(stringr::str_detect(TAXA_COARSE, paste(c( 'Themisto', 
                                                    'Calanus', 
                                                    'Copepod_large', 
                                                    'Neocalanus'), collapse = '|')) | 
           stringr::str_detect(TAXA_COARSE, paste(c( 'Themisto', 
                                                     'Calanus', 
                                                     'Copepod_large', 
                                                     'Neocalanus'), collapse = '|')))

# plot raw dat ===========
# are there differences between EMA and ECOFOCI just based on plotting averages
large_zoopsumm <- large_zoop %>% 
  filter(!YEAR <1999) %>%  
  group_by(DATA_SOURCE,YEAR) %>%
  dplyr::summarise(mean = mean(EST_NUM_PERM3),
                   n= nrow(.), 
                  se= sd(EST_NUM_PERM3)/sqrt(n))  


ggplot(data = large_zoopsumm, aes(x=YEAR, y = mean, group =DATA_SOURCE, color =DATA_SOURCE )) +
  geom_point()+
  geom_line() +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se))  +
  ggtitle("Large Zoop")

# Index Standardization for Large Zoop ================= 
mod_df_largezoop <- large_zoop %>% 
  filter(!YEAR <1999) %>%  
  group_by(DATA_SOURCE,YEAR, DOY,LAT,LON) %>% # sum across species 
  dplyr::summarise(sum_EST_NUM_PERM3 = sum(EST_NUM_PERM3))  %>% 
  mutate(YEAR = as.factor(YEAR),
         DATA_SOURCE = as.factor(DATA_SOURCE)) %>% 
  data.frame()
 

large_zoop_index <- mgcv::gam( sqrt(sum_EST_NUM_PERM3) ~ YEAR + DATA_SOURCE + 
                                s(LAT, LON) + s(DOY),
                              data = mod_df_largezoop, 
                              family = tw(link = "log"))
summary(large_zoop_index)
draw(large_zoop_index)

temp <- expand.grid(YEAR = c(unique(mod_df_largezoop$YEAR)),  
                       DATA_SOURCE = c("EcoDAAT"))

large_pred_df <- cbind(temp, 
                      data.frame(LAT = mean(mod_df_largezoop$LAT),#currently using M2, survey mean: rep(mean(scale_dat$LAT), times = nrow(DF1)),
                                 LON = mean(mod_df_largezoop$LON), #rep(mean(scale_dat$LON), times = nrow(DF1)),
                                 DOY = mean(mod_df_largezoop$DOY)))  

temp <- predict(large_zoop_index, large_pred_df, se.fit = TRUE )

large_pred_df$pred <-  exp(temp[[1]]) 
large_pred_df$se <-   exp(temp[[2]])

ggplot(data = large_pred_df, aes(x=YEAR, y = pred,
                           group =DATA_SOURCE, color =DATA_SOURCE)) +
  geom_point()+
  geom_line() +
  geom_errorbar(aes(ymin = pred-se, ymax = pred+se), width = 0.1)  +
  ggtitle("Large Zoop Modeled Index") + 
  theme_classic()

# mean scale ===========
large_pred_df <- large_pred_df %>%
  mutate(id = "Large_zoop")

ggplot(data = large_pred_df, aes(x=YEAR, y = pred_scale,
                           group =DATA_SOURCE)) +
  geom_point()+
  geom_line() +
  geom_errorbar(aes(ymin = pred_scale-se, ymax = pred_scale+se), width = 0.1)  +
  ggtitle("Large Zoop Modeled Index") + 
  ylab("Predicted Index") + 
  theme_classic() +
  geom_hline(yintercept =0, 
             linetype = 2) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  

 # Gelatinous Zoop - Cnideria ===============
cnideria_zoop<- zoop %>% 
   filter(stringr::str_detect(TAXA_COARSE, 
                              paste(c('Cnidaria_small', 'Cnidaria_large'), collapse = '|')) | 
          stringr::str_detect(TAXON_NAME, 
                              paste(c('Cnidaria_small', 'Cnidaria_large'), collapse = '|')))
 
 zoop_cnideria_summ <- cnideria_zoop %>% 
   filter(!YEAR <1999) %>% 
   group_by(DATA_SOURCE, YEAR) %>%
   dplyr::summarise(mean = mean(EST_NUM_PERM3),
                    n= nrow(.), 
                    se = sd(EST_NUM_PERM3)/sqrt(n))  
 
 ggplot(data = zoop_cnideria_summ, aes(x=YEAR, y = mean, 
                                       group =DATA_SOURCE, color =DATA_SOURCE )) +
   geom_point()+
   geom_line() +
   geom_errorbar(aes(ymin = mean-se, ymax = mean+se))  

 # Index Standardization for Cnideria ================= 
 mod_df_cnideriazoop <-  cnideria_zoop %>% 
   filter(!YEAR <1999) %>%  
   group_by(DATA_SOURCE,YEAR, DOY,LAT,LON) %>% # sum across species 
   dplyr::summarise(sum_EST_NUM_PERM3 = sum(EST_NUM_PERM3))  %>% 
   mutate(YEAR = as.factor(YEAR),
          DATA_SOURCE = as.factor(DATA_SOURCE)) %>% 
   data.frame()
 
 cnideria_zoop_index <- mgcv::gam( sqrt(sum_EST_NUM_PERM3) ~ YEAR + DATA_SOURCE + 
                                  s(LAT, LON) + s(DOY),
                                data = mod_df_cnideriazoop, 
                                family = tw(link = "log"))
 summary(cnideria_zoop_index)
 draw(cnideria_zoop_index)
 
 temp <- expand.grid(YEAR = c(unique(mod_df_cnideriazoop$YEAR)),  
                     DATA_SOURCE = c("EcoDAAT"))
 
 cnideria_pred_df <- cbind(temp, 
                  data.frame(LAT = mean(mod_df_cnideriazoop$LAT),#currently using M2, survey mean: rep(mean(scale_dat$LAT), times = nrow(DF1)),
                             LON = mean(mod_df_cnideriazoop$LON), #rep(mean(scale_dat$LON), times = nrow(DF1)),
                             DOY = mean(mod_df_cnideriazoop$DOY)))  
 
 temp <- predict(cnideria_zoop_index, cnideria_pred_df, se.fit = TRUE )
 
 cnideria_pred_df$pred <-  exp(temp[[1]]) 
 cnideria_pred_df$se <-   exp(temp[[2]])
 
 cnideria_pred_df <- cnideria_pred_df %>%
                         dplyr::mutate( id = "Cnideria")
 
 
# years in covariate timeseries to expand the TS that need it  =====
 YEAR <-data.frame(YEAR = seq(from = 2002, to = 2021, by =1))
 
 temp <-large_pred_df %>%
   rbind(cnideria_pred_df) %>% 
   dplyr::select(YEAR, pred, id) %>% 
   spread(id, pred) %>% 
   dplyr::summarise(mean_cnideria = mean(Cnideria), #rnorm(mean(Cnideria), 0.001), 
             mean_largezoop = mean(Large_zoop)) #rnorm(mean(Large_zoop), 0.001))
# a<- rnorm(1,0, 0.1)
# b<-rnorm(2,mean(temp$mean_largezoop), 0.1)

pred_df <- large_pred_df %>%
              rbind(cnideria_pred_df) %>%
              dplyr::select(YEAR, pred, id) %>% 
              spread(id, pred)  %>% 
              dplyr::mutate(YEAR = as.numeric(as.character(YEAR))) %>% 
              right_join(YEAR) %>% 
              dplyr::mutate(Cnideria = case_when(is.na(Cnideria) ~ mean(temp$mean_cnideria)  ,
                                          TRUE ~ Cnideria),
                            Large_zoop = case_when(is.na(Large_zoop) ~ mean(temp$mean_largezoop),
                                          TRUE ~ Large_zoop)) 
                            Large_zoop = as.numeric(scale(Large_zoop)),
                            Cnideria = as.numeric(scale(Cnideria)))       

# plots =============
 ggplot(data = pred_df, aes(x=YEAR, y = pred,
                            group =id, color=id)) +
   geom_point()+
   geom_line() +
   geom_errorbar(aes(ymin = pred-se, ymax = pred+se), width = 0.1)  +
   ggtitle("Cnideria Modeled Index") + 
   theme_classic()
 
 ggplot(data = pred_df, aes(x=YEAR, y = pred_scale,
                            group =id, color=id)) +
   geom_point()+
   geom_line() +
   geom_errorbar(aes(ymin = pred_scale-se, ymax = pred_scale+se), width = 0.1)  +
   ggtitle("Scale Modeled Index") + 
   theme_classic() +
   geom_hline(yintercept = 0, linetype = 2)
 
 # save data  =============
 write_csv(pred_df, "data/processed_covariates/Stage_A_Zooplankton_Index.csv")
 
 
 
 # these are not used in model ============
 ## Estimate Spatial Temporal Fields? ============================ 
 largezoop_ST_index <- mgcv::gam(sqrt(sum_EST_NUM_PERM3) ~ YEAR + DATA_SOURCE + 
                                            s(LON,LAT, by = YEAR) + s(DOY),
                                          data = mod_df_largezoop, 
                                          family = tw(link = "log"))
 
  cnideria_ST_index <- mgcv::gam(sqrt(sum_EST_NUM_PERM3) ~ YEAR + DATA_SOURCE + 
                                     s(LON,LAT, by = YEAR) + s(DOY),
                                   data = mod_df_cnideriazoop, 
                                   family = tw(link = "log"))
 
 summary(cnideria_ST_index)
 
 draw(largezoop_ST_index)
 draw(cnideria_ST_index)
# mean scale ======= 
  # OLD ============
  ## Themisto ================
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
