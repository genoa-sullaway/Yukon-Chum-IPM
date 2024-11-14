library(mgcv)
library(tidyverse)
library(readxl)

# process stomach fullness, make fullness index. 

 Fullness <- read_xlsx("data/NBS_JChumFullness.xlsx")  
 
fullness_df <- Fullness %>% 
  filter(!is.na(Number_of_Stomachs)#,
         #!SampleYear == 2023
         ) %>% 
  dplyr::rename(fullness =`Stomach_fullness_index(o/ooo)`,
                Lat = `EQ Latitude`,
                Lon = `EQ Longitude`,
                stomach_weight = `Stomach_Weight(g)`) %>%
  dplyr::mutate(#num_stomachs_scale = as.numeric(scale(Number_of_Stomachs)),
                SampleYear_factor = as.factor(SampleYear),
                GearCode = as.factor(GearCode)) %>% 
        filter(!Lat>65) 

# plot

ggplot(data = fullness_df)+
  geom_point(aes(x=Lon, y =Lat, fill = fullness, color = fullness))+
  facet_wrap(~SampleYear_factor)

# model
# stomach fullness index with a tweedie

full_mod <- mgcv::gam(fullness ~ SampleYear_factor + s(Lat,Lon) + GearCode, weights = Number_of_Stomachs,
                      data = fullness_df, family = tw(link="log"))

summary(full_mod)

# predict ===============
pred_df = data.frame(expand_grid(SampleYear_factor = unique(fullness_df$SampleYear_factor),
                                 Lat=mean(fullness_df$Lat),
                                 Lon=mean(fullness_df$Lon),
                                 GearCode = "300400"))
 
temp <- predict(full_mod,pred_df, se.fit = T,type = "response" )

pred_df$pred<-  (temp[[1]]) 

pred_df$se<-   (temp[[2]]) 

unique_year <- unique(fullness_df[c("SampleYear","SampleYear_factor")])

temp <- pred_df %>% 
  dplyr::mutate(
    lower_ci =  (pred - (1.96*(se))),
    upper_ci =  (pred + (1.96*(se))),
    pred =  (pred))  %>%
  left_join(unique_year) %>%
  as.data.frame()

 ggplot(data =  temp, aes(x=SampleYear, y =pred)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),alpha = 0.5 ) +
  geom_point() +
  geom_line() +
  theme_classic() + 
  xlab(" ") + 
  ylab("Predicted Fullness")   

 year = data.frame(SampleYear = c(seq(from = 2002, to = 2023, by =1)))

 test <- left_join(year,temp)
 
 # add in rolling means for blank years??
 
 # 2002,2008, 2014
 y2002<- test %>% 
   filter(SampleYear %in% c(2003,2004,2005 )) %>%
   dplyr::summarise(mean = mean(pred))
 
 y2008<- test %>% 
   filter(SampleYear %in% c(2007,2009,2010)) %>%
   dplyr::summarise(mean = mean(pred))
 
 y2014<- test %>% 
   filter(SampleYear %in% c(2013,2015,2016)) %>%
   dplyr::summarise(mean = mean(pred))
 
 y2020<- test %>% 
   filter(SampleYear %in% c(2021,2018,2019)) %>%
   dplyr::summarise(mean = mean(pred))

 y2022<- test %>% 
   filter(SampleYear %in% c(2023,2021, 2019)) %>%
   dplyr::summarise(mean = mean(pred))
 
 temp_full <- test %>%
   dplyr::mutate(pred = case_when(SampleYear ==2002 ~ y2002[1,1],
                                  SampleYear ==2008 ~ y2008[1,1],
                                  SampleYear ==2014 ~ y2014[1,1],
                                  SampleYear ==2020 ~ y2020[1,1],
                                  SampleYear ==2022 ~ y2022[1,1],
                                  TRUE ~ pred)) %>%
   dplyr::mutate(year_addone = SampleYear+1) # must add one so that they can all be -2 for cov b... 
 
 ggplot(data =  temp_full, aes(x=SampleYear, y =pred)) +
   geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),alpha = 0.5 ) +
   geom_point() +
   geom_line() +
   theme_classic() + 
   xlab(" ") + 
   ylab("Predicted Fullness")   
 
# save factor year fullness index
 # write_csv(temp, "data/processed_covariates/fullness_cov.csv")

 saveRDS(temp_full, "data/processed_covariates/fullness_cov.RDS")
 
 
# mod with year smooth =====
#  
# smooth_mod <- mgcv::gam(fullness ~ s(SampleYear) + s(Lat,Lon) + GearCode, weights = Number_of_Stomachs,
#                        data = fullness_df, family = tw(link="log"))
#  
#  
#  pred_df = data.frame(expand_grid(  
#    SampleYear  = unique(fullness_df$SampleYear),
#    Lat=mean(fullness_df$Lat),
#    Lon=mean(fullness_df$Lon),
#    GearCode = "300400"))
#  
#  # aug_temp=unique(zoop_df$aug_temp))) %>% 
#  # left_join(temperatures )
#  
#  
#  temp <- predict(smooth_mod,pred_df, se.fit = T,type = "response" )
#  
#  pred_df$pred<-  (temp[[1]]) 
#  
#  pred_df$se<-   (temp[[2]]) 
#  
#  temp <- pred_df %>% 
#    # group_by(CPUE_stickleback) %>% 
#    # dplyr::summarise(se = sd(pred)/sqrt(length(pred)),
#    #                  pred = mean(pred)) %>% 
#    dplyr::mutate(
#      lower_ci =  (pred - (1.96*(se))),
#      upper_ci =  (pred + (1.96*(se))),
#      pred =  (pred)
#    )   
#  
#  ggplot(data =  temp, aes(x=SampleYear, y =pred)) +
#    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),alpha = 0.5 ) +
#    geom_point() +
#    geom_line() +
#    theme_classic() + 
#    xlab(" ") + 
#    ylab("Predicted Fullness")   
#  
#  
#  
#  
#  
#  
#  