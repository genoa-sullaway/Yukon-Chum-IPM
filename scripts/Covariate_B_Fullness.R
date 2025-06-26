library(mgcv)
library(tidyverse)
library(readxl)

# process stomach fullness, make fullness index. 

Fullness <- read_xlsx("data/NBS_JChumFullness.xlsx")  
 
fullness_df <- Fullness %>% 
  filter(!is.na(Number_of_Stomachs)) %>% 
  dplyr::rename(fullness =`Stomach_fullness_index(o/ooo)`,
                Lat = `EQ Latitude`,
                Lon = `EQ Longitude`,
                stomach_weight = `Stomach_Weight(g)`) %>%
  dplyr::mutate(#num_stomachs_scale = as.numeric(scale(Number_of_Stomachs)),
                SampleYear_factor = as.factor(SampleYear),
                GearCode = as.factor(GearCode)) %>% 
        filter(!Lat>65)

sd(fullness_df$fullness)

mean(fullness_df$Lat)
mean(fullness_df$Lon)
unique(fullness_df$SampleYear_factor)
 
# plot

ggplot(data = fullness_df)+
  geom_point(aes(x=Lon, y =Lat, fill = fullness, color = fullness))+
  facet_wrap(~SampleYear_factor)

# model
# stomach fullness index with a tweedie
full_mod <- mgcv::gam(fullness ~ SampleYear_factor + s(Lat,Lon), weights = Number_of_Stomachs,
                      data = fullness_df, family = tw(link="log"))

# summary(full_mod)

# predict ===============
pred_df = data.frame(expand_grid(SampleYear_factor = unique(fullness_df$SampleYear_factor),
                                 Lat=mean(fullness_df$Lat),
                                 Lon=mean(fullness_df$Lon)
                                 #GearCode = "300400"
                                 ))


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

mean_df <- fullness_df %>%
  group_by(SampleYear) %>%
  dplyr::summarise(mean = mean(fullness))

 ggplot(data =  temp, aes(x=SampleYear, y =pred)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),alpha = 0.5 ) +
  geom_point() +
  geom_line() +
  theme_classic() + 
  xlab(" ") + 
  ylab("Predicted Fullness")   

 year = data.frame(SampleYear = c(seq(from = 2002, to = 2023, by =1)))
 
temp_scale <- temp %>%
  dplyr::mutate(full_index_scale = as.numeric(scale(pred))) %>%  
  right_join(year) %>%
  dplyr::mutate(SampleYear_add1 = SampleYear+1, # only have +1 here so that all can get a -3 for brood year  
                full_index_scale = replace_na(full_index_scale, 0)) %>%
  dplyr::select(SampleYear_add1,full_index_scale) %>%
  dplyr::arrange(SampleYear_add1)
 
 # ggplot(data =  temp_scale, aes(x=SampleYear_add1, y =pred)) +
 #   geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),width = 0.1 ) +
 #   geom_point() +
 #   geom_line() +
 #   theme_classic() + 
 #   xlab(" ") + 
 #   ylab("Predicted Fullness")   
  
 saveRDS(temp_scale, "data/processed_covariates/fullness_cov.RDS")
 
  