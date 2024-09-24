# Create covariate for stage 1, representing ocean entry marine diet index
# current spatial trim is just eyeballing from vast chum index that curry sent
# current latitude range that I trim zoop index too: 58-65
# Temporal trim to include month 7-10

library(here)
library(tidyverse)
library(dplyr)
library(lubridate)
library(mgcv)
library(sf)
 
DF  <- read.csv(here("data", "Processed_Data", "NBS_Zoop_Process_Final.csv")) 

 zoop <- DF %>% 
    group_by(CRUISE,HAUL_ID,GEAR_NAME,YEAR,MONTH,DAY,LAT,LON, DATA_SOURCE,TAXON_NAME) %>% # sum across life stages 
    dplyr::summarise(EST_NUM_PERM3 =  sum(EST_NUM_PERM3)) %>%
    filter(!LAT<58,
           !LAT>65,
           !LON> -155,
           !LON< -172,
           MONTH %in% c(5,6,7,8,9)) %>% 
    unite("date", c(YEAR, MONTH, DAY), sep = "/", remove = FALSE) %>%
    dplyr::mutate(date = as.Date(date, "%Y/%m/%d"),
                  DOY = yday(date),
                  TAXA_COARSE = case_when(grepl(pattern = "Themisto", x=TAXON_NAME, ignore.case = TRUE) ~ "large_zoop",
                                          TAXON_NAME %in% c("Calanus pacificus", "Calanus marshallae", "Calanus glacialis") ~ "large_zoop",
                                          grepl(pattern = "Copepod_large", x=TAXON_NAME, ignore.case = TRUE) ~ "large_zoop",
                                          grepl(pattern = "Neocalanus", x=TAXON_NAME, ignore.case = TRUE) ~ "large_zoop",
                                          
                                          grepl(pattern = "Cnidaria", x=TAXON_NAME, ignore.case = TRUE) ~ "Cnideria",
                                          grepl(pattern = "Larvacea", x=TAXON_NAME, ignore.case = TRUE) ~ "Cnideria",
                                          grepl(pattern = "Medusozoa", x=TAXON_NAME, ignore.case = TRUE) ~ "Cnideria",
                                          grepl(pattern = "Hydrozoa", x=TAXON_NAME, ignore.case = TRUE) ~ "Cnideria",
                                          grepl(pattern = "Scyphozoa", x=TAXON_NAME, ignore.case = TRUE) ~ "Cnideria",
                                          grepl(pattern = "Thaliacea", x=TAXON_NAME, ignore.case = TRUE) ~ "Cnideria",
                                          grepl(pattern = "Ctenophora", x=TAXON_NAME, ignore.case = TRUE) ~ "Cnideria",
                                         grepl(pattern = "Oikopleura", x=TAXON_NAME, ignore.case = TRUE) ~ "Cnideria",
                                          TRUE ~ "other")) 

names <- data.frame(unique(zoop$TAXON_NAME))

# Large zoop (Themisto, calanus, large copepods) ================
large_zoop<- zoop %>% 
  filter(TAXA_COARSE == "large_zoop") 

## plot raw data ===========
# are there differences between EMA and ECOFOCI just based on plotting averages
large_zoopsumm <- large_zoop %>%
  group_by(CRUISE,HAUL_ID,YEAR,MONTH,DAY,LAT,LON, DATA_SOURCE,TAXON_NAME) %>% # sum across life stages 
  dplyr::summarise(EST_NUM_PERM3 = sum(EST_NUM_PERM3)) %>% 
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

# different plot large zoop =============
testLZ <- large_zoop %>%
  filter(!YEAR <1999) %>%
  group_by(TAXON_NAME,YEAR) %>%
  dplyr::summarise(mean = mean(EST_NUM_PERM3),
                   n= nrow(.),
                   se= sd(EST_NUM_PERM3)/sqrt(n))

ggplot(data = testLZ, aes(x=YEAR, y = mean, group =TAXON_NAME, color =TAXON_NAME )) +
  geom_point()+
  geom_line() +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se))  +
  ggtitle("Large Zoop") +
  facet_wrap(~TAXON_NAME, scales = "free")


## Index Standardization for Large Zoop ================= 
mod_df_largezoop <- large_zoop %>% 
  filter(!YEAR <2000) %>%  
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
gratia::draw(large_zoop_index)

temp <- expand.grid(YEAR = c(unique(mod_df_largezoop$YEAR)),  
                       DATA_SOURCE = c("EcoDAAT"))

large_pred_df <- cbind(temp, 
                      data.frame(LAT = mean(mod_df_largezoop$LAT), 
                                 LON = mean(mod_df_largezoop$LON),  
                                 DOY = mean(mod_df_largezoop$DOY)))  

temp <- predict(large_zoop_index, large_pred_df, se.fit = TRUE, response = TRUE )

large_pred_df$pred <- exp(temp[[1]]) 
large_pred_df$se <-   exp(temp[[2]])

ggplot(data = large_pred_df, aes(x=YEAR, y = pred,
                           group =DATA_SOURCE, color =DATA_SOURCE)) +
  geom_point()+
  geom_line() +
  geom_errorbar(aes(ymin = pred-se, ymax = pred+se), width = 0.1)  +
  ggtitle("Large Zoop Modeled Index") + 
  theme_classic()
 
 # Gelatinous Zoop - Cnideria ===============
cnideria_zoop<- zoop %>% 
   filter(TAXA_COARSE == "Cnideria",
          # !EST_NUM_PERM3>3000,
          GEAR_NAME == "60BON") 

## Trim inner shelf Cnideria ==============
# Load Ortiz region shapefile
ortiz_shp <- st_read(here("data/ortiz_regions/BSIERP_regions_2012.shp"))
ortiz_shp <- st_transform(ortiz_shp, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")#Tranform into WGS84 coordinate system
ortiz_shp <- subset(ortiz_shp) #, !DOMAIN == 15)
sf_ortiz = st_make_valid(st_as_sf(ortiz_shp)) %>% 
  filter(!DOMAIN %in% c(15,16)) #convert your shape file to sf, too. need to use make valid because
#of intersecting polygons in shape file and sf doesnt like that data format anymore.

sf_df = st_as_sf(cnideria_zoop, coords = c("LON", "LAT"), crs = 4326) # change df to sf

#source("scripts/Function_Make_NewSpatialRegions.R")

#First do a join that contains the points to filter points that are totally outside the region. 
df_contains <- st_join(sf_ortiz, sf_df, join = st_contains) #you can change the join command to do nearest neighbor etc

#need to do some tidying to, rejoin original lat longs and plot to look at trim
st_geometry(df_contains)<-NULL #if you want to keep your data as a sf object skip this step, I want to convert back to a standard dataframe 

N_inner_cnideria<-df_contains %>%
  left_join(cnideria_zoop) %>%
  filter(name == "North middle shelf")

 zoop_cnideria_summ <- N_inner_cnideria %>% 
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
 
 # different plot large zoop =============
 testC <- cnideria_zoop %>%
   filter(!YEAR <1999) %>%
   group_by(TAXON_NAME,YEAR) %>%
   dplyr::summarise(mean = mean(EST_NUM_PERM3),
                    n= nrow(.),
                    se= sd(EST_NUM_PERM3)/sqrt(n))
 
 ggplot(data = testC, aes(x=YEAR, y = mean, group =TAXON_NAME, color =TAXON_NAME )) +
   geom_point()+
   geom_line() +
   geom_errorbar(aes(ymin = mean-se, ymax = mean+se))  +
   facet_wrap(~TAXON_NAME, scales = "free")
 
 ## Index Standardization for Cnideria ================= 
 mod_df_cnideriazoop <-  cnideria_zoop %>% 
   filter(!YEAR <1999) %>%  
   group_by(DATA_SOURCE,YEAR, DOY,LAT,LON) %>% # sum across species 
   dplyr::summarise(sum_EST_NUM_PERM3 = sum(EST_NUM_PERM3))  %>% 
   mutate(YEAR = as.factor(YEAR),
          DATA_SOURCE = as.factor(DATA_SOURCE)) %>% 
   data.frame()
 
 cnideria_zoop_index <- mgcv::gam(sqrt(sum_EST_NUM_PERM3) ~ YEAR + DATA_SOURCE + 
                                      s(LAT, LON) + s(DOY),
                                data = mod_df_cnideriazoop, family = tw(link = "log"))

 temp <- expand.grid(YEAR = c(unique(mod_df_cnideriazoop$YEAR)),  
                     DATA_SOURCE = c("EcoDAAT"))
 
 cnideria_pred_df <- cbind(temp, 
                  data.frame(LAT = mean(mod_df_cnideriazoop$LAT),#currently using M2, survey mean: rep(mean(scale_dat$LAT), times = nrow(DF1)),
                             LON = mean(mod_df_cnideriazoop$LON), #rep(mean(scale_dat$LON), times = nrow(DF1)),
                             DOY = mean(mod_df_cnideriazoop$DOY)))  
 
 temp <- predict(cnideria_zoop_index, cnideria_pred_df, se.fit = TRUE, response = TRUE)
 
 cnideria_pred_df$pred <- exp(temp[[1]]) 
 cnideria_pred_df$se <- (temp[[2]])
 
 cnideria_pred_df <- cnideria_pred_df %>%
                         dplyr::mutate(id = "Cnideria")
 
 add <-  data.frame(YEAR = c(as.factor(2023)),
                     Large_zoop = c(2.5),  
                     Cnideria = c(10.4))

# plot cnideria ==================
ggplot(data = cnideria_pred_df, aes(x=as.numeric(as.character(YEAR)), y = pred,
                                 group =DATA_SOURCE, color =DATA_SOURCE)) +
  geom_point()+
  geom_line() +
  geom_errorbar(aes(ymin = pred-se, ymax = pred+se), width = 0.1)  +
  ylab("Log Cnideria Modeled Index") + 
  ggtitle("NBS Middle Shelf") + 
  xlab("year") + 
  theme_classic()

 ggsave("output/cnideria_index_middle_shelf.png")

# bind both data sets ========== 
pred_df <- large_pred_df %>%
              dplyr::mutate( id = "Large_zoop") %>% 
              rbind(cnideria_pred_df) %>%
              dplyr::select(YEAR, pred, id) %>% 
              spread(id, pred)  %>% 
              rbind( add) %>% 
              dplyr::mutate(YEAR = as.numeric(as.character(YEAR)),
                            Large_zoop = as.numeric(scale(Large_zoop)),
                            Cnideria = as.numeric(scale(Cnideria)))  
## plots =============
 ggplot( ) +
   geom_line(data = pred_df, aes(x=YEAR, y = Large_zoop), color = "orange")+
   geom_line(data = pred_df, aes(x=YEAR, y = Cnideria), color = "green")+
   ggtitle("mean scaled Modeled Index") + 
   theme_classic()
 
 # save data  =============
 write_csv(pred_df, "data/processed_covariates/Stage_A_Zooplankton_Index.csv")
 
 # OLD ======== 
 # these are not used in model ============
 ## Estimate Spatial Temporal Fields? ============================ 
#  largezoop_ST_index <- mgcv::gam(sqrt(sum_EST_NUM_PERM3) ~ YEAR + DATA_SOURCE + 
#                                             s(LON,LAT, by = YEAR) + s(DOY),
#                                           data = mod_df_largezoop, 
#                                           family = tw(link = "log"))
#  
#   cnideria_ST_index <- mgcv::gam(sqrt(sum_EST_NUM_PERM3) ~ YEAR + DATA_SOURCE + 
#                                      s(LON,LAT, by = YEAR) + s(DOY),
#                                    data = mod_df_cnideriazoop, 
#                                    family = tw(link = "log"))
#  
#  summary(cnideria_ST_index)
#  draw(largezoop_ST_index)
#  draw(cnideria_ST_index)
#  
# # mean scale ======= 
#    ## OLD ============
#   ## Themisto ================
# themisto<- zoop %>% filter(stringr::str_detect(TAXA_COARSE, 'Themisto') )
# 
# themisto_summ <- themisto %>% 
#   group_by(YEAR) %>%
#   dplyr::summarise(mean = mean(EST_NUM_PERM3),
#                    sd = sd(EST_NUM_PERM3)) %>% 
#   dplyr::mutate(mean_scale = scale(mean),
#                 sd = scale(sd))
# 
# ggplot(data = themisto_summ, aes(x=YEAR, y = mean_scale)) +
#   geom_point()+
#   geom_line() +
#   geom_errorbar(aes(ymin = mean_scale-sd, ymax = mean_scale+sd))
# 
# ### Spatial data distribution ============
# zoop_multiple_space <- large_zoop %>% 
#   filter(!YEAR <1999) %>% 
#   group_by(YEAR, LAT, LON) %>%
#   dplyr::summarise(mean = mean(EST_NUM_PERM3),
#                    sd = sd(EST_NUM_PERM3))  
# 
# ggplot(data = zoop_multiple_space, 
#        aes(x=LON, y = LAT,
#           fill = log(mean), color = log(mean))) +
#   geom_point() +
#   facet_wrap(~YEAR)
# 
# ### Temporal data distribution ============
# zoop_multiple_space <- large_zoop %>% 
#   filter(!YEAR <1999) %>% 
#   group_by(YEAR, LAT, LON) %>%
#   dplyr::summarise(mean = mean(EST_NUM_PERM3),
#                    sd = sd(EST_NUM_PERM3))  
# 
# hist(as.numeric(zoop_multiple_space$MONTH))
# 
# ggplot(data = zoop_multiple_space, aes(x=LON, y = LAT,
#                                        fill = log(mean), color = log(mean))) +
#   geom_point() +
#   facet_wrap(~YEAR)
