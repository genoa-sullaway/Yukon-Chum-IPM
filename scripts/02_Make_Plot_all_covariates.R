# Bring in all covariates into one big dataset and plot all covariates in one plot 
library(tidyverse)
library(here)

# years in covariate timeseries to expand the TS that need it 
YEAR <-data.frame(YEAR = seq(from = 2002, to = 2022, by =1))

# Stage A - Load data ============= 
sst_a <- read_csv("data/processed_covariates/Stage_A_CDD.csv") %>%
  dplyr::rename(SST_CDD_NBS = "CDD",
                Year = "year")

# this is a GAM zoop index
zoop <- read_csv("data/processed_covariates/Stage_A_Zooplankton_Index.csv")  %>% 
  rename(Year = "YEAR")  
 
river_discharge_a <- read_csv("data/processed_covariates/Stage_A_YK_Discharge.csv") %>%
  dplyr::select(Year, mean_discharge,id) %>%
  spread(id, mean_discharge) %>%
  rename(kusko_mean_discharge = "Kusko",
         yukon_mean_discharge = "Yukon")
 
air_temp_a <- read_csv("data/processed_covariates/Stage_A_airtemp.csv") %>%
  filter(type =="average", id == "spring") %>%
  dplyr::rename(mean_air_temp = "value") %>%
  dplyr::select(Year, mean_air_temp,site) %>%
  spread(site,mean_air_temp) %>%
  rename(kusko_aniak_mean_airtemp = "aniak",
         yukon_chena_mean_airtemp = "chena")

# Stage A - One DF for model ============= 
stage_a_cov<- left_join(river_discharge_a,sst_a)  %>%
              left_join(air_temp_a) %>%
              left_join(zoop) # %>%
              
# Stage A - Save DF ============= 
write_csv(stage_a_cov, "data/processed_covariates/stage_a_all.csv")

# Stage A - Plot values =============
stage_a_cov_plot<-stage_a_cov %>%
  gather(2:ncol(.), key = "id", value = "value") #%>%
  # dplyr::mutate(id = factor(id, levels = c("kusko_mean_discharge", "yukon_mean_discharge",
  #                                          "kusko_aniak_mean_airtemp", "yukon_chena_mean_airtemp",
  #                                          "SST_CDD_NBS", "gelatinous_zoop_NBS", "large_zoop_NBS")))

plota <- ggplot(data = stage_a_cov_plot %>% filter(!Year<2000), aes(x=Year, y = value, color = id, group = id)) +
  geom_point( ) +
  geom_line( ) +
  scale_color_manual(guide = "none", values = PNWColors::pnw_palette(name="Starfish",n=7)) + 
  facet_wrap(~id, scales = "free",ncol=1) +
  theme_classic()  +
  ggtitle("Stage A Covariates")
  
plota
  
  
pdf("output/plot_covariates_a.pdf", width = 7, height = 12)
plota
dev.off()

# Stage A - Plot, scaled =============
stage_a_cov_plot_scale<-stage_a_cov_plot %>%
  group_by(id) %>% 
  filter(!Year<2003) %>% 
  mutate(scale = scale(value)) %>% 
  filter(!id%in% c("kusko_mean_discharge",
                   "kusko_aniak_mean_airtemp"))
  
plota <- ggplot(data = stage_a_cov_plot_scale, aes(x=Year, y = scale, color = id, group = id)) +
  geom_point( ) +
  geom_line( ) +
  scale_color_manual(guide = "none", values = PNWColors::pnw_palette(name="Starfish",n=7)) + 
  facet_wrap(~id, scales = "free",ncol=1) +
  theme_classic()  +
  ggtitle("Stage A Covariates - Scaled") +
  geom_hline(yintercept =0, linetype =2)

plota


pdf("output/plot_scaled_covariates_a.pdf", width = 7, height = 12)
plota
dev.off()

# Stage B - Load data ============= 
hatchery_chum_df<-read_csv("data/hatchery_Chum_Covariate_AKandAsia.csv") 
hatchery_chum_b<-hatchery_chum_df %>%
  dplyr::rename(Chum_hatchery="sum") %>%
  dplyr::select(Year, Chum_hatchery) %>%
  rbind(data.frame(Year = c(2023),
                   Chum_hatchery = c(mean(hatchery_chum_df$sum) + 0.01)))

hatchery_pink_df <- read_csv("data/hatchery_Pink_Covariate_AKandAsia.csv") 
hatchery_pink_b <- hatchery_pink_df%>%
  dplyr::rename(Pink_hatchery="sum") %>%
  dplyr::select(Year, Pink_hatchery) %>% 
  rbind(data.frame(Year = c(2023),
                   Pink_hatchery = c(mean(hatchery_pink_df$sum) + 0.01)))

sst_b<-read_csv("data/processed_covariates/Stage_B_CDD.csv") %>%
  dplyr::rename(SST_CDD_Aleut = "CDD",
                Year = "year") %>%
  dplyr::select(Year, SST_CDD_Aleut)
  
river_discharge_b <- read_csv("data/processed_covariates/Stage_B_YK_Discharge.csv") %>%
  dplyr::select(Year, mean_discharge,id) %>%
  spread(id, mean_discharge) %>%
  rename(kusko_mean_discharge_summer = "Kusko",
         yukon_mean_discharge_summer = "Yukon")
  
stage_b_cov<- left_join(river_discharge_b,sst_b)  %>%
  left_join(hatchery_pink_b) %>%
  left_join(hatchery_chum_b) %>%
  dplyr::rename(brood_year = "Year") #%>%
  #dplyr::mutate(index_year_brood_plus1 = brood_year+2)

# Stage B - Save DF ============= 
write_csv(stage_b_cov, "data/processed_covariates/stage_b_all.csv")

# Stage B - Plots ============= 

stage_b_cov_plot<-stage_b_cov %>%
  gather(2:6, key = "id", value = "value") %>%
  filter(!id %in% c( "yukon_mean_discharge_summer",
                     "kusko_mean_discharge_summer"),
         !brood_year <2002)
  # dplyr::mutate(id = factor(id, levels = c("SST_CDD_SEBS", 
  #                                          "Chum_hatchery",
  #                                          "Pink_hatchery",
  #                                          "kusko_mean_discharge_summer", 
  #                                          "yukon_mean_discharge_summer" )))

plotb <- ggplot(data = stage_b_cov_plot, aes(x=brood_year, y = value, color = id, group = id)) +
  geom_point( ) +
  geom_line( ) +
  scale_color_manual(guide = "none", values = PNWColors::pnw_palette(name="Sunset2",n=5)) + 
  facet_wrap(~id, scales = "free",ncol=1) +
  theme_classic()  +
  ylab(" ") + 
  ggtitle("Stage B Covariates")

plotb

pdf("output/plot_covariates_b.pdf", width = 7, height = 12)
plotb
dev.off()



# Old / Plots ===============
# Covariates B - scale ========== 
stage_b_cov_plot <- stage_b_cov_plot %>%
  group_by(id) %>% 
  dplyr::mutate(scale = scale(value))

plotb <- ggplot(data = stage_b_cov_plot, aes(x=Year, y = scale, color = id, group = id)) +
  geom_point( ) +
  geom_line( ) +
  scale_color_manual(guide = "none", values = PNWColors::pnw_palette(name="Sunset2",n=5)) + 
  facet_wrap(~id, scales = "free",ncol=1) +
  theme_classic()  +
  ylab(" ") + 
  ggtitle("Stage B Covariates - Scaled") +
  geom_hline(yintercept = 0, linetype =2)

plotb

pdf("output/plot_Scaled_covariates_b.pdf",  width = 7, height = 12)
plotb
dev.off()

# Stage A: Plot individual covariates ==============================
 ## Discharge ============= 
  dplyr::mutate(id = factor(id, levels = c("kusko_mean_discharge", "yukon_mean_discharge",
                                           "kusko_aniak_mean_airtemp", "yukon_chena_mean_airtemp",
                                           "SST_CDD_NBS", "gelatinous_zoop_NBS", "large_zoop_NBS")))
discharge <- stage_a_cov %>%
  select(Year, kusko_mean_discharge,yukon_mean_discharge) %>%
  gather(2:3, key = "id", value = "value") %>%
  group_by(id) %>%
  mutate(scale = scale(value))

rivera <- ggplot( data = discharge, aes(x=Year, y = scale, group = id, color =id)) +
  geom_point(  ) +
  geom_line( ) + 
  scale_color_manual(name = " ", values = PNWColors::pnw_palette(name="Bay",n=2)) +
  theme_classic()  +
  ggtitle("Stage A- Mean River Discharge") + 
  geom_hline(yintercept =0) +
   ylab(" ") +
  theme(legend.position = "bottom")

pdf("output/plot_Cov_Riverdischarge_A.pdf",  width = 7, height = 4)
rivera
dev.off()

## Airtemp =============  

airtemp <- stage_a_cov %>%
  select(Year, kusko_aniak_mean_airtemp,yukon_chena_mean_airtemp) %>%
  gather(2:3, key = "id", value = "value") %>%
  group_by(id) %>%
  mutate(scale = scale(value))

airtempa <- ggplot( data = airtemp, aes(x=Year, y = scale, group = id, color =id)) +
  geom_point(  ) +
  geom_line( ) + 
  scale_color_manual(name = " ", values = PNWColors::pnw_palette(name="Bay",n=2)) +
  theme_classic()  +
  ggtitle("Stage A- Mean Air Temp") + 
  geom_hline(yintercept =0) +
  ylab(" ") +
  theme(legend.position = "bottom")

pdf("output/plot_Cov_AirTemp_A.pdf",  width = 7, height = 4)
airtempa
dev.off()


## SST NBS =============  

SST_NBS <- stage_a_cov %>%
  select(Year, SST_CDD_NBS) %>%  
  mutate(scale = scale(SST_CDD_NBS))

SST_NBSa <- ggplot( data = SST_NBS, aes(x=Year, y = scale), color = "#4682B4") +
  geom_point( color = "#4682B4" ) +
  geom_line( color = "#4682B4" ) + 
 # scale_color_manual(name = " ", values = PNWColors::pnw_palette(name="Bay",n=2)) +
  theme_classic()  +
  ggtitle("Stage A- NBS SST (cummulative degree days)") + 
  geom_hline(yintercept =0) +
  ylab(" ") #+
  #theme(legend.position = "bottom")

pdf("output/plot_SST_NBSCDD_A.pdf",  width = 7, height = 4)
SST_NBSa
dev.off()

## Large zooplankton =============  

large_zoop <- stage_a_cov %>%
  select(Year, large_zoop_NBS) %>%  
  mutate(scale = scale(large_zoop_NBS))

large_zoopa <- ggplot( data = large_zoop, aes(x=Year, y = scale) ) +
  geom_point( color = "#e1ad01" ) +
  geom_line( color = "#e1ad01" ) + 
  # scale_color_manual(name = " ", values = PNWColors::pnw_palette(name="Bay",n=2)) +
  theme_classic()  +
  ggtitle("Stage A- Large zooplankton NBS") + 
  geom_hline(yintercept =0) +
  ylab(" ")  

pdf("output/plot_largezoop_A.pdf",  width = 7, height = 4)
large_zoopa
dev.off()

## Gelatinous zooplankton =============  
gelatinous_zoop <- stage_a_cov %>%
  select(Year, gelatinous_zoop_NBS) %>%  
  mutate(scale = scale(gelatinous_zoop_NBS))

gelatinous_zoopa <- ggplot( data = gelatinous_zoop, aes(x=Year, y = scale) ) +
  geom_point( color = "#301934" ) +
  geom_line( color = "#301934" ) + 
  # scale_color_manual(name = " ", values = PNWColors::pnw_palette(name="Bay",n=2)) +
  theme_classic()  +
  ggtitle("Stage A- Gelatinous zooplankton NBS") + 
  geom_hline(yintercept =0) +
  ylab(" ")  

pdf("output/plot_gelatinouszoop_A.pdf",  width = 7, height = 4)
gelatinous_zoopa
dev.off()


# Stage B: Plot individual covariates ==============================
## SST ============= 
# c("SST_CDD_SEBS", 
#   "Chum_hatchery",
#   "Pink_hatchery",
#   "kusko_mean_discharge_summer", 
#   "yukon_mean_discharge_summer" )))

SST_SEBS <- stage_b_cov %>%
  select(Year, SST_CDD_SEBS)  %>%
  mutate(scale = scale(SST_CDD_SEBS))

sstb <- ggplot( data = SST_SEBS, aes(x=Year, y = scale )) +
  geom_point(color = "#4B5320"  ) +
  geom_line(color = "#4B5320" ) + 
  scale_color_manual(name = " ", values = PNWColors::pnw_palette(name="Bay",n=2)) +
  theme_classic()  +
  ggtitle("Stage B- SST SEBS (CDD January-June)") + 
  geom_hline(yintercept =0) +
  ylab(" ") +
  theme(legend.position = "bottom")

pdf("output/plot_SST_CDD_SEBS_B.pdf",  width = 7, height = 4)
sstb
dev.off()

## discharge ============= 
# c("SST_CDD_SEBS", 
#   "Chum_hatchery",
#   "Pink_hatchery",
#   "kusko_mean_discharge_summer", 
#   "yukon_mean_discharge_summer" )))
discharge <- stage_b_cov %>%
  select(Year, kusko_mean_discharge_summer,yukon_mean_discharge_summer) %>%
  gather(2:3, key = "id", value = "value") %>%
  group_by(id) %>%
  mutate(scale = scale(value))

riverb <- ggplot( data = discharge, aes(x=Year, y = scale, group = id, color =id)) +
  geom_point(  ) +
  geom_line( ) + 
  scale_color_manual(name = " ", values = PNWColors::pnw_palette(name="Bay",n=2)) +
  theme_classic()  +
  ggtitle("Stage B - Mean River Discharge, Summer") + 
  geom_hline(yintercept =0) +
  ylab(" ") +
  theme(legend.position = "bottom")

pdf("output/plot_Cov_Riverdischarge_B.pdf",  width = 7, height = 4)
riverb
dev.off()


## hatchery ============= 
# c("SST_CDD_SEBS", 
#   "Chum_hatchery",
#   "Pink_hatchery",
#   "kusko_mean_discharge_summer", 
#   "yukon_mean_discharge_summer" )))
hatchery <- stage_b_cov %>%
  select(Year, Pink_hatchery,Chum_hatchery) %>%
  gather(2:3, key = "id", value = "value") %>%
  group_by(id) %>%
  mutate(scale = scale(value))

hatcheryb <- ggplot( data = hatchery, aes(x=Year, y = scale, group = id, color =id)) +
  geom_point(  ) +
  geom_line( ) + 
  scale_color_manual(name = " ", values = PNWColors::pnw_palette(name="Sunset2",n=2)) +
  theme_classic()  +
  ggtitle("Stage B - Hatchery releases") + 
  geom_hline(yintercept =0) +
  ylab(" ") +
  theme(legend.position = "bottom")

pdf("output/plot_Cov_hatchery_B.pdf",  width = 7, height = 4)
hatcheryb
dev.off()

