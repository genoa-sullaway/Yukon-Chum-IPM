# Bring in all covariates into one big dataset and plot all covariates in one plot 
library(tidyverse)
library(here)

# Stage A - Load data ============= 
sst_a <- read_csv("data/processed_covariates/Stage_A_CDD.csv") %>%
  dplyr::mutate(site = "NBS_akfin",
         id="cummulative_degree_days",
         type = "SST_CDD") %>%
  dplyr::rename(value = "CDD",
                Year = "year")
large_zoop_a <- read_csv("data/processed_covariates/covariate_large_zooplankton.csv") %>%
  dplyr::mutate(site = "NBS",
         id="mean_abundance",
         type = "large_zoop") %>%
  dplyr::rename(value = "mean",
         Year = "YEAR") %>%
  dplyr::select(Year, value, site,id,type)
gelatinous_zoop_a <- read_csv("data/processed_covariates/covariate_gelatinous_zooplankton.csv") %>%
  mutate(site = "NBS",
         id="mean_abundance",
         type = "gelatinous_zoop") %>%
  dplyr::rename(value = "mean",
         Year = "YEAR") %>%
  dplyr::select(Year, value, site,id,type)

air_temp_a <- read_csv("data/processed_covariates/Stage_A_airtemp.csv") %>%
  dplyr::mutate(type = case_when(type == "average"  ~ "average_airtemp",
                          type == "min"  ~ "min_airtemp",
                          type == "max"  ~ "max_airtemp")) %>%
  # mutate(type = case_when(type == "average" & site == "aniak" ~ "aniak_average_airtemp",
  #                         type == "min" & site == "aniak" ~ "aniak_min_airtemp",
  #                         type == "max" & site == "aniak" ~ "aniak_max_airtemp",
  #                         type == "average" & site == "chena" ~ "chena_average_airtemp",
  #                         type == "min" & site == "chena" ~ "chena_min_airtemp",
  #                         type == "max" & site == "chena" ~ "chena_max_airtemp")) %>%
  dplyr::select(Year, value, site,id,type)

# Stage A - One DF ============= 
stage_a_cov<- rbind(sst_a,large_zoop_a,gelatinous_zoop_a) #,air_temp_a)

# Stage A - Plot =============
plota <- ggplot(data = stage_a_cov, aes(x=Year, y = value, color = type, group = id)) +
  geom_point( ) +
  geom_line( ) +
  scale_color_manual(guide = "none", values = PNWColors::pnw_palette(name="Starfish",n=3)) + 
  facet_wrap(~type, scales = "free",ncol=1) +
  theme_classic()  
  
plota

plot_airtemp_a <- ggplot(data = air_temp_a,aes(x=Year, y = value, 
                                               color = site, 
                                               group = type, 
                                               linetype = type)) +
  geom_point( ) +
  geom_line( ) +
  theme_classic() +
  facet_grid(id~site, scales = "free") +
  scale_linetype_manual(name = " ", values =c(1,2,2)) + 
  ylab("Temperature F") + 
  theme(legend.position = "bottom") + 
  #theme(legend.position = "none") + 
  scale_color_manual(guide = "none", 
                     values = PNWColors::pnw_palette(name="Bay", n=2))  
  

plot_airtemp_a
 
a<-ggpubr::ggarrange(plota, plot_airtemp_a, ncol = 2)

pdf("output/plot_covariates_a.pdf", width = 12, height = 5)
a
dev.off()

# Stage B - Load data ============= 
hatchery_chum_b<-read_csv("output/hatchery_Chum_Covariate_AKandAsia.csv")
hatchery_pink_b <- read_csv("output/hatchery_Pink_Covariate_AKandAsia.csv")
sst_b<-read_csv("data/processed_covariates/Stage_B_CDD.csv")

# Stage B - Plots ============= 
 


