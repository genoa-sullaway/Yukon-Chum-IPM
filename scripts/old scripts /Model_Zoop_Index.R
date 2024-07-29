library(tidyverse)
 
## Model Index  =============================================================
df<-readRDS(here("data","ROMS_EMAcalanus.RDS")) %>%
  dplyr::mutate(day_of_year = yday(ecofoci_date),
                YEAR = factor(YEAR), 
                cubedepth_integrated_biomass = depth_integrated_biomass^(1/3),
                sqrt_depth_integrated_biomass = sqrt(depth_integrated_biomass)) %>% 
  dplyr::filter(!LAT < 60) %>%
  dplyr::filter(!LON < -175) %>%
  filter(!day_of_year<200) 

model <- gam(depth_integrated_biomass ~ s(LAT, LON ) + s(MAX_GEAR_DEPTH) + YEAR + s(day_of_year,k=7, m=1 ),
             family = tw(link = "log"), 
             data = df) 

#gam.check(model)
#summary(model)
#plot.gam(model)
             

## Predict Year  =============================================================
DF1<-expand.grid(YEAR = c(unique(df$YEAR)))

pred_df_year <- cbind(DF1, 
                      data.frame(LAT = rep(round(mean(df$LAT)), times = nrow(DF1)), 
                                 MAX_GEAR_DEPTH = rep(round(mean(df$MAX_GEAR_DEPTH)), times = nrow(DF1)), 
                                 LON = rep(round(mean(df$LON)), times = nrow(DF1)),  
                                 day_of_year = rep(round(mean(df$day_of_year)), times = nrow(DF1))))  

temp <- predict(model, pred_df_year, se.fit = TRUE )

pred_df_year$pred <-  exp(temp[[1]])#^2
pred_df_year$se <-  exp(temp[[2]])#^2

dat_year <- df %>% 
  group_by(YEAR) %>%
  summarise(mean = mean(depth_integrated_biomass),
            se = sd(depth_integrated_biomass)/ sqrt(length(depth_integrated_biomass)))

## Plot year ================================
# pred_plot_YEAR<-
COPEPOD_INDEX_PLOT <- ggplot()+
  geom_bar( data = pred_df_year,aes(x=YEAR, y = pred), stat = "identity")  + 
  geom_errorbar(data = pred_df_year, aes(x=YEAR, ymin=pred-se, ymax = pred+se), 
                alpha = 0.5, width = 0.1) +
  geom_point(data = dat_year, aes(x=YEAR, y=mean), color = "blue",
             alpha = 0.5) +
  geom_errorbar(data = dat_year, aes(x=YEAR, ymin=mean-se, ymax = mean+se),color = "blue", 
                alpha = 0.5,width = 0.1) +
  xlab("Year") +
  ylab("Biomass (mg C m-3)") +
  labs(y = expression(paste("Biomass (mg C ", m^-3, ")"))) + 
  theme_ipsum() + 
  ggtitle("Copepod Biomass Index") + 
  theme(legend.title = element_blank(),
        axis.title.x = element_text(hjust = 0.5, size = 20, vjust = 0.5), 
        axis.title.y = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text( size = 18),
        axis.text.y = element_text( size = 18),
        plot.title = element_text(size=20,hjust = 0.5),
        plot.background = element_rect(colour = "black", fill=NA, size=1))

png("output/Zoop_index_Plot.png", width = 1000, height = 500)
print(COPEPOD_INDEX_PLOT)
dev.off()
















