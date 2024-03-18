
yukon_fall<- read_csv("data/Yukon_Escapement_ADFG/Yukon_Fall_Chum_RR_JTC.csv")  %>%
  rename(cal_year = "Year") %>% 
  dplyr::select(cal_year, Spawners) %>%
  dplyr::mutate(id = "Fall")

yukon_summer <- read_excel("data/Yukon_Escapement_ADFG/S Chum RR 2023.xlsx", sheet = 2) %>%
  dplyr::select(1,2) %>% 
  janitor::row_to_names(row_number = 1) %>%
  dplyr::rename(cal_year = "Year",
                Spawners = "Escapement")  %>%
  dplyr::mutate(id = "Summer") 

yukon_spawners = rbind(yukon_fall, yukon_summer) %>% 
  dplyr::mutate(Spawners = as.numeric(Spawners),
                cal_year=as.numeric(cal_year),
                id = factor(id, levels = c("Summer", "Fall"))) 
 
a<-ggplot(data = yukon_spawners,aes(x=cal_year, y =Spawners, group =id, color =id)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  scale_color_manual(name = "Yukon River Chum Runs", values = c("#556B2F","#D2B48C")) + #"#FFC107")) + #PNWColors::pnw_palette("Starfish", n=2)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="top") +
  xlab("Return Year") +
  ylab("Estimated Spawner\nAbundance")

ggsave("output/YukonSpawnerPlot.jpg", width = 8, height = 6)


juv <- read_csv("data/Juv_Index_CC_aug2023/Index2.csv") %>%
  dplyr::select(Time, Estimate) %>%
  rename(Year = "Time") 

b<-ggplot(data = juv,aes(x=Year, y =Estimate)) +
  geom_line(color = "#AA4A44") +
  geom_point(color = "#AA4A44") +
  theme_classic() +
 # scale_color_manual(name = "Western Alaska Chum Index", values = c("#1E88E5","#ECB956")) + #"#FFC107")) + #PNWColors::pnw_palette("Starfish", n=2)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_text(vjust=-0.8))+
  xlab("Brood Year") +
  ylab("W. AK Chum\nEstimated Juv\nAbundance (Age1)")


ggpubr::ggarrange(b,a,nrow=2, labels = c("a.", "b."))

ggsave("output/YukonChumPlot.jpg", width = 9, height = 4)

