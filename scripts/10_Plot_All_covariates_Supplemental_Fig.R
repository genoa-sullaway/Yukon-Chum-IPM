library(tidyverse)
library(here)
unique(cov_a$Recruit_age_1_millions)
# plot all covariates for supplemental figure 
# load data =======
cov_a <- read_csv("data/processed_covariates/stage_a_all.csv") %>%
  dplyr::mutate( pollock_recruit_millions = as.numeric(scale(Recruit_age_1_millions)), 
                  SST_CDD_NBS = as.numeric(scale(SST_CDD_NBS)),
                  yukon_mean_discharge = as.numeric(scale(yukon_mean_discharge)) 
                 ) %>%
  # zoop are already mean scaled
  dplyr::rename(cal_year = Year) %>% 
  dplyr::mutate(brood_year = cal_year-1) %>%  
  dplyr::select(cal_year,brood_year,SST_CDD_NBS,pollock_recruit_millions,mean_size,
                yukon_mean_discharge) 

# Plot JUST covariates ==============
## A SST ========= 
sst_cov_plot <- cov_a %>%
  gather(3:ncol(.), key = "variable", value = "value") %>%
  filter(cal_year >1999) %>%
  dplyr::mutate(variable = factor(variable, levels = c("mean_size","yukon_mean_discharge",
                                  "SST_CDD_NBS","pollock_recruit_millions")))

labels = c("mean_size"="Spawner Size" ,
           "pollock_recruit_millions"="Pollock Recruitment",
           "SST_CDD_NBS" = "CDD NBS",
          "yukon_mean_discharge"= "Yukon River Flow")

cov_a_plots <- ggplot(data = sst_cov_plot,
                aes(x=cal_year, y = value, group = variable, color = variable)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values= c("#5d74a5", "#b0cbe7", "#ba8e23", "#eba07e", "#a45851")) +
  theme_classic() +
  facet_wrap(~variable, scales = "free", labeller = as_labeller(labels)) +
  ylab("Mean Covariate Trend") + 
  xlab("Calendar Year") +
  geom_hline(yintercept =0, linetype =2, color = "black") + 
  theme(panel.background = element_blank(),  
        plot.background = element_blank(),  
        legend.background = element_blank(),
        legend.position = "none", 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,color = "black")  ) 
cov_a_plots

# ggsave("output/SST_plot_Values.png", width = 4, height = 2, bg = "transparent")
 
## B ============== 
cov_b <- read_csv("data/processed_covariates/stage_b_all.csv") %>%
  dplyr::mutate(SST_CDD_Aleut = as.numeric(scale(SST_CDD_Aleut)),
                Chum_hatchery= as.numeric(scale(Chum_hatchery)), 
                Pink_hatchery= as.numeric(scale(Pink_hatchery)), 
                full_index = case_when(Year %in% c(2002,2008, 2014, 2020,2022) ~ NA,
                                       TRUE ~ full_index),
                full_index = as.numeric(scale(full_index))) %>% 
  dplyr::rename(cal_year = Year) %>%
  dplyr::select(cal_year,SST_CDD_Aleut,
                Chum_hatchery,
                Pink_hatchery,
                full_index) %>%  
  gather(2:ncol(.), key = "variable", value = "value") %>%
  filter(!cal_year<1999) %>%
  dplyr::mutate(variable = factor(variable, levels = c("full_index","SST_CDD_Aleut",
                                                       "Chum_hatchery","Pink_hatchery")),
                variable = case_when(variable == "SST_CDD_Aleut" ~ "Aleutian Winter Temperature",
                                     variable == "Chum_hatchery" ~ "Chum Salmon Hatchery Release Abundance",
                                     variable == "Pink_hatchery" ~ "Pink Salmon Hatchery Release Abundance",
                                     variable == "full_index" ~   "Fullness Index")) 

cov_b_plot<-ggplot(data = cov_b,
                   aes(x=cal_year, y = value, group = variable, color = variable)) +
  geom_hline(yintercept =0, linetype =2, color = "black") +
  geom_line() +
  geom_point() +
  scale_color_manual(values= c("#b0986c", "#dcbe9b", "#72e1e1","#009474")) +
  facet_wrap(~variable ) +
  theme_classic() +
  ylab("Mean Covariate Trend") + 
  xlab("Calendar Year") +
  theme(panel.background = element_blank(),  
        plot.background = element_blank(),  
        legend.background = element_blank(),
        legend.position = "none", 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,color = "black")  ) 

cov_b_plot  
# ggsave("output/significant_B_Covariates.png", width = 4, height = 3, bg = "transparent")

ggpubr::ggarrange(cov_a_plots,cov_b_plot, labels = c("A.", "B."), nrow = 2)

ggsave("output/Supplemental_Plot_Covariates.png",width = 6, height = 6, bg = "white")

