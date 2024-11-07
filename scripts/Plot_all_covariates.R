library(tidyverse)
library(here)

stage_a_cov <- read_csv("data/processed_covariates/stage_a_all.csv") %>%
  dplyr::rename(cal_year = Year) %>% 
  dplyr::mutate(brood_year = cal_year-1) %>% 
  filter(brood_year >= year_min, 
         brood_year <= year_max_brood) %>%
  dplyr::mutate(SST_CDD_NBS = as.numeric(scale(SST_CDD_NBS)), 
                yukon_mean_discharge=as.numeric(scale(yukon_mean_discharge)),
                pollock_recruit_scale  =as.numeric(scale(Recruit_age_1_millions))) %>%
  dplyr::select(brood_year,
                SST_CDD_NBS, 
                yukon_mean_discharge,
                pollock_recruit_scale,
                mean_size) %>%   
  gather(2:ncol(.), key = "id", value = "value") 


plota <- ggplot(data = stage_a_cov, 
                aes(x=brood_year, y = value, color = id, group = id)) +
  geom_point( ) +
  geom_line( ) +
  scale_color_manual(guide = "none", values = PNWColors::pnw_palette(name="Starfish",n=8)) + 
  # facet_wrap(~id, scales = "free",ncol=1) +
  theme_classic()  +
  ggtitle("Stage A Covariates") +
  geom_hline(yintercept =0, linetype =2)

plota 

pdf("output/plot_covariates_a.pdf", width = 7, height = 12)
plota
dev.off()


# plot stage B ==========
stage_b_cov <- read_csv("data/processed_covariates/stage_b_all.csv") %>%
  dplyr::rename(cal_year = Year) %>% 
  dplyr::mutate(brood_year = cal_year-2) %>% 
  filter(brood_year >= year_min, 
         brood_year <= year_max_brood) %>% 
  dplyr::mutate( SST_CDD_Aleut = as.numeric(scale(SST_CDD_Aleut)),
                 Chum_hatchery= as.numeric(scale(Chum_hatchery)),
                 Pink_hatchery= as.numeric(scale(Pink_hatchery)),
                 full_index = as.numeric(scale(full_index))) %>% 
  dplyr::select(brood_year,
                SST_CDD_Aleut,
                Chum_hatchery,
                Pink_hatchery,
                full_index) %>%
  gather(2:ncol(.), key = "id", value = "value") 


plotb <- ggplot(data = stage_b_cov, 
                aes(x=brood_year, y = value, color = id, group = id)) +
  geom_point( ) +
  geom_line( ) +
  scale_color_manual(guide = "none", values = PNWColors::pnw_palette(name="Starfish",n=8)) + 
  theme_classic()  +
  ggtitle("Stage B Covariates") +
  geom_hline(yintercept =0, linetype =2)

plotb

pdf("output/plot_covariates_b.pdf", width = 7, height = 12)
plotb
dev.off()




