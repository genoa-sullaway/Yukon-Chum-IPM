# Start ==================================================================================================
# Project Name: AYK Chum Salmon Model
# Creator: Genoa Sullaway
# Date: Start June 2023
#
# Purpose: Compare estimated params from Bue excerl model to my R model
# Currently organized for the 1988-2007 version. 

# Load Packages =========================================================================================
library(tidyverse)
library(here)
library(hrbrthemes)
 
# Load data =========================================================================================
# Yukon Fall: Table 14 in ADFG Yukon river report 2022
yukon_fall<- read_csv("data/Yukon_Escapement_ADFG/Yukon_Fall_Chum_RR_JTC.csv") #%>%
  # filter(!is.na(Brood_Year_Return)) %>%
  # dplyr::mutate(Year = `Brood Year`+4)
 
# Kusko
estimated_parameters<- readRDS("output/optim_output_par_data2021.RDS") 
# estimated with dnorm
#estimated_parameters<- readRDS("output/OLD_optim_output_pardnorm.RDS")  

upper_year = 2022

# Load data (below just for plotting) =========================================================================================
escapement <- read_csv("data/Processed_Data/kusko_escapement.csv")  %>% 
  filter(!year < 1988) 
proj_names<-colnames(escapement)[1:9] 

# Plot N =========================================================================================
pred_N<-data.frame(Year = c(1988:(upper_year-1)),   
                   pred_N_est= as.vector(c(estimated_parameters[2:35])))  
 
Nplota<- ggplot(data = pred_N, aes(x=Year, y = pred_N_est/1000)) +
  geom_vline(xintercept = 2019, linetype = 2, color = "#a0025c", size =1.5)  +
  geom_line(size=1) +
  geom_point(size=2) + 
  theme_classic() +
  ylab("Total Run (thousands of fish)") +
  xlab("Return Year") +
  theme_ipsum()+
  ggtitle("Kuskokwim River Chum\nEstimated Run Size") + 
  theme(axis.title.x = element_text(hjust = 0.5, size = 20), 
        axis.title.y = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text( size = 18),
        axis.text.y = element_text( size = 18),
        plot.title = element_text(size=20,,hjust = 0.5),
        plot.background = element_rect(colour = "black", fill=NA, size=1))  
 
 Nplotb<-ggplot(data = yukon_fall, aes(x=Year, y = Estimated_Run/1000)) +
  geom_vline(xintercept = 2020, linetype = 2, color = "#a0025c", size =1.5)  +
  geom_line(size=1) +
  geom_point(size=2) + 
  theme_classic() +
  xlab("Return Year") +
  ylab("Total Run (thousands of fish)") +
  theme_ipsum()+
  ggtitle("Yukon River Fall Chum\nEstimated Run Size") + 
  theme(axis.title.x = element_text(hjust = 0.5, size = 20), 
        axis.title.y = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text( size = 18),
        axis.text.y = element_text( size = 18),
        plot.title = element_text(size=20,hjust = 0.5),
        plot.background = element_rect(colour = "black", fill=NA, size=1))

png("output/Kusko_Yukon_RR.png", width = 1400, height = 700)
ggpubr::ggarrange(Nplota,Nplotb, labels = c("a.", "b."), font.label=list(color="black",size=18))
dev.off()

