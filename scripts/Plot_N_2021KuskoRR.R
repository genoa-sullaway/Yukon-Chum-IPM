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
 
N_plot<- ggplot(data = pred_N, aes(x=Year, y = pred_N_est/1000)) +
  geom_vline(xintercept = 2019, linetype = 2, color = "#a0025c", size =1.5)  +
  geom_line(size=1) +
  geom_point(size=2) + 
  theme_classic() +
  ylab("Total Run (thousands of fish)") +
  theme_ipsum()+
  ggtitle("Kuskokwim River Run Reconstruction") + 
  theme(axis.title.x = element_text(hjust = 0.5, size = 15), 
        axis.title.y = element_text(hjust = 0.5, size = 15),
        axis.text.x = element_text( size = 12),
        axis.text.y = element_text( size = 12),
        plot.title = element_text(size=15),
        plot.background = element_rect(colour = "black", fill=NA, size=1)
        #panel.background = element_rect(colour = "black", size=1)
        )  

png("output/Kusko_RR_N_2021.png", width = 1200, height = 700)
N_plot
dev.off()

