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

# Load data =========================================================================================
bue_estimated <- read_csv("data/Processed_Data/OLD/Estimated_N_OldModel_XLS.csv") # this is from the older excel sheet, columns Q,R,FW  
#SSQ
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

bueest_N<- bue_estimated %>% 
  filter(param == "N") %>% 
  dplyr::mutate(Year = as.numeric(year_or_project),
                Bue_Estimate_Thousands = value ) %>%
  dplyr::select(c(4:5)) %>% 
  filter(Year < upper_year) 


est_N <- pred_N %>%
   left_join(bueest_N) %>% 
  gather(2:3, key = "id", value = "value")

N_plot<-ggplot(data = est_N, aes(x=Year, y = value/1000, group = id, color = id)) +
  geom_line( ) +
  geom_point() + 
  theme_classic() +
  ylab("Total Run (thousands of fish") 
N_plot

# plot observed vs my predictions 
escapement_sum <-  read_csv("data/Processed_Data/OLD/OLD_kusko_escapement.csv") %>%
  filter(year < upper_year & year >1987) %>%
  gather(2:8, key = "project", value = "value")  %>% 
  group_by(year) %>%
  dplyr::summarise(value = sum(value)) %>% 
  rename(Year = "year") %>%
  dplyr::mutate(id = "obs_E_sum") %>%
  rbind(est_N) %>%
  filter(id %in% c("obs_E_sum", "pred_N_est"))

N_plot_observed<-ggplot(data = escapement_sum,aes(x=Year, y = value/1000, group = id, color = id)) +
  geom_line( ) +
  geom_point() + 
  theme_classic() +
  ylab("Total Run (thousands of fish") 
N_plot_observed

# Plot Slope =========================================================================================
pred_slope<-data.frame(project = c(proj_names),
                       pred_slope= as.vector(c(estimated_parameters[36:44])))  

estbue_slope<- bue_estimated %>% 
  filter(param == "Slope") %>% 
  dplyr::mutate(project = word(year_or_project, 1),
                Bue_slope = value) %>%
  dplyr::select(c(4:5))  %>%
  mutate(project = case_when(project == "Aniak" ~ "Sonar",
                             project == "Tatlawiksuk" ~ "Tatlawitsak",
                             TRUE ~ project))

est_slope <- pred_slope %>%
    left_join(estbue_slope) %>% 
  gather(2:3, key = "id", value = "value") #%>%
  # mutate(value = case_when(id == "pred_slope" ~ value*4.5,
  #                          TRUE ~ value))

### Plot predicted slope
slope_plot <- ggplot(data = est_slope,aes(x=project, y = value, group = id, fill = id)) +
  geom_bar(stat="identity", position = "dodge") +
  theme_classic() +
  ylab("Slope")  +
  labs(caption = "Pred Slope is from GS reconstruction model") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

### Plot predicted slope
slope_plot2 <- ggplot(data = est_slope,aes(x=project, y = value, group = id, fill = id)) +
  geom_bar(stat="identity", position = "dodge") +
  theme_classic() +
  ylab("Slope")  +
  facet_wrap(~id)

# Plot Q =========================================================================================
pred_Q<-data.frame(pred_Q= as.vector(c(estimated_parameters[1])))  

est_Q<- bue_estimated %>% 
  filter(param == "Q") %>% 
  dplyr::mutate(Bue_Q = value) %>%
  dplyr::select(c(4)) %>% 
  cbind(pred_Q) %>% 
  gather(1:2, key = "id", value = "value")

### Plot predicted slope
q_plot<-ggplot(data = est_Q,aes(x=id, y = value, group = id, fill = id)) +
  geom_bar(stat="identity", position = "dodge") +
  theme_classic() +
  ylab("Q")  
 
slope_plot
slope_plot2
q_plot
N_plot

pdf("output/Kusko_RR_Plots.pdf")
slope_plot
q_plot
N_plot
dev.off()

