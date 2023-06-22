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
estimated_parameters<- readRDS("output/OLD_optim_output_par.RDS") 

# Load data (below just for plotting) =========================================================================================
escapement <- read_csv("data/Processed_Data/OLD/OLD_kusko_escapement.csv") %>%
  filter(year < 2008 & year >1987) 
proj_names<-colnames(escapement)[2:8]

# Plot N =========================================================================================
pred_N<-data.frame(Year = c(1988:2007),   
                   pred_N_est= as.vector(c(estimated_parameters[2:21])))  

est_N<- bue_estimated %>% 
  filter(param == "N") %>% 
  dplyr::mutate(Year = as.numeric(year_or_project),
                Bue_Estimate_Thousands = value) %>%
  dplyr::select(c(4:5)) %>% 
  filter(Year < 2008) %>% 
  left_join(pred_N) %>% 
  gather(2:3, key = "id", value = "value")

### Plot predicted N
N_plot<-ggplot(data = est_N,aes(x=Year, y = value, group = id, color = id)) +
  geom_line( ) +
  geom_point() + 
  theme_classic() +
  ylab("Total Run (thousands of fish")  

# Plot Slope =========================================================================================
pred_slope<-data.frame(project = c(proj_names),
                       pred_slope= as.vector(c(estimated_parameters[22:28])))  

est_slope<- bue_estimated %>% 
  filter(param == "Slope") %>% 
  dplyr::mutate(project = word(year_or_project, 1),
                Bue_slope = value) %>%
  dplyr::select(c(4:5)) %>% 
  left_join(pred_slope) %>% 
  gather(2:3, key = "id", value = "value") #%>%
  # mutate(value = case_when(id == "pred_slope" ~ value*4.5,
  #                          TRUE ~ value))

### Plot predicted slope
slope_plot <- ggplot(data = est_slope,aes(x=project, y = value, group = id, fill = id)) +
  geom_bar(stat="identity", position = "dodge") +
  theme_classic() +
  ylab("Slope")  

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


