library(here)
library(tidyverse)

# load 2018 lipid data 
lipid <- read.csv("data/2018 Copepod data QA CP.csv") %>%
  dplyr::select(Latitude, Longitude, estimated...lipid..dry.weight) %>%
  dplyr::rename("proportion_lipid_dw" = estimated...lipid..dry.weight) %>% 
  filter(!is.na(proportion_lipid_dw))
 
#How variable is lipid data? 
mean_lipid <- mean(log(lipid$proportion_lipid_dw))
sd_lipid <- sd(log(lipid$proportion_lipid_dw))

p <- ggplot(data = lipid) + 
  geom_histogram(aes(x=log(proportion_lipid_dw)), color = "black") + 
            geom_vline(xintercept = mean_lipid)
p           
 cal_lipid_draw <- data.frame(x= rnorm(n=49, mean = mean_lipid, sd = sd_lipid))
  
 p +
   geom_histogram(data = cal_lipid_draw,
                  aes(x=x), alpha = 0.5, color = "red") +
   geom_vline(xintercept = mean(cal_lipid_draw$x), color = "red")
 
 mean(cal_lipid_draw$x)
 sd(cal_lipid_draw$x)
 
#load zoop data
zoop <- read_csv("data/Species_Processed_Final.csv") %>% 
  filter(TAXA_COARSE == "Calanus marshallae/glacialis", 
         YEAR == 2018) 


# convert WW to DW, then I can scale by lipid content. 
 









