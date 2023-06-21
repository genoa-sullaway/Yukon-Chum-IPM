# Plot figure 8 from Bue and Molyneaux report based on estimated N from the excel sheet provided

# this is just me guestimating from figure 8: bue_estimated <- read_csv("data/Kusko_Reconstruction/Bue_Reconstruction_Dat.csv") 
estimated_mod <- read_csv("data/Processed_Data/OLD/Estimated_N_OldModel_XLS.csv") # this is from the older excel sheet, columns Q,R,FW 

ggplot(data = estimated_mod %>% 
         filter(param == "N") %>% 
         dplyr::mutate(year_or_project = as.numeric(year_or_project)), 
       aes(x=year_or_project, y = value/1000))+
  geom_line()+
  geom_point() + 
  theme_classic() +
  scale_y_continuous(limits = c(0,3500))
