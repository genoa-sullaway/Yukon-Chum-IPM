# Start ==================================================================================================
#Project Name: AYK Chum Salmon Model
#Creator: Genoa Sullaway
#Date: Start May 2023
#
# Purpose: Compare predicted and observed from Kuskokwim Run Reconstruction model - output, optim is from Optimization_Kusko_reconstruction.R
# Load Packages =========================================================================================
library(tidyverse)
library(here)
 
### Load Bue data
## NOTE: genoa looked at figure 8 in bue and Molyneaux 2008 and guess-timated the estimated #'s per year because the paper doesnt provide a table with exact points 
#bue_estimated <- read_csv("data/Kusko_Reconstruction/Bue_Reconstruction_Dat.csv") 
bue_estimated <- read_csv("data/Processed_Data/OLD/Estimated_N_OldModel_XLS.csv") %>% # this is from the older excel sheet, columns Q,R,FW 
  filter(param == "N") %>% 
  dplyr::mutate(Year = as.numeric(year_or_project),
                Estimate_Thousands = value/1000) %>%
  dplyr::select(c(4:5))

estimated_parameters<- readRDS("output/OLD_optim_output_par.RDS")  
#assign weights 
names(estimated_parameters) <- par_names

# I think to plot I need to recreate the NLL function without the LL components to then get pred N and compare it to obs N?
 # NLL Function =========================================================================================
predict_NLL <- function(par,
                data,
                weeks,
                projects,
                Nyear, 
                weights){ 
   
  # Extract parameters and data: ============================================================================
  
  # grep("ln_q_vec", par_names)
  q_vec <- par[1] 
  
  #  grep("pred_N", par_names)
  pred_N <- par[2:21]
  
  #  grep("ln_pred_slope", par_names)  
  pred_slope <- par[22:28]
   
  # Extract Data: ============================================================================
  
  B_yj=as.matrix(data$B_yj)
  obs_catch_week=as.matrix(data$obs_catch_week)
  #obs_N=as.matrix(data$obs_N)
  obs_escape_project = as.matrix(data$obs_escape_project)
  obs_subsistence = as.matrix(data$obs_subsistence)
  obs_commercial = as.matrix(data$obs_commercial)
  
  # Predict N - Observed Total Return =========================================================================================
  #pred_N = pred_E + rowSums(pred_catch) 
  
  # N_yi = number of chum present in commercial district by week/year (Eq 4)
  #summing across weeks for Nyi is supposed to give Ny, total fish present across years 
  N_yi <- matrix(nrow = Nyear, ncol = weeks)
  for (j in 1:Nyear) {
    N_yi[j,] =  as.matrix(pred_N[j]*prop[j,])
  }
  
  # Predict C - Catch, using Baranov catch equation: ============================================================================
  
  # N is # of fish (week by year)
  # B is effort (week by year)
  # for right now estimate 1 q for whole data set
  #pred_catch = N_yi_vec_prop*(1-(exp(-q_vec*B_yj_vec))) 
  
  pred_catch <- matrix(ncol = weeks, nrow = Nyear)
  
  for (i in 1:weeks) {
    for (j in 1:Nyear) { 
      pred_catch[j,i] = N_yi[j,i]*(1-(exp(-q_vec*B_yj[j,i])))
    }
  }
   
  # Predict E - Escapement =========================================================================================
  #Eq 1 expands the data and yield "observed escapement"
  # this is equation 1 (trying to code it exactly as it is even though it seems super weird...)
  obs_e_week <-matrix(NA, ncol = projects, nrow = Nyear)  
  
  for (p in 1:projects) {
    for (j in 1:Nyear) {
      obs_e_week[j,p] = pred_slope[p]*obs_escape_project[j,p]
    }
  }
  obs_escape<- rowSums(obs_e_week)
  
  #equation 6 in Bue paper 
  #obs_N = as.matrix(obs_escape + obs_subsistence + obs_commercial)# + inriver[2] ) # +catch[,4] + catch[,5]) # on Page 5 of model paper this is N_y, in excel this is "# of fish accounted for"
  
  # equation 2 yields predicted escapement 
  pred_E = pred_N - obs_subsistence - obs_commercial
  
  output <- list(pred_catch, pred_E, pred_N,obs_e_week,N_yi)
  # Return the predicted values based on parameter estimates in optimization script 
  return(output)
}

# List input data  ===================================================================
data <- list(B_yj=B_yj, 
                     obs_catch_week=obs_catch_week,
                     #obs_N=obs_N,
                     obs_escape_project=obs_escape_project,
                     obs_commercial = obs_commercial,
                     obs_subsistence=obs_subsistence)
 
# Run function  ======================================================================
pre_outputs <- predict_NLL(par=estimated_parameters, # starting values for parameter estimations 
                           # data/fixed values go below
                          data = data,
                           #values
                           weeks=weeks,
                           projects=projects,
                           Nyear=Nyear,
                           weights = weights)
  
 
#   catch 2 data sources  ======================================================================
# 
# obs_com_subs_catch<-read_csv("data/Processed_Data/OLD/OLD_catch.csv") %>%  # from bethel csv
#   dplyr::mutate(com_subs = Commercial,# +Subsistence, 
#          year = Year) %>%
#   left_join(obs_catch %>% 
#               group_by(year) %>%
#               dplyr::summarise(bethelobs_catch=sum(obs_catch))) %>%
#   select(year, com_subs,bethelobs_catch) %>%
#   gather(2:3,  key = "id", value = "value")
# 
#  
 
# Plot predicted catch  ======================================================================
obs_catch <- read_csv("data/Processed_Data/OLD/OLD_catch_week.csv") %>%  #Taken from Chum RR data.xlsx
  dplyr::mutate(year = as.numeric(1976:2011)) %>%
  select(-`9/2-9/8`) %>% 
  gather(1:12, key = "week", value = "obs_catch") %>%
  filter(!year < 1988 & !year >2007)


  # prop<-read_csv("data/Processed_Data/Prop_V2.csv") %>%
  # filter(year < 2008 & year >1987) %>%
  # dplyr::select(-year)
prop<- read_csv("data/Processed_Data/OLD/OLD_Proportions_run_present_weekly.csv") %>% # only select some weeks for now because proportion has less weeks than the effort data...
  mutate(year = 1976:(1976+nrow(.)-1)) %>%
  filter(year < 2008 & year >1987) %>%
  dplyr::select(-year) %>%
  dplyr::select(c(3:14)) 

weeks_label<-colnames(prop)
  # load pred catch from Bues estiamtes (csv sheet old) to also compare to obs and mine
  #need to tidy them, done below. 
Bue_pred <- read_csv("data/Processed_Data/OLD/Bue_pred_catch.csv") 
 
col_odd <- seq_len(ncol(Bue_pred)) %% 2

bue_pred_catch<- data.frame(Bue_pred[ , col_odd == 0])
bue_pred_catch <- bue_pred_catch[1:12]
names(bue_pred_catch) <- weeks_label 

bue_pred_catch_df <- bue_pred_catch %>%
  slice(-1) %>%
  mutate(year =1976:2011) %>% 
  filter(year>1987 & year<2008) %>% 
  gather(1:12, key = "week", value = "bue_pred_catch")  %>%
  mutate(bue_pred_catch = as.numeric(bue_pred_catch),
         year = as.numeric(year))

######## now predicted catch from my model
pred_catch<-pre_outputs[[1]]
 
pred_catch <- pred_catch %>% 
  data.frame()  

names(pred_catch) <- weeks_label

pred_catch <- pred_catch %>%
  mutate(year = 1988:2007) %>%
  gather(1:12, key = "week", value = "pred_catch")  

join <- left_join(obs_catch,pred_catch) %>%
  left_join(bue_pred_catch_df) %>% 
  gather(3:5, key = "id", value = "catch")

ggplot(data = join %>% filter(!id=="bue_pred_catch"), aes(x=week, y=catch/1000, group = id, color = id)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  facet_wrap(~year,scales="free")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

pred_catch_bue_obs<- ggplot(data = join %>% filter(!id=="pred_catch"), aes(x=year, y=catch/1000, group = id, color = id)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  facet_wrap(~week,scales="free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(caption = "Observed catch vs Bue predicted catch")

pred_catch_obs_gs <- ggplot(data = join %>% filter(!id=="bue_pred_catch"), aes(x=year, y=catch/1000, group = id, color = id)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  facet_wrap(~week,scales="free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(caption = "Observed catch vs GS predicted catch")

#add bues in compare to mine 
pred_catch_gs_bue<-ggplot(data = join%>% filter(!id=="obs_catch"), aes(x=year, y=catch/1000, group = id, color = id)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  facet_wrap(~week,scales="free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(caption = "Bue predicted catch vs GS predicted catch")

pdf("output/predicted_catch_plots.pdf")
pred_catch_gs_bue
pred_catch_obs_gs
pred_catch_bue_obs
 
dev.off()

# Plot predicted number per projects ======================================================================

pred_escape <-pre_outputs[[2]]
  
# 
# escape %>%
#   gather(2:3, key = "id", value = "value")
# 
# #add bues in compare to mine 
# ggplot(data = escape, aes(x=year, y=value, group = id, color = id)) +
#   geom_point() +
#   geom_line() +
#   theme_classic() +
#   facet_wrap(~week,scales="free") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Plot predicted per project =============================================
#from my model:
proj_names<-colnames(read_csv("data/Processed_Data/OLD/OLD_kusko_escapement.csv"))[2:8]
expanded_per_proj <-data.frame(pre_outputs[[4]]) %>%
  mutate(year = 1988:2007 )  
  
names(expanded_per_proj)[1:7] <- proj_names
  
expanded_per_proj<- expanded_per_proj %>%
  gather(1:projects, key = "project", value = "value") %>%
  dplyr::mutate(id = "expanded_N_GS_mod")

# Bue's OBS values:
 obs_escape_per_proj<-  read_csv("data/Processed_Data/OLD/OLD_kusko_escapement.csv") %>%
   filter(year < 2008 & year >1987) %>%
   gather(2:(projects+1), key = "project", value = "value")%>%
   dplyr::mutate(id = "obs_escape_per_proj")

# Bues predcited from excel
 bue_pred_per_proj <- read_csv("data/Processed_Data/OLD/Bue_pred_escapement.csv")%>%
   gather(2:(projects+1), key = "project", value = "value") %>%
   rename(year = "Year") %>%
   dplyr::mutate(id = "bue_pred_per_proj")
 
per_proj <- rbind(obs_escape_per_proj,bue_pred_per_proj,expanded_per_proj) 
per_proj[per_proj == 0] <- NA
  
#add bue's in compare to mine 
ggplot(data = per_proj %>% filter(!id == "obs_escape_per_proj"), 
       aes(x=year, y=value, group = id, color = id)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  facet_wrap(~project,scales="free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# same as above but sum across projects to just get total index: 
pred_e<-data.frame(pre_outputs[[2]]) %>%
  rename(value = "Subsistence") %>%
  mutate(year = 1988:2007,
         id = "predicted_escapement")

per_proj[is.na(per_proj)]<- 0

totol_E_df<- per_proj %>% 
  group_by(year, id) %>% 
  summarise(value = sum(value))%>%
  rbind(pred_e)



#add bue's in compare to mine 
ggplot(data = totol_E_df %>% filter(!id == "obs_escape_per_proj", year>1987, !year >2007), 
       aes(x=year, y=value, group = id, color = id)) +
  geom_point() +
  geom_line() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
