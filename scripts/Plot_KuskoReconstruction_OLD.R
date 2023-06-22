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
  ln_q_vec <- par[1] 
  
  #  grep("pred_N", par_names)
  ln_pred_N <- par[2:21]
  
  #  grep("ln_pred_slope", par_names)  
  ln_pred_slope <- par[22:28]
  
  q_vec <- ln_q_vec
  pred_N <-ln_pred_N
  pred_slope <- ln_pred_slope

  # Extract Data: ============================================================================
  
  B_yj=as.matrix(data$B_yj)
  obs_catch_week=as.matrix(data$obs_catch_week)
  obs_N=as.matrix(data$obs_N)
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
  
  # equation 2 yields predicted escapement 
  pred_E = pred_N - obs_subsistence - obs_commercial
  
  output <- list(pred_catch, pred_E, pred_N)
  # Return the predicted values based on parameter estimates in optimization script 
  return(output)
}

# List input data  ===================================================================
data <- list(B_yj=B_yj, 
                     obs_catch_week=obs_catch_week,
                     obs_N=obs_N,
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


pred_catch<-pre_outputs[[1]]
pred_escape_pj<-pre_outputs[[2]]

pred_N<-data.frame(Year = c(1988:2007), 
                   Pred_N_fxn = c(pre_outputs[[3]]), 
                   pred_N_est= as.vector(c(estimated_parameters[2:21])))  
  
### Plot predicted N
ggplot(data = pred_N,aes(x=Year, y = Pred_N )) +
  geom_bar(stat= "identity") +
  theme_classic() +
  ylab("Total Run (thousands of fish") +
  geom_vline(xintercept = 2000) + 
  geom_vline(xintercept = 2007)  

### Plot predicted N
old_rr<-ggplot(data = pred_N,aes(x=Year, y = Pred_N/1000)) +
  geom_point() +
  geom_line() + 
  theme_classic() +
  ylab("Total Run (thousands of fish") +
  geom_vline(xintercept = 2007, linetype =2, color ="blue")  + #end of Bue study
  geom_vline(xintercept = 1986, linetype =2, color ="blue") + # start of Bue study
  geom_line(data = bue_estimated, aes(x=Year, y =Estimate_Thousands), color = "red") +
  labs(caption = "red is Bue estimate, black is my estimate")

# 
# pdf("output/Old_RR_1976_2007.pdf")
# print(old_rr)
# dev.off()
# Plot predicted catch 2 data sources  ======================================================================
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


ggplot(data = join %>% filter(!id=="bue_pred_catch"), aes(x=year, y=catch/1000, group = id, color = id)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  facet_wrap(~week,scales="free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#add bues in compare to mine 
ggplot(data = join%>% filter(!id=="obs_catch"), aes(x=year, y=catch/1000, group = id, color = id)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  facet_wrap(~week,scales="free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))






