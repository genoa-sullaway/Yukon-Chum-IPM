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
bue_estimated <- read_csv("data/Kusko_Reconstruction/Bue_Reconstruction_Dat.csv") 
estimated_parameters<- readRDS("output/current_optim_output_par.RDS")
  

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
  
  q_vec <- par[1] 
  
  # grep("pred_N", par_names) 
  pred_N <- par[2:47]
  
  # grep("ln_pred_slope", par_names) 
  pred_slope <- par[48:56]
  
  # Vectorize Data: ============================================================================
  
  B_yj=as.matrix(data$B_yj)
  obs_catch_week=as.matrix(data$obs_catch_week)
  obs_N=as.matrix(data$obs_N)
  obs_escape_project = as.matrix(data$obs_escape_project)
  
   
  # B_yj_vec <- as.vector(B_yj)
  # obs_catch_week_vec <- as.vector(as.matrix(obs_catch_week))
 
  # N_yi_vec_prop <-as.vector(as.matrix(pred_N*prop))
 
  
  # Predict N - Observed Total Return =========================================================================================
  #pred_N = pred_E + rowSums(pred_catch) 
  
  # N_yi = number of chum present in commercial district by week/year (Eq 4)
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

pred_N<-data.frame(Year = c(1976:2021), Pred_N = c(pre_outputs[[3]]))  

### Plot predicted N
ggplot(data = pred_N,aes(x=Year, y = Pred_N/1000 )) +
  geom_bar(stat= "identity") +
  theme_classic() +
  ylab("Total Run (thousands of fish") +
  geom_vline(xintercept = 2000) + 
  geom_vline(xintercept = 2007)  


### Plot predicted N
current_rr<-ggplot(data = pred_N,aes(x=Year, y = Pred_N/1000)) +
  geom_point() +
  geom_line() + 
  theme_classic() +
  ylab("Total Run (thousands of fish") +
  geom_vline(xintercept = 2007, linetype =2, color ="blue")  + #end of Bue study
  geom_vline(xintercept = 1986, linetype =2, color ="blue") + # start of Bue study
  geom_line(data = bue_estimated, aes(x=Year, y =Estimate_Thousands), color = "red") +
  labs(caption = "red is Bue estimate, black is my estimate")
current_rr

current_rr2<-ggplot(data = pred_N,aes(x=Year, y = Pred_N/1000)) +
  geom_point() +
  geom_line() + 
  theme_classic() +
  ylab("Total Run (thousands of fish") 

current_rr2

pdf("output/Current_RR_1976_2021.pdf")
print(current_rr)
print(current_rr2)
dev.off()

