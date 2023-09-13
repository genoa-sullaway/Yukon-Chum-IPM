# Plot SR Curves for observed (simulated) and predicted 
sim_yukon_spring <- read_csv("data/Simulated_Yukon_Spring.csv")[1:3] %>%
  mutate(id = "yukon_spring")
sim_yukon_fall <- read_csv("data/Simulated_Yukon_Fall.csv")[1:3] %>%
  mutate(id = "yukon_fall")
sim_kusko <- read_csv("data/Simulated_Kusko.csv") %>%
  mutate(id = "kusko")

sim_obs<- rbind(sim_yukon_spring,sim_yukon_fall,sim_kusko)
   

params <- bh_summary %>% 
  filter(variable %in% c('alpha[1]','alpha[2]','alpha[3]',
                         'beta[1]','beta[2]','beta[3]')) 

stock_list = c( "yukon_spring",
                "yukon_fall",
                "kusko")
# calculate recruits based on estiamted alpha beta =============
list <- list()
for (k in 1:length(stock_list)) {
  
  filter_id <- stock_list[[k]]
  
  # calculate recruits based on estimated alpha beta
  sim_obs_df <- sim_obs %>% 
    filter(id == filter_id) %>%
    rowwise() %>%
    dplyr::mutate(pred_recruit = as.numeric((params[k,2] * spawners) / (1 + params[k+3,2] * spawners)))
  
  list[[k]] <- sim_obs_df
}

sim_obs_pred<-do.call(rbind, list)
 
ggplot(data = sim_obs_pred) +
  geom_point(aes(x=spawners, y = recruits), alpha = 0.5) +
  geom_point(aes(x=spawners, y = pred_recruit), color = "red",alpha = 0.5) +
  facet_wrap(~id)
#  
# plot(sim_yukon_fall$spawners, sim_yukon_fall$recruits, type = "p", xlab = "S", ylab = "R", 
#      main = "Simulated Fall Yukon")
#  
