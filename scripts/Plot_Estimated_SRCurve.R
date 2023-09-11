# see plot posterior bh v1 for files... 

params <- bh_summary %>% 
  filter(variable %in% c('alpha','beta'))

# calculate recruits based on estiamted alpha beta
sim_yukon_spring <- sim_yukon_spring %>% 
  rowwise() %>%
  dplyr::mutate(pred_recruit = as.numeric((params[1,2] * spawners) / (1 + params[2,2] * spawners)))
 
ggplot(data = sim_yukon_spring) +
  geom_point(aes(x=spawners, y = recruits), alpha = 0.5) +
  geom_point(aes(x=spawners, y = pred_recruit), color = "red",alpha = 0.5) 
#  
# plot(sim_yukon_fall$spawners, sim_yukon_fall$recruits, type = "p", xlab = "S", ylab = "R", 
#      main = "Simulated Fall Yukon")
#  
