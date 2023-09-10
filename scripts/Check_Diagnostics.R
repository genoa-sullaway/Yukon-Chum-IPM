
 
mack_diagnostics <- rstan::get_sampler_params(bh_fit) %>% 
  set_names(1:n_chains) %>% 
  map_df(as_data_frame,.id = 'chain') %>% 
  group_by(chain) %>% 
  mutate(iteration = 1:length(chain)) %>% 
  mutate(warmup = iteration <= warmups)

mack_diagnostics %>% 
  group_by(warmup, chain) %>% 
  summarise(percent_divergent = mean(divergent__ >0)) %>% 
  ggplot() +
  geom_col(aes(chain, percent_divergent, fill = warmup), position = 'dodge', color = 'black') + 
  scale_y_continuous(labels = scales::percent, name = "% Divergent Runs")  

mack_diagnostics %>% 
  ggplot(aes(iteration, treedepth__, color = chain)) + 
  geom_line() + 
  geom_hline(aes(yintercept = max_treedepth), color = 'red')  

mack_diagnostics %>% 
  ggplot(aes(iteration, stepsize__, color = chain)) + 
  geom_line()

bh_summary <- summary(bh_fit)$summary %>% 
  as.data.frame() %>% 
  mutate(variable = rownames(.)) %>% 
  select(variable, everything()) %>% 
  as_data_frame()

bh_summary %>% 
  ggplot(aes(n_eff)) + 
  geom_histogram() + 
  geom_vline(aes(xintercept = 4000), color = 'red')

bh_summary %>% 
  ggplot(aes(Rhat)) + 
  geom_histogram() + 
  geom_vline(aes(xintercept = 1.1), color = 'red')
