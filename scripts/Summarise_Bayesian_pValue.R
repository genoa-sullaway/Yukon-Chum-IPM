# First, load required packages
library(loo)
library(bayesplot)
library(rstanarm)

fit<- read_rds("output/stan_fit_DATA.RDS")
 

# just using example objects that come with bayesplot

# juveniles ====== 
# y <- example_y_data()
# yrep <- example_yrep_draws()
juv_rep <- as.matrix(fit, pars = "N_j_predicted")
any(is.na(juv_rep))
juv_rep_clean <- juv_rep[, colSums(is.na(juv_rep)) == 0]


y <- data_list_stan$data_stage_j
yrep <- juv_rep_clean  

# using stat=median but doesn't matter which stat
plot <- ppc_stat(y, yrep, stat = "median")

# calculate proportion of stat(yrep) > stat(y)
p_juv <- mean(apply(yrep, 1, median) > median(y))
plot + 
  yaxis_text() + # just so I can see y-axis values for specifying them in annotate() below, but can remove this if you don't want the useless y-axis values displayed 
  annotate("text", x = 89, y = 40, label = paste("p =", p_juv))

# returns ====== 
# y <- example_y_data()
total_rep <- as.matrix(fit, pars = "N_brood_year_return")
any(is.na(total_rep))
total_rep_clean <- total_rep[, colSums(is.na(total_rep)) == 0]

y <- data_list_stan$data_stage_return
yrep <- total_rep_clean[,1:17]  
ncol(yrep)

# using stat=median but doesn't matter which stat
plot <- ppc_stat(y, yrep, stat = "median")

# calculate proportion of stat(yrep) > stat(y)
p_return <- mean(apply(yrep, 1, median) > median(y))
 
plot + 
  yaxis_text() + # just so I can see y-axis values for specifying them in annotate() below, but can remove this if you don't want the useless y-axis values displayed 
  annotate("text", x = 89, y = 40, label = paste("p =", p_return))
 
# harvest ====== 
# y <- example_y_data()
total_harvest <- as.matrix(fit, pars = "N_catch")
any(is.na(total_harvest))
total_harvest_clean <- total_harvest[, colSums(is.na(total_harvest)) == 0]


y <- data_list_stan$data_stage_harvest
yrep <- total_harvest_clean 
 
yrep_clean <- as.data.frame(yrep) %>%
  # Add a draw_id column to keep track of posterior draws
  dplyr::mutate(draw_id = row_number()) %>%
  # Pivot longer all columns except draw_id 
  pivot_longer(
    -draw_id,  # This excludes draw_id from being pivoted
    names_to = c("first_num", "second_num"),
    names_pattern = "N_catch\\[(\\d+),(\\d+)\\]",
    values_to = "value"
  ) %>%
  # Group by both draw_id and first number to maintain row structure
  group_by(draw_id, first_num) %>%
  summarise(sum = sum(value), .groups = 'drop') %>%
  # Pivot wider to get back to wide format with one sum per first_num
  pivot_wider(
    names_from = first_num,
    values_from = sum,
    names_prefix = "N_catch_sum_"
  ) %>%
  # Remove draw_id if you don't need it
  dplyr::select(-draw_id) %>%
  dplyr::select(paste0("N_catch_sum_", 1:26)) %>%
  dplyr::select(c(1:21)) %>%
  as.matrix()
 
# using stat=median but doesn't matter which stat
plot <- ppc_stat(y, yrep_clean, stat = "median")

# calculate proportion of stat(yrep) > stat(y)
p_harvest <- mean(apply(yrep_clean, 1, median) > median(y))

plot + 
  yaxis_text() + # just so I can see y-axis values for specifying them in annotate() below, but can remove this if you don't want the useless y-axis values displayed 
  annotate("text", x = 89, y = 40, label = paste("p =", p_harvest))


# spawners =========== 
total_sp <- as.matrix(fit, pars = "N_sp")
any(is.na(total_sp))
total_sp_clean <- total_sp[, colSums(is.na(total_sp)) == 0]

y <- data_list_stan$data_stage_sp[1:20]
yrep <- total_sp_clean 
 
# plot(y)
 
yrep_clean <- as.data.frame(yrep) %>%
  # Add a draw_id column to keep track of posterior draws
  dplyr::mutate(draw_id = row_number()) %>%
  # Pivot longer all columns except draw_id 
  pivot_longer(
    -draw_id,  # This excludes draw_id from being pivoted
    names_to = c("first_num", "second_num"),
    names_pattern = "N_sp\\[(\\d+),(\\d+)\\]",
    values_to = "value"
  ) %>%
  # Group by both draw_id and first number to maintain row structure
  group_by(draw_id, first_num) %>%
  summarise(sum = sum(value), .groups = 'drop') %>%
  # Pivot wider to get back to wide format with one sum per first_num
  pivot_wider(
    names_from = first_num,
    values_from = sum,
    names_prefix = "N_sp_sum_"
  ) %>%
  # Remove draw_id if you don't need it
  dplyr::select(-draw_id) %>%
  dplyr::select(paste0("N_sp_sum_", 1:26)) %>%
  dplyr::select(c(1:20)) %>%
  as.matrix()

# using stat=median but doesn't matter which stat
plot <- ppc_stat(y, yrep_clean, stat = "median")
 
# calculate proportion of stat(yrep) > stat(y)
p_spawners <- mean(apply(yrep_clean, 1, median) > median(y))

plot + 
  yaxis_text() + # just so I can see y-axis values for specifying them in annotate() below, but can remove this if you don't want the useless y-axis values displayed 
  annotate("text", x = 89, y = 40, label = paste("p =", p_spawners))

# paste all p's together: =====
p_juv
p_return
p_harvest
p_spawners
