
# make traceplots for supplement ---- this generally works and makes nice traceplots. 
# but the format part of the component doesnt need to revisit how it is formatted and saved
# but the atual plot part of the function works 

library(rstan)
library(bayesplot)
library(gridExtra)
library(ggplot2)
library(cowplot)
library(tidyverse)

fit<- read_rds("output/stan_fit_DATA.RDS")

# have to remove NAs so trace plot function works. 
# posterior_samples <- as.matrix(fit)
# any(is.na(posterior_samples))
# clean_posterior <- posterior_samples[, colSums(is.na(posterior_samples)) == 0]
#  
posterior_array <- as.array(fit)
has_na <- apply(posterior_array, 3, function(param_array) any(is.na(param_array)))
# Keep only parameters without any NA values
clean_posterior_array <- posterior_array[,, !has_na]
parameters_to_plot <- c("Dir_alpha")

# list parameters to plot ======= 
parameters_to_plot <- c(
  "log_c_1",
  "log_c_2",
  "alpha[1]",  "alpha[2]",  "alpha[3]",  "alpha[4]", 
  "prob[1]", "prob[2]", "prob[3]",  
  "D_scale",
  "log_F_mean",   
  "basal_p_1",  "basal_p_2", 
  "theta1[1]" ,"theta1[2]" ,"theta1[3]" ,"theta1[4]" ,
  "theta2[1]","theta2[2]","theta2[3]", 
  "log_catch_q")

parameters = parameters_to_plot

# color_scheme_set("viridisD") 
# color_scheme_set("blue")

# Create individual traceplots with improved formatting
trace_plots <- lapply(parameters, function(param) {
  color_scheme_set("mix-blue-red")
  mcmc_trace(clean_posterior_array, 
             pars = param#, 
             # div_color = "red"#,
             #n_warmup = floor(dim(clean_posterior)[1]/2)
             ) +
    # theme_bw() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      legend.position = "none"
    ) +
    labs(
      title = param,
      y = "Parameter Value",
      x = "Iteration"
    )
})


# Create individual traceplots with improved formatting
  
legend <- get_legend(mcmc_trace(clean_posterior_array, 
             pars = "theta2[1]") +
    # theme_bw() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9)#,
      # legend.position = "none"
    ) +
    labs(
      title = "theta2[1]",
      y = "Parameter Value",
      x = "Iteration"
    ) )

traceplots_all <- ggpubr::ggarrange(trace_plots[[1]],
                  trace_plots[[2]],
                  trace_plots[[3]],
                  trace_plots[[4]],
                  trace_plots[[5]],
                  trace_plots[[6]],
                  trace_plots[[7]],
                  trace_plots[[8]],
                  trace_plots[[9]],
                  trace_plots[[10]],
                  trace_plots[[11]],
                  trace_plots[[12]],
                  trace_plots[[13]],
                  
                  trace_plots[[14]],
                  trace_plots[[15]],
                  trace_plots[[16]],
                  trace_plots[[17]],
                  trace_plots[[18]],
                  trace_plots[[19]],
                  trace_plots[[20]],
                  trace_plots[[21]],
                  legend  
                  # trace_plots[[22]],
                  # trace_plots[[23]],
                  # trace_plots[[24]]
                  
                  )

#traceplots_all
ggsave("output/Supplementary_Traceplots.png", width = 10, height = 6)
