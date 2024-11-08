
# make traceplots for supplement:

library(rstan)
library(bayesplot)
library(gridExtra)
library(ggplot2)
library(cowplot)
library(tidyverse)

# traceplot ========
traceplot(bh_fit,pars=  c( "theta1[1]", "theta1[2]", "theta1[3]", "theta1[4]",
                           "theta2[1]", "theta2[2]", "theta2[3]", "theta2[4]"))

traceplot(bh_fit,pars=  c( "D_scale" ))

traceplot(bh_fit,pars=  c( "basal_p_1", "basal_p_2"))

traceplot(bh_fit,pars=  c( "log_c_1", "log_c_2"))

traceplot(bh_fit,pars=  c("log_catch_q"))

traceplot(bh_fit,pars=  c("Dir_alpha"))

traceplot(bh_fit,pars=  c("log_c_1","log_c_2"))

traceplot(bh_fit,pars=  c("pi"))

traceplot(bh_fit,pars=  c("sigma_sp"))

traceplot(bh_fit,pars=  c("sigma_catch"))

traceplot(bh_fit,pars=  c("log_F"))

traceplot(bh_fit,pars=  c("log_S"))


# from AI =====
# Function to create multi-page traceplot figures
create_traceplot_pages <- function(fit, 
                                   parameters, 
                                   plots_per_page = 4,
                                   output_file = "traceplot_supplement.pdf",
                                   fig_height = 11,
                                   fig_width = 8.5) {
  
  # Extract posterior draws including warmup
  posterior <- na.omit(as.array(fit))
 
  # Create individual traceplots with improved formatting
  trace_plots <- lapply(parameters, function(param) {
    mcmc_trace(posterior, 
               pars = param,
               n_warmup = floor(dim(posterior)[1]/2)) +
      theme_bw() +
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
  
  # Calculate number of pages needed
  n_pages <- ceiling(length(parameters) / plots_per_page)
  
  # Open PDF device
  png(output_file, height = fig_height, width = fig_width)
  
  # Create pages
  for(i in 1:n_pages) {
    # Get plots for current page
    start_idx <- (i-1) * plots_per_page + 1
    end_idx <- min(i * plots_per_page, length(trace_plots))
    current_plots <- trace_plots[start_idx:end_idx]
    
    # Arrange plots on page
    plot_grid(
      plotlist = current_plots,
      ncol = 1,
      align = "v",
      axis = "lr",
      labels = paste0(LETTERS[1:length(current_plots)], ")"),
      label_size = 12
    )
    
    # Add page number
    title <- textGrob(
      paste0("Supplementary Figure S", i, 
             ": Traceplots for MCMC convergence diagnostics"),
      gp = gpar(fontsize = 12, fontface = "bold")
    )
    grid.arrange(title, arrangeGrob(
      plot_grid(
        plotlist = current_plots,
        ncol = 1,
        align = "v",
        axis = "lr",
        labels = paste0(LETTERS[1:length(current_plots)], ")"),
        label_size = 12
      )
    ), heights = c(0.05, 0.95))
  }
  
  # Close PDF device
  dev.off()
}

# list parameters ======= 
parameters_to_plot <- c(
  "log_c_1",
  "log_c_2",
  "log_sigma_sp",
  "log_sigma_catch",
  "theta1[[1]]",
  "theta1[[2]]",
  "theta1[[3]]",
  "theta1[[4]]",
  "theta2[[1]]",
  "theta2[[2]]",
  "theta2[[3]]",
  "theta2[[4]]",
  "D_scale",
  "g",
  "pi",
  "log_catch_q",
  "log_S",
  "basal_p_1",
  "basal_p_2")

# Create the multi-page figure
create_traceplot_pages(
  fit = fit,  # your stan fit object
  parameters = parameters_to_plot,
  plots_per_page = 4,  # adjust based on your preference
  output_file = "Supplement_parameter_traceplots.png",
  fig_height = 11,     # standard letter paper height
  fig_width = 8.5      # standard letter paper width
)

