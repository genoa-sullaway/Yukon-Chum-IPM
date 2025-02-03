
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

posterior_samples <- as.matrix(fit)
any(is.na(posterior_samples))
clean_posterior <- posterior_samples[, colSums(is.na(posterior_samples)) == 0]


# list parameters ======= 
parameters_to_plot <- c(
  # "log_c_1",
  # "log_c_2",
  "log_sigma_sp",
  "log_sigma_catch",
  "log_sigma_y_j", 
  # "theta1[[1]]",
  "theta1[1]" ,"theta1[2]" ,"theta1[3]" ,"theta1[4]" ,
  "theta2[1]","theta2[2]","theta2[3]","theta2[4]",
  "D_scale",
  "log_catch_q")

parameters = parameters_to_plot

# Create individual traceplots with improved formatting
trace_plots <- lapply(parameters, function(param) {
  # color_scheme_set("mix-blue-red")
  mcmc_trace(clean_posterior, 
             pars = param, 
             div_color = "red",
             n_warmup = floor(dim(clean_posterior)[1]/2)) +
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
                  trace_plots[[13]])

traceplots_all
ggsave("output/Supplementary_Traceplots.png", width =10, height = 6)

# old ======== 
# traceplot function =====
# Function to create multi-page traceplot figures
create_traceplot_pages <- function(fit, 
                                   parameters, 
                                   plots_per_page = 4,
                                   output_file = "traceplot_supplement.pdf",
                                   fig_height = 11,
                                   fig_width = 8.5) {

  posterior_samples <- as.matrix(fit)
  any(is.na(posterior_samples))
  clean_posterior <- posterior_samples[, colSums(is.na(posterior_samples)) == 0]

  # Create individual traceplots with improved formatting
  trace_plots <- lapply(parameters, function(param) {
    color_scheme_set("mix-blue-red")
    mcmc_trace(clean_posterior, 
               pars = param,
               n_warmup = floor(dim(clean_posterior)[1]/2)) +
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
  pdf(output_file, height = fig_height, width = fig_width)
  
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
    # title <- textGrob(
    #   paste0("Supplementary Figure S", i, 
    #          ": Traceplots for MCMC convergence diagnostics"),
    #   gp = gpar(fontsize = 12, fontface = "bold")
    # )
    grid.arrange(#title, 
                 arrangeGrob(
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
 
 
# # Create the multi-page figure
# create_traceplot_pages(
#   fit = fit,  # your stan fit object
#   parameters = parameters_to_plot,
#   plots_per_page = 4,  # adjust based on your preference
#   output_file = "output/Supplement_parameter_traceplots.pdf",
#   fig_height = 11,     # standard letter paper height
#   fig_width = 8.5      # standard letter paper width
# )

   