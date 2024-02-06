# Plot all covariates in one plot 
library(tidyverse)
library(here)

# Stage A - Load data ============= 
# Cumulative degree days
sst_a <- read_csv("data/processed_covariates/Stage_A_CDD.csv")




# Stage A - Plots ============= 
plot_CDD <- ggplot(data = sst_a) +
  geom_point(aes(x=year, y = CDD)) +
  geom_line(aes(x=year, y = CDD)) +
  theme_classic() +
  geom_hline(yintercept = mean(sst_a$CDD), linetype =2) +
  ylab("NBS Cumulative Degree Days") +
  xlab("Year") + 
  ggtitle("Stage A - CDD NBS") +
  labs(caption = "horizontal line is the mean for timeseries")




