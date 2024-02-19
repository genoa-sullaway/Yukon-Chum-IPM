
library(GGally)
library(here)
library(tidyverse)

# load covariates ==========
cov_a <- read.csv("data/processed_covariates/stage_a_all.csv") %>%
  gather(2:8, key = "key", value = "value") %>% 
  group_by(key) %>%
  mutate(value = scale(value)) %>%
  spread(key, value)

cov_b <- read.csv("data/processed_covariates/stage_b_all.csv") %>% 
  gather(2:6, key = "key", value = "value") %>% 
  group_by(key) %>%
  mutate(value = scale(value)) %>%
  spread(key, value)

# Test for A =========
X_a<-cov_a[,2:8]
colinearity_a <-ggpairs(X_a)
pdf("output/cova_a_plot.pdf")
print(colinearity_a)
dev.off()

# Test for b =========
X_b<-cov_b[,2:6]

colinearity_b <-ggpairs(X_b)
pdf("output/cov_b_plot.pdf")
print(colinearity_b)
dev.off()
