library(GGally)
library(here)
library(tidyverse)

year_min = 2001
year_max_cal = 2020
year_max_brood = 2017

# load more recent covariates ========================
stage_a_cov <- read_csv("data/processed_covariates/stage_a_all.csv") %>%
  dplyr::mutate(yukon_mean_discharge = as.numeric(scale(yukon_mean_discharge)),
                SST_CDD_NBS = as.numeric(scale(SST_CDD_NBS)),
                full_index = as.numeric(scale(full_index))) %>%
  # zoop are already mean scaled
  dplyr::rename(cal_year = Year) %>% 
  # dplyr::mutate(brood_year = cal_year-1) %>% 
  # filter(brood_year >= year_min, 
  #        brood_year <= year_max_brood) %>%
  dplyr::select(SST_CDD_NBS,# yukon_mean_discharge,
                Large_zoop,
                Cnideria,
                full_index) 


X_a<-stage_a_cov[,1:4]
colinearity_a <-ggpairs(X_a)
pdf("output/cova_a_plot.pdf")
print(colinearity_a)
dev.off()




# old ====================================
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
