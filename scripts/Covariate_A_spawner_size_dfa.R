library(here)
library(tidyverse)

# data load combine =====
## load older data already compiled by oke et al  ==================
asl_dat <-read.csv("data/ASL_summary_byFWSWage.csv") %>%
  filter(Species == "chum",
         !sampleYear < 1980,
         # ASLProjectType %in% c("commercial catch", "escapement"),
         SASAP.Region %in% c("Yukon"), 
         !Fresh.Water.Age == "NA", 
         !is.na(Fresh.Water.Age),
         !is.na(Salt.Water.Age),
         !Salt.Water.Age == "NA",
         Salt.Water.Age %in% c(2,3,4,5,6,7))  %>%
  dplyr::rename(Location = "LocationID",
                Length = "mean") %>%
  dplyr::select(sampleYear,ASLProjectType, Fresh.Water.Age,Salt.Water.Age, Species,Location, Length)

## load new data =========
filenames<- list.files("data/size_covariate/", pattern="*.csv", full.names=TRUE)

size_new_list <- lapply(filenames, read.csv)

size_new_df<-bind_rows(size_new_list, .id = "column_label") %>%
  dplyr::rename(sampleYear = "Sample.Year") %>%
  filter(!Fresh.Water.Age == "NA",
         !is.na(Fresh.Water.Age),
         !is.na(Salt.Water.Age),
         !Salt.Water.Age == "NA",
         Species== "Chum") %>% 
  dplyr::select(sampleYear,ASLProjectType, Fresh.Water.Age,Salt.Water.Age, Species,Location,Length) 

## combine =============
size <- rbind(size_new_df,asl_dat) %>%
  # unite("total_age", 3:4,sep = ".") %>% 
  filter(ASLProjectType %in% c("escapement", "Escapement","General Escapement")) %>% 
  filter(!is.na(Length),
         !Salt.Water.Age ==6) # only 1 measurement here   

# look at mean size by age and river system
size_sum <-size %>% 
  group_by(sampleYear,Location,Salt.Water.Age) %>% 
  dplyr::summarise(mean = mean(Length))

ggplot(data = size_sum, 
       aes(x=sampleYear, y=mean, color = Location, group = Location))+
  #  geom_path() + 
  geom_point() + 
  facet_wrap(~Salt.Water.Age) +
  theme_classic() +
  theme(legend.position="none") +
  ylab("Length")

# make them go back to brood year?!?!
# look  at trend just mean across locations by age
brood_size <- size %>% 
  dplyr::mutate(brood_year = sampleYear-Salt.Water.Age) %>%
  group_by(brood_year,Salt.Water.Age) %>% 
  dplyr::summarise(mean = mean(Length),
                   sd =sd(Length))

# plot trend among ages 
ggplot(data = brood_size, 
       aes(x=brood_year, y=mean, color = Salt.Water.Age, group = Salt.Water.Age))+
  geom_path() + 
  geom_point() + 
  geom_smooth() +
  facet_wrap(~Salt.Water.Age) +
  theme_classic() +
  theme(legend.position="none") +
  ylab("Length")

# mean across brood years ==== 
brood_size_scale <- size %>% 
  dplyr::mutate(brood_year = sampleYear-Salt.Water.Age) %>%
  filter(!Fresh.Water.Age == 1) %>% # just one of these 
  # group_by(brood_year,Salt.Water.Age) %>% 
  # dplyr::summarise(mean = mean(Length),
  #                  sd =sd(Length)) %>%
  group_by(Salt.Water.Age) %>% 
  dplyr::mutate(scale = as.numeric(scale(Length)))
 
# # just get mean trend? do i need a DFA? 
# # use mean scaled trend for now.... ============
# size_scale <- size %>%  
#   dplyr::rename(cal_year = "sampleYear") %>% 
#   group_by(cal_year,Salt.Water.Age) %>% 
#   dplyr::summarise(mean = mean(Length),
#                    sd =sd(Length)) %>%
#   group_by(Salt.Water.Age) %>% 
#   dplyr::mutate(scale = as.numeric(scale(mean))) %>% 
#   group_by(cal_year) %>% 
#   dplyr::summarise(mean = mean(scale))

# run DFA to get a mean trend ============
library(MARSS)

fish_data <- brood_size_scale

# Reshape the data for DFA
ts_data <- fish_data %>%
  group_by(sampleYear, Salt.Water.Age) %>%
  summarise(mean_length = mean(scale, na.rm = TRUE)) %>%
  pivot_wider(
    names_from = Salt.Water.Age,
    values_from = mean_length,
    names_prefix = "age_"
  ) %>%
  arrange(sampleYear)

# Convert to matrix format required by MARSS
y <- as.matrix(ts_data[,-1])  # Remove year column
rownames(y) <- ts_data$sampleYear

# Set up model dimensions
num_time <- nrow(y)
num_series <- ncol(y)
num_trends <- 4  # Using one common trend

input_dat <- t(y)

# make Z ==========
Z <- matrix(1, 4, 1)
# 
# Z.model3[seq(1, 29, 4), 1] <- 1  # Column 1: starts at 1, steps of 4 (1,5,9,...,85)
# Z.model3[seq(2, 30, 4), 2] <- 1  # Column 2: starts at 2, steps of 4 (2,6,10,...,86)
# Z.model3[seq(3, 31, 4), 3] <- 1  # Column 3: starts at 3, steps of 4 (3,7,11,...,87)
# Z.model3[seq(4, 32, 4), 4] <- 1  # Column 4: starts at 4, steps of 4 (4,8,12,...,88)

# Set up DFA model
dfa_model <- list(
  Z = Z,#matrix(1, num_trends,num_time),     # Loading matrix
  A = "zero",                             # No interactions between trends
  R = "diagonal and equal",               # Equal observation errors
  B = "identity",                         # Random walk
  U = "zero",                             # No drift
  Q = "diagonal and equal",               # Equal process errors
  x0 = "zero"                             # Start at zero
)

# Fit the model
dfa_fit <- MARSS(input_dat,
                 model = dfa_model,
                 control = list(maxit = 1000))

# Extract the estimated trends
trends <- as.data.frame(t(dfa_fit$states))
trends$Year <- as.numeric(rownames(y))

# Create plot
ggplot(trends, aes(x = Year, y = X1)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Estimated Common Length Trend",
       y = "Trend Value",
       x = "Year")

# save DFA trend results ========
trends_df <- trends %>%
  dplyr::rename(trend = "X1")

write_csv(trends_df,"data/processed_covariates/Stage_A_Size.csv")


