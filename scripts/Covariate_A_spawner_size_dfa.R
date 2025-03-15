library(here)
library(tidyverse)
library(MARSS)

# data load combine =====
## load older data already compiled by oke et al  ==================
asl_dat <-read.csv("data/ASL_summary_byFWSWage.csv") %>%
  filter(Species == "chum",
         !sampleYear < 1980, 
         SASAP.Region %in% c("Yukon"), 
         !Fresh.Water.Age == "NA", 
         !is.na(Fresh.Water.Age),
         !is.na(Salt.Water.Age),
         !Salt.Water.Age == "NA",
         ASLProjectType %in% c("escapement", "Escapement","General Escapement"),
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
         Species== "Chum",
        ASLProjectType %in% c("escapement", "Escapement","General Escapement")) %>% 
  dplyr::select(sampleYear,ASLProjectType, Fresh.Water.Age,Salt.Water.Age, Species,Location,Length) %>%
  #SASAP DATA HAS MEAN BY AGE GROUP BY LOCATION, UPDATE TO THAT FOR NEW YEARS TOO. 
  group_by(sampleYear,Fresh.Water.Age,Salt.Water.Age, Species,Location) %>% 
  dplyr::summarise(Length = mean(Length),
                   sd = sd(Length))  

ggplot(data = size_new_df, 
       aes(x=sampleYear, y=Length, color = Location, group = Location))+
  #  geom_path() + 
  geom_point() + 
  facet_wrap(~Salt.Water.Age) +
  theme_classic() + 
  ylab("Length")

## combine =============
size <- rbind(size_new_df,asl_dat) %>%
  # unite("total_age", 3:4,sep = ".") %>% 
  # filter(ASLProjectType %in% c("escapement", "Escapement","General Escapement")) %>% 
  filter(!is.na(Length),
         !Salt.Water.Age ==6) # only 1 measurement here   

# look at mean size by age and river system
size_sum <-size %>% 
  group_by(sampleYear,Location,Salt.Water.Age) %>% 
  dplyr::summarise(mean = mean(Length))

# ggplot(data = size, 
#        aes(x=sampleYear, y=Length, color = Location, group = Location))+
#   #  geom_path() + 
#   geom_point() +  
#   theme_classic() +
#   facet_wrap(~Location) + 
#   theme(legend.position="none") +
#   ylab("Length")

# make them go back to brood year
# no do not actually want them on brood year, want them on calendar year. 
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

# DFA by location =========
to_filter <- size %>% 
  filter(!Fresh.Water.Age == 1) %>%
  group_by(Location) %>% 
  count(sampleYear) %>% 
  ungroup() %>% 
  count(Location)  %>%
  filter(!n<10)

  filter_list <- c(to_filter$Location)

size_scale_location <- size %>% 
  filter(Location %in% filter_list,
         !Fresh.Water.Age == 1) %>%  
  group_by(sampleYear, Location,Salt.Water.Age) %>% 
  dplyr::summarise(length_mean = mean(Length)) %>% 
  group_by(Location,Salt.Water.Age) %>% 
  dplyr::mutate(scale = as.numeric(scale(length_mean)))
 
ggplot(data = size_scale_location, 
       aes(x=sampleYear, y=scale, color = Location, group = Location))+
  # geom_path() + 
  geom_point() + 
  geom_smooth() +
  facet_wrap(~Salt.Water.Age) +
  theme_classic() +
  theme(legend.position="none") +
  ylab("Length")

## make DFA ======
# Reshape the data for DFA
ts_data <- size_scale_location %>%
  dplyr::select(sampleYear, Location,Salt.Water.Age,scale) %>%
  unite("loc_age_id", 2:3) %>% 
  spread(loc_age_id, scale) %>%
  arrange(sampleYear)
  
# Convert to matrix format required by MARSS
y <- as.matrix(ts_data[,-1])  # Remove year column
rownames(y) <- ts_data$sampleYear

# Set up model dimensions
num_time <- nrow(y)
num_series <- ncol(y)
num_trends <- 1  # Using one common trend

input_dat <- t(y)

## Set up DFA model  ========== 
dfa_model <- list(
  Z =  matrix(1, 40, 1),  # Loading matrix
  A = "zero",                             # if this is zero then it means --> No interactions between trends
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

## save DFA trend results ========
trends_df <- trends %>%
  dplyr::rename(trend = "X1")

write_csv(trends_df,"data/processed_covariates/Stage_A_Size.csv")

# is this trend the same across age classes? ==============
head(input_dat)

Z_mat = matrix(0, 40, 4)
# fill in 1's for rows that have corresponding age class 
 
# Extract the age identifiers from row names of input_dat
row_names <- rownames(input_dat)

# Loop through each row name and set the appropriate column to 1
for (i in 1:length(row_names)) {
  # Extract the age number from the end of the string
  age_suffix <- as.numeric(sub(".*_(\\d+)$", "\\1", row_names[i]))
  
  # Map the age to column index (subtracting 1 to get 0-based index, then adding 1 for R's 1-based indexing)
  # Ages 2,3,4,5 map to columns 1,2,3,4
  col_index <- age_suffix - 1
  
  # Set the corresponding element to 1
  Z_mat[i, col_index] <- 1
}
## Set up DFA model  ========== 
dfa_model <- list(
  Z =  Z_mat,  # Loading matrix
  A = "zero",                             # if this is zero then it means --> No interactions between trends
  R = "diagonal and equal",               # Equal observation errors
  B = "identity",                         # Random walk
  U = "zero",                             # No drift
  Q = "diagonal and equal",               # Equal process errors
  x0 = "zero"                             # Start at zero
)

# Fit the model
dfa_fit_age <- MARSS(input_dat,
                 model = dfa_model,
                 control = list(maxit = 1000))

# Extract the estimated trends
trends <- as.data.frame(t(dfa_fit_age$states))
trends$Year <- as.numeric(rownames(y))
trends <- trends %>% 
  gather(1:4, key = "age", value = "trend")

# Create plot
ggplot(trends, aes(x = Year, y = trend, color =age)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Estimated Common Length Trend",
       y = "Trend Value",
       x = "Year")

# aic both DFAs
# evidence for same trend across all age classes! 

AIC(dfa_fit)

AIC(dfa_fit_age)
# 
# # OLD ========
# # this dfa takes the mean across locations looks at age class trend. 
# # maybe better to take mean with lcoation and ages - this what i did above. 
# # mean across brood years ==== 
# brood_size_scale <- size %>% 
#   dplyr::mutate(brood_year = sampleYear-Salt.Water.Age) %>%
#   filter(!Fresh.Water.Age == 1) %>% # just one of these 
#   # group_by(brood_year,Salt.Water.Age) %>% 
#   # dplyr::summarise(mean = mean(Length),
#   #                  sd =sd(Length)) %>%
#   group_by(Salt.Water.Age) %>% 
#   dplyr::mutate(scale = as.numeric(scale(Length)))
# 
# 
# # run DFA to get a mean trend ============
# 
# fish_data <- brood_size_scale
# 
# # Reshape the data for DFA
# ts_data <- fish_data %>%
#   group_by(sampleYear, Salt.Water.Age) %>%
#   summarise(mean_length = mean(scale, na.rm = TRUE)) %>%
#   pivot_wider(
#     names_from = Salt.Water.Age,
#     values_from = mean_length,
#     names_prefix = "age_"
#   ) %>%
#   arrange(sampleYear)
# 
# # Convert to matrix format required by MARSS
# y <- as.matrix(ts_data[,-1])  # Remove year column
# rownames(y) <- ts_data$sampleYear
# 
# # Set up model dimensions
# num_time <- nrow(y)
# num_series <- ncol(y)
# num_trends <- 4  # Using one common trend
# 
# input_dat <- t(y)
# 
# # make Z ==========
# Z <- matrix(1, 4, 1)
# 
# # Set up DFA model
# dfa_model <- list(
#   Z = Z,#matrix(1, num_trends,num_time),  # Loading matrix
#   A = "scaling",                             # if this is 0 then it means --> No interactions between trends
#   R = "diagonal and equal",               # Equal observation errors
#   B = "identity",                         # Random walk
#   U = "zero",                             # No drift
#   Q = "diagonal and equal",               # Equal process errors
#   x0 = "zero"                             # Start at zero
# )
# 
# # Fit the model
# dfa_fit <- MARSS(input_dat,
#                  model = dfa_model,
#                  control = list(maxit = 1000))
# 
# # Extract the estimated trends
# trends <- as.data.frame(t(dfa_fit$states))
# trends$Year <- as.numeric(rownames(y))
# 
# # Create plot
# ggplot(trends, aes(x = Year, y = X1)) +
#   geom_line() +
#   geom_point() +
#   theme_minimal() +
#   labs(title = "Estimated Common Length Trend",
#        y = "Trend Value",
#        x = "Year")
# # 
# # # save DFA trend results ========
# # trends_df <- trends %>%
# #   dplyr::rename(trend = "X1")
# # 
# # write_csv(trends_df,"data/processed_covariates/Stage_A_Size.csv")
# # 
# # 
# 
# 
