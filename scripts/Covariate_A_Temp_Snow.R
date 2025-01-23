# Load required libraries
library(tidyverse)
library(here)
library(purrr)
library(readr)

# downloaded here : https://akclimate.org/data/data-portal/ 

# this is for snow depth ============
# List all CSV files in the snow_depth folder
files <- list.files(path = "data/Fairbanks_airport_covariates/snow_depth", pattern = "*.csv", full.names = TRUE)
 
# Create a month lookup vector
month_lookup <- c(
  "January" = 1, "February" = 2, "March" = 3, "April" = 4,
  "May" = 5, "June" = 6, "July" = 7, "August" = 8,
  "September" = 9, "October" = 10, "November" = 11, "December" = 12
)

file =files[1]

# Function to read and clean each CSV file
read_snow_data <- function(file) {
  # Read the file
  df <- readr::read_csv(file, skip = 5)
  
  # Get the column name containing "Maximum Snow Depth"
 snow_depth_col <- names(df)[grep("Maximum Snow Depth", names(df))][[1]]
  
  # Extract month from column name
  month_name <- stringr::str_extract(snow_depth_col, "January|February|March|April|May|June|July|August|September|October|November|December")
 
 output <- df %>%
   filter(!Date =="Maximum") %>%
   # dplyr::select(Date, dplyr::all_of(snow_depth_col)) %>%  # Select relevant columns
   dplyr::rename(max_snow_depth_in = 2)%>% # `October Maximum Snow Depth (in)`) %>%  # Rename the snow depth column
    dplyr::mutate(
      year = as.numeric(Date),  # Convert Date column to year
      month = month_lookup[month_name]  # Convert month name to number
    ) %>%
    dplyr::select(year, month, max_snow_depth_in) #%>%  # Select and order final columns
    # dplyr::as_tibble()  # Ensure tibble format
}

# Read and combine all files
combined_data_snow <- files %>%
  purrr::map_df(read_snow_data) %>%  # Apply the function to all files
  dplyr::arrange(year, month) %>% # Sort by year and month
  dplyr::mutate(year = case_when(month == 1 ~ year -1,
                                  TRUE ~ year)) %>%
  filter(!year <1950)
 

# Optionally save the combined dataset
# readr::write_csv(combined_data, "data/Fairbanks_airport_covariates/combined_snow_depth.csv")

# this is for temperature ============ 
files <- list.files(path = "data/Fairbanks_airport_covariates/temp", pattern = "*.csv", full.names = TRUE)

# Create a month lookup vector
month_lookup <- c(
  "January" = 1, "February" = 2, "March" = 3, "April" = 4,
  "May" = 5, "June" = 6, "July" = 7, "August" = 8,
  "September" = 9, "October" = 10, "November" = 11, "December" = 12
)

 file = files[1]

# Function to read and clean each CSV file
read_temp_data <- function(file) {
  # Read the file
  df <- readr::read_csv(file, skip = 5)
  
  # Get the column name containing "Maximum Snow Depth"
 temp_col <- names(df)[grep("Maximum Temperature", names(df))][[1]]
  
  # Extract month from column name
  month_name <- stringr::str_extract(temp_col, "January|February|March|April|May|June|July|August|September|October|November|December")
  
  output <- df %>%
    filter(!Date =="Maximum") %>%
    # dplyr::select(Date, dplyr::all_of(snow_depth_col)) %>%  # Select relevant columns
    dplyr::rename(min_temperature = 4,
                  mean_temperature=6
                  )%>% # `October Maximum Snow Depth (in)`) %>%  # Rename the snow depth column
    dplyr::mutate(
      year = as.numeric(Date),  # Convert Date column to year
      month = month_lookup[month_name]  # Convert month name to number
    ) %>%
    dplyr::select(year, month, min_temperature,mean_temperature) #%>%  # Select and order final columns
  # dplyr::as_tibble()  # Ensure tibble format
}

# Read and combine all files
combined_data_temp <- files %>%
  purrr::map_df(read_temp_data) %>%  # Apply the function to all files
  dplyr::arrange(year, month) %>% # Sort by year and month
  dplyr::mutate(year = case_when(month == 1 ~ year -1,
                                 TRUE ~ year)) %>%
  filter(!year <1950)

# combine both =====
combined_dat <- left_join(combined_data_snow, combined_data_temp) 
  
 
data_plot <- combined_dat %>% 
  group_by(month) %>%
  dplyr::mutate(max_snow_depth_in = as.numeric(scale(max_snow_depth_in)),
                min_temperature = as.numeric(scale(min_temperature)),
                mean_temperature= as.numeric(scale(mean_temperature))) %>% 
  gather(3:5, key = "id", value = "value") 

ggplot(data = data_plot, aes(x=year, y =value, color = id, group =id)) +
  geom_path() +
  facet_wrap(~month)

# Optionally save the combined dataset
readr::write_csv(combined_dat, "data/Fairbanks_airport_covariates/combined_temp_fairbanks.csv")
