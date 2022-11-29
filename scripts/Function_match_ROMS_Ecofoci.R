################################################################################

library(here)
library(tidyverse)
library(lubridate)
library(yaImpute) #this package is for the ann - approx nearest neighbor 

#function for spatial temporal overlap
#ask which value in the ROMS dates minimize the difference between ROMS and each survey ecofoci date, go through all the ROMS and check it against ecofoci date, do this for each ecofoci date entry...
# @eco_df - Ecofoci dataframe 
# @roms_df - ROMS dataframe
# @year_column - just a number, year column within ecofoci dataframe
# @roms_date_col - just a numbner, date column within ROMS dataframe
# 
# @roms_lat - just a number, latitude in ROMS dataframe
# @roms_lon - just a number, longitude in ROMS dataframe

# @eco_lat - just a number, latitude in ecofoci dataframe
# @eco_lon - just a number, longitude in ecofoci dataframe
# @ type  - ROMS code for the species you are working with ie. "NCaS"

#species - for saving data frame with correct name

match_ROMS_ecofoci <- function(eco_df,roms_df,year_column,roms_date_col,roms_lat,roms_lon,eco_lat,eco_lon,species){
 
  roms_temporal_id <- list()
  for (i in 1:nrow(eco_df)) {
    
    # roms_filter<- roms_df %>%  
    #  dplyr::filter(year ==  as.numeric(eco_df[i,year_column])) # pulls out all ROMS for specific year 
    # 
    roms_time_index <- which.min(abs(roms_df$date - eco_df$ecofoci_date[i])) # look across all the ROMS rows and see which one minimizese the difference in dates 
    
    roms_temporal_id[[i]] <-roms_df[roms_time_index,roms_date_col] # 9 is the date column, I don't need all the info just the matching ROMS date
  }
  
  #list to data frame 
  roms_temporal_df <-map_df(roms_temporal_id,~as.data.frame(x=.x), .id= "roms_date")  
  
  #do the spatial search
  # the 'ann' command from the 'yaImpute' package
  # Approximate nearest-neighbour search, reporting 2 nearest points (k=2)
  # This command finds the 3 nearest points in foo2 for each point in foo1
  # In the output:
  #   The first k columns are the row numbers of the points
  #   The next k columns (k+1:2k) are the *squared* euclidean distances
  knn.out <- ann(as.matrix(roms_df[,roms_lat:roms_lon]), as.matrix(eco_df[,eco_lat:eco_lon]), k=1) #PULL OUT LAT AND LONGS
  roms_spatial_index<-as.data.frame(knn.out$knnIndexDist)
  roms_spatial_index <-roms_spatial_index %>%
    dplyr::rename(ID = V1) %>%
    dplyr::select(-V2)
  
  #need to filter the ROMS data set by the ID's, dont want all data just lat and longs
  roms_lat_lon <- semi_join(roms_df,roms_spatial_index, by = "ID" ) %>%
    dplyr::select(ID,latitude ,longitude)
  
  #join back to get lat and longs specific to each ecofoci row 
  space_time_index<-left_join(roms_spatial_index,roms_lat_lon) %>%
    cbind(roms_temporal_df) %>%
    dplyr::select(-roms_date, - ID)  
  
  #put it all together
  output_nn <-eco_df %>% 
    dplyr::select(-ID) %>%
    cbind(space_time_index) %>%
    left_join(roms_df %>% dplyr::select(-ID), by = c("latitude", "longitude", "date")) #%>% #add in roms data based on the space time date combo...  
   # dplyr::select(-ID)
  
  test <- space_time_index %>%
    left_join(roms_df %>% dplyr::select(-ID), by = c("latitude", "longitude", "date"))  
    
  #write_csv(output_nn, paste("data/",species,"_nn.csv"))
  return(output_nn)
}
