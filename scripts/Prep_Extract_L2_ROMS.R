#extract hindcasts from "B10K-K20_CORECFS", all variables, depth integrated.  

###########################################################################################################
# Direct Extraction and plotting 
###########################################################################################################
 
library(RNetCDF)
library(tidync)
library(tidyverse)
library(lubridate) 

month_names <- c("02", "03", "04","05","06","07","08","09","10") 
#thredds updated their website in Fall 2022 --- this script was updated with new places where files were stored. 
grid<-tidync("https://data.pmel.noaa.gov/aclim/thredds/dodsC/ancillary/Bering10K_extended_grid.nc")

 
#  One of the native grids contains the columns we need to match the bottom temperature data to latitudes and longitudes.
grid_lkp <- grid %>%
  activate("D8,D2") %>%
  hyper_tibble() %>%
  dplyr::select(lat_rho,lon_rho,xi_rho,eta_rho) %>%
  mutate(lon_rho=lon_rho-360)

#grid_truncated <- read_csv("data/truncated_grid.csv") # this is matched with ecofoci locations and created in Prep_extract_ROMS_z_rho_surveydepth.R  

#  Now load load the ROMS data file (note that this does not extract the data so it is quick).
#ROMS_l2 <- tidync("https://data.pmel.noaa.gov/aclim/thredds/dodsC/Level2/B10K-K20_CORECFS_integrated.nc") #this opens a netcdf for the whole simulation

ROMS_l2<-tidync("https://data.pmel.noaa.gov/aclim/thredds/dodsC/B10K-K20_Level2_CORECFS_integrated_collection.nc")

datevec <- ROMS_l2 %>% 
  activate("D1") %>% 
  hyper_tibble() %>% 
  mutate(date=as_date(as_datetime(ocean_time,origin="1900-01-01 00:00:00", tz = "UTC")),
         date_index=1:n()) %>% 
  filter(date>=as.Date("2000-02-01"))
#print(ROMS)

#  Extract all the zoop across the lookup grid since 2000 
#  Save directly to RDS.
df_filtered <- ROMS_l2 %>% 
  activate("D2,D0,D1") %>% 
  hyper_filter(ocean_time=ocean_time>=ocean_time[min(datevec$date_index)],    # Filter dates based on the datevec index for 1985-01-01
               xi_rho=xi_rho>=min(grid_lkp$xi_rho) & xi_rho<=max(grid_lkp$xi_rho), # Filter xi_rho based on the spatial lookup
               eta_rho=eta_rho>=min(grid_lkp$eta_rho) & eta_rho<=max(grid_lkp$eta_rho)) %>%  # Filter eta_rho based on the spatial lookup
  hyper_tibble(select_var = c("NCaS", "NCaO", "Cop", "EupO"))  %>%
  left_join(datevec) %>%
  left_join(grid_lkp) %>% 
  filter(!lat_rho>75 & !lat_rho<50) %>%
  filter(!lon_rho< -180 & lon_rho < -155)  %>%
  dplyr::select(NCaS, NCaO, Cop, EupO, lat_rho, lon_rho, date,xi_rho,eta_rho) %>%
  separate(date, into= c("year", "month", "day"), remove = FALSE)  %>%
  filter(month %in% month_names) %>% 
  filter(!is.na(NCaS))

saveRDS(df_filtered, "data/romsL2.RDS") 
# 
# #make sure it worked spatially
# temp <- ncas_filtered %>% filter(year == "2000", month == "02", day =="06")
# 
# ggplot(temp) +
#   geom_point(aes(x=longitude, y = latitude, fill = NCaS))
# 
