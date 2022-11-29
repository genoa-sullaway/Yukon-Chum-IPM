library(here)
library(tidyverse)
library(mgcv)

source("scripts/Function_match_ROMS_Ecofoci.R")


zoop <- read.csv(here("data", "Processed_Data", "NBS_Zoop_Process_Final.csv")) %>%
  unite("ecofoci_date", c(YEAR, MONTH, DAY), sep = "/", remove = FALSE) %>%
  ungroup() %>%
  dplyr::mutate(ecofoci_date = as.Date(ecofoci_date, "%Y/%m/%d"),
                ID = row_number())  %>%
  filter(!is.na(ecofoci_date)) %>% # NAs for date in the following years: 2002 2003 2005 2007 2010 2012, 4000 data points. Deleting for now unitl I talk to DK. 
  group_by(ID, YEAR, MONTH, DAY, ecofoci_date, LAT, LON, TAXA_COARSE) %>%  # ROMS doesnt use stages - sum across stages 
#BIOMASS_MEAN_MG_DW 
  dplyr::summarise(BIOMASS_C_MG_M3_MEAN = sum(BIOMASS_C_MG_M3_MEAN) ) %>%
  filter(!LAT >68,
         !LON > -155)
 
# Load ROMS
ROMS<- readRDS(here("data/romsL2.RDS")) %>%
  # dplyr::rename(L2_mg_c = species ) %>%
  dplyr::rename(latitude= "lat_rho", # these are mis-labeled in the OG df!!! 
                longitude= "lon_rho")  %>%
  dplyr::select(date, latitude, longitude, NCaS, NCaO, Cop) %>%
  dplyr::mutate(latitude= round(latitude, 4),longitude= round(longitude, 4),
                ID = row_number()) %>% 
  filter(!latitude >68)

#Match ROMS and zoop 
#check spatial extent of ROMS
# check <- data.frame(unique(ROMS[c("latitude", "longitude")]))
# 
# ggplot(data = check) +
#   geom_point(aes(x=longitude, y = latitude))
# 
# #check spatial extent of ROMS
# check <- data.frame(unique(zoop[c("LAT", "LON")]))
# 
# ggplot(data = zoop) +
#   geom_point(aes(x=LON, y = LAT)) +
#   geom_sf(data = ak)

prep_data_GAMS<-function(ROMS_sp, ecofoci_sp, eco_lab){

  #Filter ecofoci to species of interest
  zoop_df <- zoop %>%
    filter(TAXA_COARSE == ecofoci_sp) 
  
  #Calanus ROMS and Ecofoci match - run function (loaded above)
  nn <-match_ROMS_ecofoci(eco_df = zoop_df,
                          roms_df=ROMS,
                          year_column = 2, #year column within ecofoci dataframe
                          roms_date_col = 1,
                          roms_lat = 2,
                          roms_lon = 3,
                          eco_lat = 6,
                          eco_lon = 7,
                          species= ecofoci_sp)

  df<-nn %>%
    dplyr::mutate(day_of_year = yday(date)) %>%
    ungroup() %>% 
    dplyr::select(c(1:12), ROMS_sp)
 
  
  #save DF for model 
  saveRDS(df, paste0("data/ROMS_EMA",eco_lab,".RDS"))
  return(df)
}

calanus <-prep_data_GAMS(ROMS_sp= "NCaS", ecofoci_sp= "Calanus marshallae/glacialis", eco_lab = "calanus")
prep_data_GAMS(ROMS_sp= "Cop", ecofoci_sp= "Pseudocalanus")
prep_data_GAMS(ROMS_sp= "NCaO", ecofoci_sp= "Neocalanus")

#Now that ROMS is 