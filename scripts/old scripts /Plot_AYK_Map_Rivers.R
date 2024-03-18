# install.packages("rgdal")
library(rgdal)
library(here)
library(sf)
library(sp)

source("scripts/Function_AKMap.R")

# Import US zipcode data
rivers <- readOGR(dsn = "data/USA_Major_Rivers/v10/rivers.gdb") 

yukon<-st_read(here("data/Yukon/yuk_da.shp"))
plot(yukon)

dat <- rivers$NAME %>%  filter(NAME %in% c("Yukon", "Kuskokwim"))

 
x <- st_read(dsn="data/USA_Major_Rivers/v10/rivers.gdb")
y <- st_zm(x)
z <- as(y, "Spatial")

t<- subset(z, NAME %in% c("Yukon", "Kuskokwim"))
plot(t)
river_sf<- st_as_sf(t)
 
ggplot( ) +
  geom_sf(data = can,fill = NA) +
  geom_sf(data = ak, fill = NA) + 
  geom_sf(data =river_sf) + 
  theme_void()  

#3######## try using tutorial-- https://milospopovic.net/map-rivers-with-sf-and-ggplot2-in-r/
get_data <- function() {
  
  url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_na_shp.zip"
  
  res <- GET(url,
             write_disk("eu_rivers.zip"),
             progress())
  unzip("eu_rivers.zip")
  filenames <- list.files("HydroRIVERS_v10_eu_shp", pattern="*.shp", full.names=T)
  
  riv_list <- lapply(filenames, st_read)
  
  return(riv_list)
}

load_rivers <- function() {
  list_riv <- lapply(filenames, sf::st_read)
  eu_riv <- list_riv[[1]] |>
    sf::st_cast("MULTILINESTRING")
  
  return(eu_riv)
}

eu_riv <- load_rivers()
