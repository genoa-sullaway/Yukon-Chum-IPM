#map funciton

#mapping plotting/packages 
library(maps)  
library(raster)  
library(sf)
library(ggpubr) # for ggarrange 
library(viridis) 

#set up maps
ak <- sf::st_as_sf(maps::map('world','USA:Alaska',
                             plot=FALSE, fill=TRUE))

#create bounds to trim AK map 
bounds_ak <- extent(-180,-156,60,75)
#bounds_ak <- extent(-164.5,-160, 55,60) 
extent_ak <- st_set_crs(st_as_sf(as(bounds_ak, "SpatialPolygons")), 4326)
ak <- st_intersection(ak, extent_ak) #trim map by intersections 

