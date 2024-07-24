#map funciton

#mapping plotting/packages 
library(maps)  
library(raster)  
library(sf)
library(ggpubr) # for ggarrange 
library(viridis) 
 
can <- sf::st_as_sf(maps::map('world','Canada', 
                             plot=FALSE, fill=TRUE)) %>%
  st_transform(4326)

#create bounds to trim CAN map 
bounds_can <- extent(-156,-130,55,70)
#bounds_ak <- extent(-164.5,-160, 55,60) 
extent_can <- st_set_crs(st_as_sf(as(bounds_can, "SpatialPolygons")), 4326)
can <- st_intersection(can, extent_can) #trim map by intersections 

 plot(can)

#set up maps
ak <- sf::st_as_sf(maps::map('world','USA:Alaska', 
                             plot=FALSE, fill=TRUE)) %>%
  st_transform(4326)

#create bounds to trim AK map 
bounds_ak <- extent(-168,-140,52,73)
#bounds_ak <- extent(-164.5,-160, 55,60) 
extent_ak <- st_set_crs(st_as_sf(as(bounds_ak, "SpatialPolygons")), 4326)
ak <- st_intersection(ak, extent_ak) #trim map by intersections 
  plot(ak)
