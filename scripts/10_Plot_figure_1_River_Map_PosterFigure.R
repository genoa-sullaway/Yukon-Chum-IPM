
# copied from here: https://github.com/milos-agathon/map-rivers-with-sf-and-ggplot2-in-R/blob/main/R/rivers_america.r
# AYK MAP THAT SHOWS WEIR PROJECTS, USED IN POSTERS

library(httr)
library(tidyverse)
library(sf)
library(here)
library(raster)
library(concaveman)
library(akgfmaps)
library(readxl)
library(ggspatial)

# devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
 # test <- akgfmaps::get_nmfs_areas(set.crs = "auto")
 # plot(test)

# load BASIS survey grid =========
# Curry sent this data June 26 2025. 
survey <- read_xlsx("data/Sabrina_J_Chum_10.16.xlsx",sheet = "Event") %>%
  # filter(CommonName == "Chum Salmon") %>%
  dplyr::rename(Lat = "EQ Latitude",
                Lon = "EQ Longitude") %>%
  filter(!is.na(Lat),
         !is.na(Lon),
         !(Lon < -170 & Lat < 55),
         !Lon < -174)
         # Lon > -170 & Lat > 55)

NBS_survey_grid <- data.frame(unique(survey[c("Lat", "Lon")]))

# fullnessdf <- read_xlsx("data/NBS_JChumFullness.xlsx") %>% 
#   filter(!is.na(Number_of_Stomachs)) %>% 
#   dplyr::rename(fullness =`Stomach_fullness_index(o/ooo)`,
#                 Lat = `EQ Latitude`,
#                 Lon = `EQ Longitude`,
#                 stomach_weight = `Stomach_Weight(g)`) %>%
#   dplyr::mutate( 
#     SampleYear_factor = as.factor(SampleYear),
#     GearCode = as.factor(GearCode)) %>% 
#   filter(!Lat>65) 

 # NBS_survey_grid <- data.frame(unique(fullnessdf[c("Lat", "Lon")]))
  
#  points_sf <- st_as_sf(NBS_survey_grid, coords = c("Lon", "Lat"), crs = 4326)
#  
#  points_projected <- st_transform(points_sf, crs = 32603)
#  
#  # Buffer in meters (since UTM is in meters)
#  buffered_points <- st_buffer(points_projected, dist = 30 * 1000)
#  
#  # Transform back to WGS84 (lat/long)
#  buffered_points_geo <- st_transform(buffered_points, crs = 4326)
#  
#  # Extract coordinates from the buffer boundary
#  coords <- st_coordinates(st_boundary(buffered_points_geo))
#  
#  # Create a data frame with the coordinates
#  points_df <- data.frame(
#    x = coords[, "X"],
#    y = coords[, "Y"]
#  )
#  
#  # Create an sf object that concaveman can use
#  points_for_hull <- st_as_sf(points_df, coords = c("x", "y"), crs = 4326)
#  
#  # Create concave hull
#  polygon_NBS <- concaveman::concaveman(points_for_hull,
#                                    concavity = 2,
#                                    length_threshold = 0)
# 
# plot(polygon_NBS)
 

# load other data ==== 
sf_use_s2(FALSE)

# Load shape file with yukon watershed
# found here: https://www.sciencebase.gov/catalog/item/5813d2b2e4b0bb36a4c29e31
yukon_poly<- sf::st_read("data/11573/11573.shp") %>%
  st_make_valid()

# weir projects ============
project <- read.csv("data/project_lat_lon.csv") %>%
  filter(River == "Yukon")

fall_df <- project %>% 
  filter(Season == "Fall")#,
         # !Project.Name == "Pilot Station Sonar")

summ_fall <- project %>% 
  filter(Project.Name == "Pilot Station Sonar") %>%
  mutate(value = 0.5)

summer <- project %>% 
  filter(Season == "Summer")#, 
         # !Project.Name == "Pilot Station Sonar")

# GET RIVERS DATA ============
ar_riv <- sf::st_read("HydroRIVERS_v10_ar_shp/HydroRIVERS_v10_ar_shp/HydroRIVERS_v10_ar.shp") %>%
  st_cast("MULTILINESTRING")

ariv <- ar_riv %>%
  mutate(
    width = as.numeric(ORD_FLOW),
    width = case_when(
      width == 3 ~ 0.8,
      width == 4 ~ 0.6,
      width == 5 ~ 0.45,
      width == 6 ~ 0.35,
      width == 7 ~ 0.25,
      width == 8 ~ 0.15,
      width == 9 ~ 0.1,
      width == 10 ~ 0.1,
      TRUE ~ 0
    )
  ) %>%
  st_as_sf()

ariv$geometry <- ariv$geometry %>%
  s2::s2_rebuild() %>%
  sf::st_as_sfc()

crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"
 
bbox <- st_sfc(
  st_polygon(list(cbind(
    c(-168, -130, -130, -168, -168),
    c(52, 52, 80, 70, 52)
  ))),
  crs = crsLONGLAT
)

new_prj <- sf::st_transform(bbox, crs = crsLONGLAT)
bb <- sf::st_bbox(new_prj)

## make white box to cover russia =======
russia_box <- st_sfc(
  st_polygon(list(cbind(
    c(-168.5, -176, -176, -168.5, -168.5),
    c(64, 64, 67.5, 67.5, 64)
  ))),
  crs = crsLONGLAT
)

canada_box <- st_sfc(
  st_polygon(list(cbind(
    c(-125, -130.5, -130.5, -125, -125),
    c(55, 55, 72, 72, 55)
  ))),
  crs = crsLONGLAT
)

lower_box <- st_sfc(
  st_polygon(list(cbind(
    c(-135, -130.5, -130.5, -135, -135),
    c(55, 55, 59, 59, 55)
  ))),
  crs = crsLONGLAT
)

# make mask ========
# trim rivers to just be inside AK and part of canada
source("scripts/Function_AKMap.R")
na_outline <- st_union(ak,can)

# use this to make a alpha filter that highlights the yukon watershed
plot(yukon_poly)
plot(bbox)
# 
# ggplot() +
#     geom_sf(data = yukon_poly ) +
#     geom_sf(data = bbox, alpha = 0.5)

mask <-sf::st_difference(bbox,yukon_poly) 
mask2 <-st_difference(mask,na_outline) 
# plot(mask)
# plot(mask2)

both <- st_difference(mask, mask2)

# plot(both) 

# # make plot with points  ========
# p <- ggplot() +
#   geom_sf(data = na_outline, fill = "white") +
#   geom_sf(data = ariv, aes(
#     color = factor(ORD_FLOW), size = width, alpha = width 
#   )) +
#   geom_sf(data = both, alpha = 0.7, color = "black" ) + 
#    scale_color_manual(
#     name = "",
#     values = c(
#       "#08306b", "#1c4680", "#305d94", "#4574a7",
#       "#5d8cb9", "#77a4cb", "#deebf7", "#deebf7", "#deebf7"), drop = F ) +
#     # geom_point(data = summ_fall, aes(x=Lon, y = Lat), color= "darkgreen", size = 1 ) +
#     geom_point(data = fall, aes(x=Lon, y = Lat), color= "darkgreen", size = 2 ) +
#     geom_point(data = summer, aes(x=Lon, y = Lat), color= "orange", size = 2 ) +
#     geom_point(aes(x= -162.88, y = 61.9), color= "black", size = 2, shape = 17 ) +
#   # geom_col(data = summ_fall,aes(x=Lon, y = Lat, fill = value )) +
#   # scale_fill_manual(values = c("orange", "darkgreen")) +
#   # coord_polar(theta = "y") +  
#   coord_sf(
#     crs = crsLONGLAT,
#     xlim = c(bb["xmin"], bb["xmax"]),
#     ylim = c(bb["ymin"], bb["ymax"])
#   ) +
#   labs(
#     y = "", subtitle = "",
#     x = "" ) +
#   scale_alpha(range = c(0, 1)) +
#   scale_size(range = c(0, .45)) +
#   theme_minimal() +
#   theme(
#     text = element_text(family = "Montserrat"),
#     panel.background = element_blank(),
#     legend.background = element_blank(),
#     legend.position = "none",
#     panel.border = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.grid.major = element_blank(),
#     axis.title.y = element_blank(),
#     axis.ticks = element_blank(),
#     axis.text.x = element_blank(),
#     axis.text.y = element_blank()
#   )
# 
#  
# 
# ggsave(
#   filename = "output/ak_rivers.png",
#   width = 9, height = 6, dpi = 600,
#   bg = "transparent", device = "png", p
# )

## manuscript figure  ===============
# just fall, a little more detail o nthe plot 

fall_plot <- ggplot() +
    geom_sf(data = na_outline, fill = "white") +
  geom_sf(data = ariv, aes(
    color = factor(ORD_FLOW), size = width, alpha = width 
  )) +
  geom_sf(data = both, alpha = 0.7, color = "black" ) + 
  scale_color_manual(
    name = "",
    values = c(
      "#08306b", "#1c4680", "#305d94", "#4574a7",
      "#5d8cb9", "#77a4cb", "#deebf7", "#deebf7", "#deebf7"), drop = F ) +
  geom_point(data = fall_df, aes(x=Lon, y = Lat), color= "darkgreen", size = 3) +
  geom_point(aes(x= -162.4, y = 61.9), color= "purple", size = 3, shape = 17 ) + # pilot station 
  geom_sf(data = russia_box, fill = "white", color = "white") +  
  geom_sf(data = canada_box, fill = "white", color = "white") + 
  # geom_sf(data = lower_box, fill = "white", color = "white") + 
 # geom_sf(data = polygon_NBS, fill = NA, color = "black") + 
  geom_point(data = NBS_survey_grid, aes(x = Lon, y= Lat), size = 1) + 
  geom_point(aes(x= -144.0657, y= 65.8252), color = "orange", size = 3, shape = 18) + # plot Circle lat long (source of snow data)
   coord_sf(
    crs = crsLONGLAT,
    xlim = c(-173,#bb["xmin"], 
             -130.5),#bb["xmax"]),
    ylim = c(bb["ymin"], 71.5)# bb["ymax"])
  ) +
  labs(
    y = "Latitude", subtitle = "",
    x = "Longitude" ) +
  scale_alpha(range = c(0, 1)) +
  scale_size(range = c(0, .45)) +
  theme_classic() + 
  theme(
    text = element_text(family = "Montserrat"),
    # panel.background = element_blank(),
    legend.background = element_blank(),
    legend.position = "none",
    panel.background = element_rect(fill = "white"),  # Makes the plot area blue
    plot.background = element_rect(fill = "white"),       # Keeps the outer margin white
    panel.grid = element_blank(), 
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  annotation_scale(
    location = "br",
    width_hint = 0.25,
    style = "bar",
    pad_x = unit(0.4, "cm"),
    pad_y = unit(0.3, "cm")
  ) +
  annotation_north_arrow(
    location = "br",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.6, "cm"),  
    height = unit(0.9, "cm"),
    width = unit(0.9, "cm"),
    which_north = "true",
    style = north_arrow_fancy_orienteering
  )


# save manuscript figure ========
ggsave(
  filename = "output/ak_rivers_fall_weirs.png",
  width = 9, height = 6, dpi = 600,
  # bg = "white", 
  device = "png", fall_plot
)

# stop here for manuscript figure =======
# keep going for alt map figures ========
  
## just fall fish with lat longs ===============
fall <- ggplot() +
  geom_sf(data = na_outline, fill = "white") +
  geom_sf(data = ariv, aes(
    color = factor(ORD_FLOW), size = width, alpha = width 
  )) +
  geom_sf(data = both, alpha = 0.7, color = "black" ) + 
  scale_color_manual(
    name = "",
    values = c(
      "#08306b", "#1c4680", "#305d94", "#4574a7",
      "#5d8cb9", "#77a4cb", "#deebf7", "#deebf7", "#deebf7"), drop = F ) +
  # geom_point(data = summ_fall, aes(x=Lon, y = Lat), color= "darkgreen", size = 1 ) +
  geom_point(data = fall, aes(x=Lon, y = Lat), color= "darkgreen", size = 2 ) +
  geom_point(aes(x= -162.87, y = 61.9), color= "black", size = 2, shape = 17 ) + # pilot station 
  coord_sf(
    crs = crsLONGLAT,
    xlim = c(bb["xmin"], bb["xmax"]),
    ylim = c(bb["ymin"], bb["ymax"])
  ) +
  labs(
    y = "", subtitle = "",
    x = "" ) +
  scale_alpha(range = c(0, 1)) +
  scale_size(range = c(0, .45)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Montserrat"),
    panel.background = element_blank(),
    legend.background = element_blank(),
    legend.position = "none",
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )
 

# make plot with no points ========
p <-ggplot() +
  geom_sf(data = na_outline, fill = "white") +
  geom_sf(data = ariv, aes(
    color = factor(ORD_FLOW), size = width, alpha = width 
  )) +
  geom_sf(data = both, alpha = 0.7, color = "black" ) + 
 
  scale_color_manual(
    name = "",
    values = c(
      "#08306b", "#1c4680", "#305d94", "#4574a7",
      "#5d8cb9", "#77a4cb", "#deebf7", "#deebf7", "#deebf7"), drop = F ) +
  coord_sf(
    crs = crsLONGLAT,
    xlim = c(bb["xmin"], bb["xmax"]),
    ylim = c(bb["ymin"], bb["ymax"])
  ) +
  labs(
    y = "", subtitle = "",
    x = "" ) +
  scale_alpha(range = c(0, 1)) +
  scale_size(range = c(0, .45)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Montserrat"),
    panel.background = element_blank(), #element_rect(fill = "black", colour = NA),
    plot.background = element_blank(), #element_rect(fill = "black", colour = NA),
    legend.background = element_blank(),
    legend.position = "none",
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    rect = element_rect(fill = "transparent")
  )
 
ggsave(
  filename = "output/ak_rivers_nopoints.png",
  width = 9, height = 6, dpi = 600,
  bg = "transparent", device = "png", p
)


# GOA ======================
## make box for GOA rivers =======
goa_bbox <- st_sfc(
  st_polygon(list(cbind(
    c(-175, -130, -130, -175, -175),
    c(52, 52, 65, 65, 52)  #(-175,-130,52,65)
  ))),
  crs = crsLONGLAT
)

new_prj_goa <- sf::st_transform(goa_bbox, crs = crsLONGLAT)
bb_goa <- sf::st_bbox(new_prj_goa)

##  base map GOA ============
can <- sf::st_as_sf(maps::map('world','Canada', 
                              plot=FALSE, fill=TRUE)) %>%
  st_transform(4326)

#create bounds to trim CAN map 
bounds_can <- extent(-175,-130,52,65)
#bounds_ak <- extent(-164.5,-160, 55,60) 
extent_can <- st_set_crs(st_as_sf(as(bounds_can, "SpatialPolygons")), 4326)
can <- st_intersection(can, extent_can) #trim map by intersections 

plot(can)

ak <- sf::st_as_sf(maps::map('world','USA:Alaska', 
                             plot=FALSE, fill=TRUE)) %>%
  st_transform(4326)

#create bounds to trim AK map 
bounds_ak <- extent(-175,-130,52,65)
extent_ak <- st_set_crs(st_as_sf(as(bounds_ak, "SpatialPolygons")), 4326)
ak_map <- st_intersection(ak, extent_ak) #trim map by intersections 
plot(ak_map)

goa_na_outline <- st_union(ak_map,can)
plot(goa_na_outline)

# plot GOA ===== 

p_goa <- ggplot() +
  geom_sf(data = goa_na_outline, fill = "white") +
  geom_sf(data = ariv, aes(
    color = factor(ORD_FLOW), size = width, alpha = width 
  )) +
  # geom_sf(data = both, alpha = 0.7, color = "black" ) + 
  scale_color_manual(
    name = "",
    values = c(
      "#08306b", "#1c4680", "#305d94", "#4574a7",
      "#5d8cb9", "#77a4cb", "#deebf7", "#deebf7", "#deebf7"), drop = F ) +
  coord_sf(
    crs = crsLONGLAT,
    xlim = c(bb_goa["xmin"], bb_goa["xmax"]),
    ylim = c(bb_goa["ymin"], bb_goa["ymax"])
  ) +
  labs(
    y = "", subtitle = "",
    x = "" ) +
  scale_alpha(range = c(0, 1)) +
  scale_size(range = c(0, .45)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Montserrat"),
    panel.background = element_blank(), #element_rect(fill = "black", colour = NA),
    plot.background = element_blank(), #element_rect(fill = "black", colour = NA),
    legend.background = element_blank(),
    legend.position = "none",
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    rect = element_rect(fill = "transparent")
  )

ggsave(
  filename = "output/ak_rivers_GOA.png",
  width = 9, height = 6, dpi = 600,
  bg = "transparent", device = "png", p_goa
)

