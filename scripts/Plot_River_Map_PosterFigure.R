
# copied from here: https://github.com/milos-agathon/map-rivers-with-sf-and-ggplot2-in-R/blob/main/R/rivers_america.r
# AYK MAP THAT SHOWS WEIR PROJECTS, USED IN POSTERS


library("httr")
library("tidyverse")
library("sf")
library("here")
library("raster")

sf_use_s2(FALSE)

# Load shape file with yukon watershed
# found here: https://www.sciencebase.gov/catalog/item/5813d2b2e4b0bb36a4c29e31
yukon_poly<- sf::st_read("data/11573/11573.shp") %>%
  st_make_valid()

# GET RIVERS DATA ============
project <- read.csv("data/project_lat_lon.csv") 
fall <- project %>% 
  filter(Season == "Fall")

summ_fall <- project %>% 
  filter(Season == "Summer/Fall")

summer <- project %>% 
  filter(Season == "Summer")

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

# make mask ========
# trim rivers to just be inside AK and part of canada
source("scripts/Function_AKMap.R")
na_outline <- st_union(ak,can)

# use this to make a alpha filter that highlights the yukon watershed
plot(yukon_poly)
plot(bbox)

ggplot() +
    geom_sf(data = yukon_poly ) +
    geom_sf(data = bbox, alpha = 0.5)

mask <-sf::st_difference(bbox,yukon_poly) 
mask2 <-st_difference(mask,na_outline) 
plot(mask)
plot(mask2)

both <- st_difference(mask, mask2)

plot(both) 
 
# make plot with points  ========
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
    # geom_point(data = summ_fall, aes(x=Lon, y = Lat), color= "darkgreen", size = 1 ) +
    geom_point(data = fall, aes(x=Lon, y = Lat), color= "darkgreen", size = 1 ) +
    geom_point(data = summer, aes(x=Lon, y = Lat), color= "orange", size = 1 ) +
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

 

ggsave(
  filename = "output/ak_rivers.png",
  width = 9, height = 6, dpi = 600,
  bg = "transparent", device = "png", p
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

