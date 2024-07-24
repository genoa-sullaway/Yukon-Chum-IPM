library(sf)
library(here)

major_tribs <- sf::st_read("data/Map/Chinook_BConnorsDFO/yukonmajortribs.shp") 
plot(major_tribs)

major_watersheds <- sf::st_read("data/Map/Chinook_BConnorsDFO/majorwatersheds.shp") 
plot(major_watersheds)

major_watersheds_sub <- sf::st_read("data/Map/Chinook_BConnorsDFO/majorwatersheds_subdrainages.shp") 
plot(major_watersheds_sub)


yukon_ck <- sf::st_read("data/Map/Chinook_BConnorsDFO/yukon_ck_cus.shp") 
plot(yukon_ck)

test<- sf::st_read("data/11573/11573.shp")
plot(test)
# base - whole US better to use my base. 
# base1 <- sf::st_read("data/Map/Chinook_BConnorsDFO/baselayers/cb_2013_us_nation_500k.shp") 
# plot(base1)
 
hydro <- sf::st_read("data/Map/Chinook_BConnorsDFO/baselayers/hydrography_l_rivers_v2.shp")

# base map
source("scripts/Function_AKMap.R")

na_outline <- rbind(ak,can)

# full plot
# p <-
  ggplot() +
    geom_sf(data = na_outline, fill = NA) +
    # geom_sf(data = major_watersheds_sub ) +
    geom_sf(data = test, alpha = 0.5) 
  
  
  
  
   geom_sf(data = ariv, aes(
    color = factor(ORD_FLOW), size = width, alpha = width
  )) +
  scale_color_manual(
    name = "",
    values = c(
      "#08306b", "#1c4680", "#305d94", "#4574a7",
      "#5d8cb9", "#77a4cb", "#deebf7", "#deebf7", "#deebf7"), drop = F ) +
  geom_point(data = summ_fall, aes(x=Lon, y = Lat), color= "darkgreen", size = 1 ) +
  geom_point(data = fall, aes(x=Lon, y = Lat), color= "yellow", size = 1 ) +
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
