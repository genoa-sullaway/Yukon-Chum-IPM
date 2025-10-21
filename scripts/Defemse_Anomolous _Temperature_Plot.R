library(here)
library(lubridate)
library(viridis)
library(tidyverse)
 
sst <- read_csv("data/BS-SST-2025-09-15.csv")

head(sst)

# Change this line to highlight different years
highlight_years <- c(#2002, 2004, 
                     2014,2015, 2016,2017,2019,2020,2021,2022,2023,2024,2025)

# Prepare the data
sst_processed <- sst %>%
  dplyr::mutate(
    year = year(date),
    day_of_year = yday(date),
    # Create a reference mean (you can adjust the years as needed)
    is_reference_period = year >= 1985 & year <= 2025,
    year_group = ifelse(year %in% highlight_years, "highlight", "background"),
    color_var = ifelse(year %in% highlight_years, as.character(year), NA)
  )

# Calculate long-term mean for reference period
reference_mean <- sst_processed %>%
  filter(is_reference_period) %>%
  group_by(Ecosystem_sub, day_of_year) %>%
  dplyr::summarise(mean_sst = mean(meansst, na.rm = TRUE), .groups = 'drop')

# Split data into highlighted and background years
background_data <- sst_processed %>% filter(year_group == "background")
highlight_data <- sst_processed %>% filter(year_group == "highlight")

custom_color <- c(
  "#FFF7CC", # pale cream yellow
  "#FFEE99", # light yellow
  "#FFE066", # bright yellow
  "#FFC94C", # golden yellow-orange
  "#FFAD33", # light orange
  "#FF9133", # medium orange
  "#FF704D", # coral orange
  "#FC4E2A", # Red-orange
  "#FF5C73", # pinkish coral
  "#FF66B2", # warm pink
  "#FFA6FF"  # soft magenta highlight
)

create_chronological_colors <- function() {
  # Sort highlight years chronologically
  sorted_years <- sort(highlight_years)
  # Create a gradient across the sorted years
  n_colors <- length(sorted_years)
  colors <- custom_color #RColorBrewer::brewer.pal(n=n_colors, name="YlOrRd")
   #scale_color_brewer(name = "Year", palette = "YlOrRd", na.translate = FALSE)
   #viridis::plasma(n_colors)
  names(colors) <- as.character(sorted_years)
  return(colors)
}
 
  p <- ggplot() +
    geom_line(data = filter(sst_processed, year_group == "background"),
              aes(x = day_of_year, y = meansst, group = year),
              color = "gray70", alpha = 0.4, size = 0.3) +
    geom_line(data = filter(sst_processed, year_group == "highlight"),
              aes(x = day_of_year, y = meansst, group = year, color = color_var),
              alpha = 0.8, size = 0.5) +
    geom_line(data = reference_mean, 
              aes(x = day_of_year, y = mean_sst),
              color = "white", size = 0.8) +
    scale_color_manual(name = "Year", values = create_chronological_colors(), na.translate = FALSE) +
    facet_wrap(~ Ecosystem_sub, scales = "free_y",ncol =1) +
    theme_minimal() +
    theme(panel.background = element_blank(), #element_rect(fill = "black", colour = NA),
          plot.background = element_blank(), #element_rect(fill = "black", colour = NA),
          legend.background = element_blank(),
          legend.text = element_text(color = "white"),
          legend.title = element_blank(), 
          plot.title = element_text( hjust = 0.5, vjust = 0.5,size = 14, color = "white",
                                     face = "bold",margin = margin(t = 10, r = 10, b = 10, l = 10)),
          strip.text = element_blank( ), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(colour = "white", fill = NA), 
          strip.text.x = element_blank(), 
          axis.line = element_line(color = "white"), 
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,color = "white"),
          axis.text.y = element_text(color = "white"),
          axis.title.y = element_text(color = "white"),
          axis.title.x = element_text(color = "white"),
          axis.ticks.y = element_line(color = "white"),
          axis.ticks.x = element_line(color = "white"),
          panel.spacing.y=unit(0, "lines")) +
  
    # theme(
    #   panel.background = element_rect(fill = "white", color = NA), 
    #   panel.grid.major = element_blank(),
    #   panel.grid.minor = element_blank(),
    #   axis.line = element_line(color = "black", size = 0.5),
    #   plot.title = element_text( hjust = 0.5, vjust = 0.5,size = 14, face = "bold",margin = margin(t = 10, r = 10, b = 10, l = 10)),
    #     plot.background = element_rect(fill = "white", color = "black", size = 1)) +
    labs(
      x = "Day of Year",
      y = "Sea Surface Temperature (°C)"
    ) 
  p

  ggsave("Anomolous_Temp_Plot_defense.png", width = 5, height =6) 
