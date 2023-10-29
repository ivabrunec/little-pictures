#### Arctic ice extent by year ####

library(dplyr)
library(ggplot2)
library(showtext)

font_add_google(name = "Inter", family = "Inter", regular.wt = 500)
font_add_google(name = "DM Sans", family = "Montserrat", regular.wt = 500)
showtext_auto(enable = T)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
# from: https://github.com/littlepictures/datasets/tree/main/arcticSeaIceExtent
arctic_data <- read.csv('arcticSeaIceExtent.csv')

# summarize every decade to make plotting cleaner
arctic_sum <- arctic_data |>
  mutate(decade = 10 * (year %/% 10)) |>
  # add a plotting column so 2020-2022 get included (?)
  mutate(
    decade_group = ifelse(year > 2019, 2010, decade)
  ) |>
  group_by(decade) |>
  summarize(mean_extent = mean(arcticSeaIceExtent))  

## set up function to plot rounded polygons representing arctic ice sheet ####
# very minor adaptation to this convex polygon code:
# https://stackoverflow.com/questions/4859482/how-to-generate-random-shapes-given-a-specified-area-r-language

convex.poly <- function(nSides, area)
{
  # Find the radius of the circumscribed circle, and the angle of each point if this was a regular polygon
  radius <- sqrt((2*area)/(nSides*sin((2*pi)/nSides)))
  angle <- (2*pi)/nSides
  
  # Randomize the radii/angles
  radii <- rnorm(nSides, radius, radius/20)
  angles <- rnorm(nSides, angle, angle/20) * 1:nSides
  angles <- sort(angles)
  
  points <- list(x=NULL, y=NULL)
  points$x <- cos(angles) * radii
  points$y <- sin(angles) * radii
  
  # Find the area of the polygon
  m <- matrix(unlist(points), ncol=2)
  m <- rbind(m, m[1,])
  current.area <- 0.5 * (sum(m[1:nSides,1]*m[2:(nSides+1),2]) - sum(m[1:nSides,2]*m[2:(nSides+1),1]))
  
  points$x <- points$x * sqrt(area/current.area)
  points$y <- points$y * sqrt(area/current.area)
  
  return (points)
}


#### loop over years & store coordinates for each year ####

# initialize polygon list
polygon_list = list()
y_height = 0

for (year_iter in 1:nrow(arctic_sum)){
  cur_extent <- arctic_sum$mean_extent[year_iter]
  point_set <- convex.poly(40, cur_extent)
  poly_df <- data.frame(point_set)
  
  # flatten the y-axis to achieve visual perspective
  poly_df$y <- poly_df$y * .5
  
  # add iterative step to the y-axis to separate out the years
  poly_df$y <- poly_df$y + y_height
  
  poly_df$year <- arctic_sum$decade[year_iter]
  
  polygon_list[[year_iter]] <- poly_df
  
  # add a step to the y-height so next polygon is separated
  y_height = max(poly_df$y) + .85


}

polygon_df_all <- do.call(rbind, polygon_list)

#### once polygons generated, read in directly ####
polygon_df_all <- read.csv('arctic_sea_polygons.csv')
polygon_df_all$year <- as.factor(polygon_df_all$year)

## define color scale and add year labels ####
color_scale <- c( 
                  "#64c79f", 
                  "#76b5bb",
                  "#87a5d5", 
                  "#a3a6d6", 
                  "#c5b7c3")
bg_color <- '#18291e'

# reverse y-axis so it goes top to bottom to illustrate the shrinkage
polygon_df_all$y_rev <- polygon_df_all$y * (-1)

# get mean y-axis of each year for alignment
year_coords <- polygon_df_all |>
  group_by(year) |>
  summarize(mean_y = mean(y_rev)) |>
  mutate(text_label = paste0(year, 's'))

# keep only latest and oldest data labels
year_coords_display <- filter(year_coords, text_label == '1980s' | text_label == '2020s')

ggplot() +
  geom_polygon(data = polygon_df_all, 
               aes(x = x, y = y_rev, fill = year, group = year),
               color = bg_color, linewidth = .5) +
  geom_text(data = year_coords_display,
             aes(x = -2, y = mean_y, label = text_label),
             color = '#F2edd5',
            family = 'Montserrat', size = 24) + 
  xlim(-2.15,2.15) +
  scale_fill_manual(values = color_scale) +
  theme_void() +
  theme(legend.position = '',
        plot.background = element_rect(color = NA, fill = bg_color),
        plot.margin = margin(t = 50,  
                             r = 10,  
                             b = 50,  
                             l = 10))

ggsave('ArcticSeaIce.png', height = 6, width = 5, dpi = 400)

