library(lme4)
library(tidyverse)
library(insight)
library(sjPlot)
library(sf)
library(tmap)

data <- read_csv('https://raw.githubusercontent.com/lucasangio01/vienna-airbnb/refs/heads/main/data/vienna_listings_no_outliers.csv')

vienna_shapefile <- st_read("/Users/tommipremoli8/Desktop/Progetti/ams-exam/data/Vienna_Districts Shape/BEZIRKSGRENZEOGDPolygon.shp")
vienna_shapefile$NAMEK <- iconv(vienna_shapefile$NAMEK, from = "latin1", to = "UTF-8")

airbnb_data_sf <- st_as_sf(
  data,
  coords = c("longitude", "latitude"),
  crs = 4326  
)

airbnb_data_sf <- st_transform(airbnb_data_sf, st_crs(vienna_shapefile))

bbox_airbnb <- st_bbox(airbnb_data_sf)
xlim <- c(bbox_airbnb["xmin"], bbox_airbnb["xmax"])
ylim <- c(bbox_airbnb["ymin"], bbox_airbnb["ymax"])

# Prices
ggplot(vienna_shapefile) +
  geom_sf(data = vienna_shapefile, fill = "white", color = "grey") + 
  geom_sf(data = airbnb_data_sf, aes(color = price_dollars), size = 1.2) +  
  scale_color_gradient(
    name = "Price ($)",
    low = "white",
    high = "darkred"  
  ) +
  theme_minimal() +
  labs(
    title = "Airbnb Prices in Vienna",
  ) +
  theme(legend.position = "right") +
  geom_sf_label(aes(label = NAMEK), size = 2.5, color = "black", nudge_y = 0.5, nudge_x = 1) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE)

# Room Type
ggplot(vienna_shapefile) +
  geom_sf(data = vienna_shapefile, fill = "white", color = "grey") + 
  geom_sf(data = airbnb_data_sf, aes(color = room_type), size = 0.7) +  
  scale_color_manual(
    name = "Room Type",
    values = c("Entire home/apt" = "cornflowerblue", "Private room" = "coral"), 
    labels = c("Entire home/apt", "Private room")
  ) +
  theme_minimal() +
  labs(
    title = "Airbnb Room Types in Vienna"
  ) +
  theme(legend.position = "right") +
  geom_sf_label(aes(label = NAMEK), size = 2.5, color = "black", nudge_y = 0.5, nudge_x = 1) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE)

# Average Accomodates
mean_accomodates_per_neighbourhood <- data %>%
  group_by(neighbourhood) %>%
  summarise(mean_accomodatese = mean(accomodates, na.rm = TRUE))

vienna_shapefile <- vienna_shapefile %>%
  left_join(mean_accomodates_per_neighbourhood, by = c("NAMEK" = "neighbourhood"))

ggplot(data = vienna_shapefile) +
  geom_sf(aes(fill = mean_accomodatese), color = "grey") + 
  scale_fill_gradient(
    name = "Avg Accomodates",
    low = "#faebd7",  
    high = "darkred", 
    na.value = "white"
  ) +
  theme_minimal() +
  labs(
    title = "Average Airbnb Accomodates by Neighbourhood in Vienna"
  ) +
  theme(legend.position = "right") +
  geom_sf_text(aes(label = NAMEK), size = 3, color = "black", ,nudge_y = 0.5, nudge_x = 1) 

# Average Age
mean_apt_age_days_per_neighbourhood <- data %>%
  group_by(neighbourhood) %>%
  summarise(mean_apt_age_days = mean(apt_age_days, na.rm = TRUE))

vienna_shapefile <- vienna_shapefile %>%
  left_join(mean_apt_age_days_per_neighbourhood, by = c("NAMEK" = "neighbourhood"))

ggplot(data = vienna_shapefile) +
  geom_sf(aes(fill = mean_apt_age_days), color = "grey") + 
  scale_fill_gradient(
    name = "Avg Age",
    low = "#faebd7",  
    high = "darkred", 
    na.value = "white"
  ) +
  theme_minimal() +
  labs(
    title = "Average Airbnb Age by Neighbourhood in Vienna"
  ) +
  theme(legend.position = "right") +
  geom_sf_text(aes(label = NAMEK), size = 3, color = "black", ,nudge_y = 0.5, nudge_x = 1) 
