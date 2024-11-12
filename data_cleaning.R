library(dplyr)
library(tidyverse)
library(tibble)
library(osmdata)
library(sf)


data <- read.csv("Data/vienna_listings.csv")

data <- data %>% select(id, host_id, host_acceptance_rate, host_is_superhost, host_listings_count, neighbourhood_cleansed, latitude, longitude, room_type, accommodates, bathrooms, beds, amenities, price, number_of_reviews, first_review, review_scores_rating:review_scores_value, reviews_per_month)
data <- data %>% select(id, host_id, price, latitude, longitude, neighbourhood_cleansed, room_type, accommodates, bathrooms, beds, amenities, host_acceptance_rate, host_is_superhost, host_listings_count, number_of_reviews:reviews_per_month)
data <- subset(data, bathrooms > 0)

data$bathrooms <- ceiling(data$bathrooms)
data$id <- as.integer(data$id)
data$bathrooms <- as.numeric(data$bathrooms)
data$host_acceptance_rate <- substr(data$host_acceptance_rate, 1, nchar(data$host_acceptance_rate)-1) %>%
  as.numeric(data$host_acceptance_rate)
data$host_acceptance_rate <- data$host_acceptance_rate / 100.0
data$price <- as.numeric(gsub("\\$", "", data$price))
data$first_review <- as.Date(data$first_review, format = "%Y-%m-%d")
data$host_is_superhost <- as.factor(data$host_is_superhost)

colnames(data)[colnames(data) == 'price'] <- 'price_dollars'
colnames(data)[colnames(data) == 'neighbourhood_cleansed'] <- 'neighbourhood'

data$upload_date <- "2024-09-12"
data$apt_age_days <- ceiling(as.numeric(difftime(data$upload_date , data$first_review , units = c("days"))))

data$ID <- 1:nrow(data)
data <- data %>% select(ID, everything())
data <- data %>% select(-id)

data <- drop_na(data)

schonbrunn <- opq("Vienna") %>%
  add_osm_feature(key = "name", value = "Schönbrunn") %>%
  osmdata_sf()
stephansdom <- opq("Vienna") %>%
  add_osm_feature(key = "name", value = "Stephansdom") %>%
  osmdata_sf()
train_station <- opq("Vienna") %>%
  add_osm_feature(key = "name", value = "Hauptbahnhof") %>%
  osmdata_sf()

schonbrunn_coords <- st_coordinates(schonbrunn$osm_points)[1, ]
stephansdom_coords <- st_coordinates(stephansdom$osm_points)[1, ]
train_station_coords <- st_coordinates(train_station$osm_points)[1, ]

data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
schonbrunn_point <- st_sfc(st_point(schonbrunn_coords), crs = 4326)
stephansdom_point <- st_sfc(st_point(stephansdom_coords), crs = 4326)
train_station_point <- st_sfc(st_point(train_station_coords), crs = 4326)

data$dist_schonbrunn <- st_distance(data_sf, schonbrunn_point)
data$dist_stephansdom <- st_distance(data_sf, stephansdom_point)
data$dist_train_station <- st_distance(data_sf, train_station_point)

data$neighbourhood <- gsub("Rudolfsheim-Fnfhaus", "Rudolfsheim-Fünfhaus", data$neighbourhood)
data$neighbourhood <- gsub("Landstra§e", "Landstraße", data$neighbourhood)
data$neighbourhood <- gsub("Whring", "Währing", data$neighbourhood)
data$neighbourhood <- gsub("Dbling", "Döbling", data$neighbourhood)

data$dist_schonbrunn_km <- round((as.numeric(st_distance(data_sf, schonbrunn_point)) / 1000), 2)
data$dist_stephansdom_km <- round((as.numeric(st_distance(data_sf, stephansdom_point)) / 1000), 2)
data$dist_train_station_km <- round((as.numeric(st_distance(data_sf, train_station_point)) / 1000), 2)

data <- data %>% select(ID, host_id, price_dollars, latitude, longitude, dist_stephansdom_km, dist_schonbrunn_km, dist_train_station_km, neighbourhood, room_type, accommodates, bathrooms, beds, amenities, host_acceptance_rate, host_is_superhost, host_listings_count, number_of_reviews, apt_age_days, review_scores_rating:reviews_per_month)

View(data)

# Ameneties Selection Part
# Step 1: Count specific amenities
amenities_check <- c(
  "Cleaning available during stay" = "Cleaning available during stay",
  "Air Conditioning" = "Portable air conditioning|Air conditioning|Central air conditioning",
  "Self check-in" = "Self check-in",
  "Wi-Fi" = "Wi-Fi|Wifi|Wireless internet"
)

# Calculate the count for each amenity
results <- sapply(amenities_check, function(pattern) {
  sum(str_detect(data$amenities, regex(pattern, ignore_case = TRUE)), na.rm = TRUE)
})

# Convert results to a data frame and display counts
results_df <- data.frame(Amenity = names(results), Count = as.integer(results))
print("Amenities Count Summary:")
print(results_df)

# Step 2: Add new columns to indicate the presence of each amenity
data$`Cleaning available during stay` <- ifelse(str_detect(data$amenities, regex("Cleaning available during stay", ignore_case = TRUE)), 1, 0)
data$`Air Conditioning` <- ifelse(str_detect(data$amenities, regex("Portable air conditioning|Air conditioning|Central air conditioning", ignore_case = TRUE)), 1, 0)
data$`Self check-in` <- ifelse(str_detect(data$amenities, regex("Self check-in", ignore_case = TRUE)), 1, 0)
data$WiFi <- ifelse(str_detect(data$amenities, regex("Wi-Fi|Wifi|Wireless internet", ignore_case = TRUE)), 1, 0)

# Step 3: Double-check for discrepancies between new columns and amenities text
discrepancies <- list(
  Cleaning = nrow(data %>% filter(`Cleaning available during stay` == 0 & str_detect(amenities, regex("Cleaning available during stay", ignore_case = TRUE)))),
  AirConditioning = nrow(data %>% filter(`Air Conditioning` == 0 & str_detect(amenities, regex("Portable air conditioning|Air conditioning|Central air conditioning", ignore_case = TRUE)))),
  SelfCheckIn = nrow(data %>% filter(`Self check-in` == 0 & str_detect(amenities, regex("Self check-in", ignore_case = TRUE)))),
  WiFi = nrow(data %>% filter(WiFi == 0 & str_detect(amenities, regex("Wi-Fi|Wifi|Wireless internet", ignore_case = TRUE))))
)

# Print discrepancies (if any)
print("Discrepancies Summary:")
print(discrepancies)
