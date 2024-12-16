library(tidyverse)
library(tibble)
library(osmdata)
library(sf)
library(MASS) 


data_original <- read_csv("https://raw.githubusercontent.com/lucasangio01/vienna-airbnb/refs/heads/main/data/vienna_listings.csv")

data <- data_original %>% dplyr::select(id, host_id, host_acceptance_rate, host_listings_count, neighbourhood_cleansed, latitude, longitude, room_type, accommodates, bathrooms, amenities, price, number_of_reviews, first_review, review_scores_rating:review_scores_value, reviews_per_month)
data <- data %>% dplyr::select(id, host_id, price, latitude, longitude, neighbourhood_cleansed, room_type, accommodates, bathrooms, amenities, host_acceptance_rate, host_listings_count, number_of_reviews:review_scores_rating, reviews_per_month)
data <- subset(data, bathrooms > 0)

colnames(data)[colnames(data) == 'price'] <- 'price_dollars'
colnames(data)[colnames(data) == 'neighbourhood_cleansed'] <- 'neighbourhood'
colnames(data)[colnames(data) == 'accommodates'] <- 'accomodates'

data$bathrooms <- ceiling(data$bathrooms)
data$id <- as.integer(data$id)
data$bathrooms <- as.numeric(data$bathrooms)
data$host_acceptance_rate <- substr(data$host_acceptance_rate, 1, nchar(data$host_acceptance_rate)-1) %>%
  as.numeric(data$host_acceptance_rate)
data$host_acceptance_rate <- data$host_acceptance_rate / 100.0
data$price_dollars <- as.numeric(gsub("\\$", "", data$price_dollars))

data$upload_date <- "2024-09-12"
data$first_review <- as.Date(data$first_review, format = "%Y-%m-%d")
data$apt_age_days <- ceiling(as.numeric(difftime(data$upload_date , data$first_review , units = c("days"))))

set.seed(123)
data$ID <- sample(10000:99999, size = nrow(data), replace = FALSE)
data <- data %>% 
  dplyr::select(ID, everything())  %>%
  dplyr::select(-id)

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

data <- data %>% dplyr::select(ID, host_id, price_dollars, latitude, longitude, dist_stephansdom_km, dist_schonbrunn_km, dist_train_station_km, neighbourhood, room_type, accomodates, bathrooms, amenities, host_acceptance_rate, host_listings_count, number_of_reviews, apt_age_days, review_scores_rating:reviews_per_month)

data$cleaning_service <- ifelse(str_detect(data$amenities, regex("Cleaning available during stay", ignore_case = TRUE)), 1, 0)
data$air_conditioning <- ifelse(str_detect(data$amenities, regex("Portable air conditioning|Air conditioning|Central air conditioning", ignore_case = TRUE)), 1, 0)
data$self_checkin <- ifelse(str_detect(data$amenities, regex("Self check-in", ignore_case = TRUE)), 1, 0)

data$cleaning_service <- as.factor(data$cleaning_service)
data$air_conditioning <- as.factor(data$air_conditioning)
data$self_checkin <- as.factor(data$self_checkin)

data_with_outliers <- data %>% dplyr::select(ID, host_id, price_dollars, latitude, longitude, dist_stephansdom_km, dist_schonbrunn_km, dist_train_station_km, neighbourhood, room_type, accomodates, bathrooms, cleaning_service, air_conditioning, self_checkin, host_acceptance_rate, host_listings_count, number_of_reviews, apt_age_days, review_scores_rating:reviews_per_month)

View(data_with_outliers)


#write.csv(data_with_outliers, "./data/vienna_listings_with_outliers.csv", row.names = FALSE)


###################### REMOVE OUTLIERS #########################################


linear_model <- lm(data = data_with_outliers, formula = price_dollars ~ .)

cooks_distance <- cooks.distance(linear_model)
abs_std_res <- abs(stdres(linear_model))
threshold_cook <- 4 / nrow(data_with_outliers)
residual_threshold <- 2
leverage_values <- hatvalues(linear_model)
predictors <- setdiff(names(data_with_outliers), "price_dollars")
studentized_residuals <- rstudent(linear_model)
z_scores <- (data_with_outliers$price_dollars - mean(data_with_outliers$price_dollars)) / sd(data_with_outliers$price_dollars)

outliers_manual_rows <- c(4374, 4375, 4376, 4573, 7159, 8580)
outliers_cook <- which(cooks_distance > threshold_cook)
outliers_residual <- which(abs_std_res > residual_threshold)
outliers_leverage <- which(leverage_values > (2 * length(coefficients(linear_model)) / nrow(data_with_outliers)))
outliers_studres <- which(abs(studentized_residuals) > 3)
outliers_z_score <- which(abs(z_scores) > 3)

all_outliers <- union(outliers_manual_rows, union(outliers_cook, union(outliers_residual, union(outliers_leverage, union(outliers_z_score, outliers_studres)))))

data_no_outliers <- data_with_outliers[-all_outliers, ]

View(data_no_outliers)

#write.csv(data_no_outliers, "./data/vienna_listings_no_outliers.csv", row.names = FALSE)