library(dplyr)
library(tidyverse)

data <- read.csv("https://raw.githubusercontent.com/lucasangio01/ams-exam/refs/heads/main/Data/vienna_listings.csv")
data <- data %>% select(id, host_id, host_acceptance_rate, host_is_superhost, host_listings_count, neighbourhood_cleansed, latitude, longitude, room_type, accommodates, bathrooms, beds, amenities, price, minimum_nights, maximum_nights, number_of_reviews, first_review, review_scores_rating:review_scores_value, reviews_per_month)
data <- drop_na(data)

data <- data %>% select(id, host_id, price, latitude, longitude, neighbourhood_cleansed, room_type, accommodates, bathrooms, beds, amenities, host_acceptance_rate, host_is_superhost, host_listings_count, minimum_nights, maximum_nights, number_of_reviews:reviews_per_month)
data <- subset(data, bathrooms > 0)
View(data)

table(data$bathrooms)
