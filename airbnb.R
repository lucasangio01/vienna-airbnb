library(dplyr)
library(tidyverse)


data <- read.csv("Data/vienna_listings.csv")

data <- data %>% select(id, host_id, host_acceptance_rate, host_is_superhost, host_listings_count, neighbourhood_cleansed, latitude, longitude, room_type, accommodates, bathrooms, beds, amenities, price, number_of_reviews, first_review, review_scores_rating:review_scores_value, reviews_per_month)
#data <- drop_na(data)
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

View(data)
print(colSums(is.na(data)))
