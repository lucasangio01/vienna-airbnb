library(plyr)
library(tidyverse)
library(ggplot2)
library(tmap)
library(sf)
library(osmdata)
library(ggplot2)
library(corrplot)


data <- read_csv("https://raw.githubusercontent.com/lucasangio01/vienna-airbnb/refs/heads/main/data/vienna_listings_no_outliers.csv", show_col_types = FALSE)

chosen_variables <- data %>%
  dplyr::select(-neighbourhood, -room_type) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

correlations <- corrplot(cor(chosen_variables), method = "circle", type = "upper")

neighb_count <- data %>%
  dplyr::count(neighbourhood, sort = TRUE)

neighb_price <- data %>%
  dplyr::group_by(neighbourhood) %>%
  dplyr::summarise(mean_price = mean(price_dollars))


################################### PLOTS ######################################


box_plot <- ggplot(data = chosen_variables, aes(x = variable, y = value, fill = variable)) + geom_boxplot() + labs(title = "Distribution of variables", x = "", y = "Value\n")

price_density <- ggplot(data = data, aes(x = price_dollars)) + geom_density(color = "darkgreen", fill = "lightgreen", lwd = 1) + labs(title = "Density of price", x = "Price", y = "Density")
reviews_density <- ggplot(data = data, aes(x = review_scores_rating)) + geom_density(color = "blue", lwd = 1, fill = "lightblue") + labs(title = "Density of reviews", x = "Review score", y = "Density")
#lat_lon <- ggplot(data = data, aes(x = longitude, y = latitude, color = neighbourhood)) + geom_point()
age_score <- ggplot(data = data, aes(x = apt_age_days, y = review_scores_rating)) + geom_point(color = "purple") + labs(title = "Apt age vs avg rating", x = "Apt age (days)",y = "Avg rating")

neighb_count_plot <- ggplot(data = neighb_count, aes(x = reorder(neighbourhood, n), y = n)) + geom_bar(stat = "identity", fill = "darkred") + labs(title = "Number of AirBnB, by neighbourhood", x = "", y = "") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
neighb_price_plot <- ggplot(data = neighb_price, aes(y = reorder(neighbourhood, mean_price), x = mean_price)) + geom_bar(stat = "identity", fill = "lightblue") + labs(title = "Average price by neighborhood", x = "", y = "") + theme_classic()
