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

#correlations <- corrplot(cor(chosen_variables), method = "circle", type = "upper")

neighb_count <- data %>%
  dplyr::count(neighbourhood, sort = TRUE)

neighb_price <- data %>%
  dplyr::group_by(neighbourhood) %>%
  dplyr::summarise(mean_price = mean(price_dollars))


################################### PLOTS ######################################


box_plot <- ggplot(data = chosen_variables, aes(x = variable, y = value, fill = variable)) + geom_boxplot() + labs(title = "Distribution of variables", x = "", y = "Value\n")

price_density <- ggplot(data = data, aes(x = price_dollars)) + geom_density(color = "darkgreen", fill = "lightgreen", lwd = 1) + 
  labs(title = "Density of price", x = "Price", y = "Density") + 
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14), plot.title = element_text(size = 18, face = "bold"))

reviews_density <- ggplot(data = data, aes(x = review_scores_rating)) + geom_density(color = "blue", lwd = 1, fill = "lightblue") + 
  labs(title = "", x = "Review score", y = "Density") + 
  theme(axis.title = element_text(size = 16),  axis.text = element_text(size = 14), plot.title = element_text(size = 18, face = "bold"))

age_score <- ggplot(data = data, aes(x = apt_age_days, y = review_scores_rating)) + 
  geom_point(color = "purple") + labs(title = "Apt age vs avg rating", x = "Apt age (days)",y = "Avg rating") + 
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14), plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

neighb_count_plot <- ggplot(data = neighb_count, aes(x = reorder(neighbourhood, n), y = n)) + 
  geom_bar(stat = "identity", fill = "midnightblue") + labs(title = "", x = "Neighbourhood", y = "") + 
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

neighb_price_plot <- ggplot(data = neighb_price, aes(y = reorder(neighbourhood, mean_price), x = mean_price)) + 
  geom_bar(stat = "identity", fill = "lightblue") + labs(title = "Average Price by Neighborhood", x = "Average Price (USD)", y = "Neighborhood") + 
  theme_classic() + theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14), plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
