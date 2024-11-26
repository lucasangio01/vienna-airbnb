library(plyr)
library(tidyverse)
library(ggplot2)
library(tmap)
library(sf)
library(osmdata)
library(corrplot)


data <- read_csv("./data/vienna_listings_final.csv")

numeric_variables <- data %>%
  dplyr::select("price_dollars":"dist_train_station_km", "accomodates":"reviews_per_month")

reviews <- data %>%
  dplyr::select("review_scores_rating":"review_scores_value") %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

others <- data %>%
  dplyr::select("price_dollars", "number_of_reviews") %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

correlations <- corrplot(cor(numeric_variables), method = "circle", type = "upper")
reviews_density <- ddply(reviews, "variable", summarise, grp.mean=mean("value"))


################################### PLOTS ######################################


box_reviews <- ggplot(data = reviews, aes(x = variable, y = value, fill = variable)) + geom_boxplot() + theme(legend.position = "bottom") + labs(title = "Distribution of reviews", x = "", y = "Review score\n")
box_others <- ggplot(data = others, aes(x = variable, y = value, fill = variable)) + geom_boxplot() + theme(legend.position = "bottom") + labs(title = "Distribution of other variables", x = "", y = "")

density_reviews <-ggplot(data = reviews, aes(x = variable, y = value, color = variable)) + geom_density() + theme(legend.position = "bottom") + labs(title = "Distribution of reviews", x = "", y = "Review score\n")

price_neighb <- ggplot() + geom_histogram(data = data, aes(x = price_dollars, fill = neighbourhood), position = "identity")
price_density <- ggplot(data = data, aes(x = price_dollars)) + geom_density(color = "darkgreen", fill = "lightgreen", lwd = 1)
score_density <- ggplot(data = data, aes(x = review_scores_rating)) + geom_density(color = "blue", lwd = 1, fill = "lightblue")
lat_lon <- ggplot(data = data, aes(x = longitude, y = latitude, color = neighbourhood)) + geom_point()
age_score <- ggplot(data = data, aes(x = apt_age_days, y = review_scores_rating)) + geom_point(color = "purple")

                    