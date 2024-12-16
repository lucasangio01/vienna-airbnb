# Load necessary libraries
library(tidyverse)
library(corrplot)
library(car)
library(boot)
library(ggplot2)

data <- read.csv("https://raw.githubusercontent.com/lucasangio01/vienna-airbnb/refs/heads/main/data/vienna_listings_no_outliers.csv")

str(data)
head(data)

num_vars <- data %>%
  select(price_dollars, dist_stephansdom_km, dist_schonbrunn_km, dist_train_station_km, 
         accomodates, bathrooms, host_acceptance_rate, host_listings_count, 
         number_of_reviews, apt_age_days, review_scores_rating, reviews_per_month)

cor_matrix <- cor(num_vars, use = "complete.obs")

corrplot(cor_matrix, method = "circle", type = "upper", tl.cex = 0.7)

ggplot(data, aes(x = room_type, y = price_dollars)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = "median", geom = "point", color = "red", size = 3) +
  labs(title = "Price vs Room Type", x = "Room Type", y = "Price")

top_neighbourhoods <- data %>%
  group_by(neighbourhood) %>%
  summarise(avg_price = mean(price_dollars)) %>%
  arrange(desc(avg_price)) %>%
  slice(1:10)

ggplot(top_neighbourhoods, aes(x = reorder(neighbourhood, -avg_price), y = avg_price)) +
  geom_col() +
  coord_flip() +
  labs(title = "Average Price by Top 10 Neighbourhoods", x = "Neighbourhood", y = "Average Price")

model <- lm(price_dollars ~ dist_stephansdom_km + accomodates + bathrooms + room_type, data = data)
vif_values <- vif(model)
print(vif_values)

full_model <- lm(price_dollars ~ dist_stephansdom_km + dist_schonbrunn_km + dist_train_station_km + 
                   accomodates + bathrooms + room_type + review_scores_rating + host_listings_count, 
                 data = data)
stepwise_model <- step(full_model, direction = "both")
summary(stepwise_model)

model_with_neighbourhood <- lm(price_dollars ~ dist_stephansdom_km + dist_schonbrunn_km + 
                                 dist_train_station_km + accomodates + bathrooms + 
                                 room_type + review_scores_rating + host_listings_count + 
                                 neighbourhood, data = data)
summary(model_with_neighbourhood)

boot_function <- function(data, indices) {
  boot_data <- data[indices, ]
  model <- lm(price_dollars ~ dist_stephansdom_km + dist_schonbrunn_km +
                dist_train_station_km + accomodates + bathrooms +
                room_type + review_scores_rating + host_listings_count +
                neighbourhood, data = boot_data)
  return(coef(model))
}

set.seed(123)
bootstrap_results <- boot(data = data, statistic = boot_function, R = 1000)

nonparametric_ci <- apply(bootstrap_results$t, 2, function(x) quantile(x, c(0.025, 0.975)))

parametric_boot_function <- function(model, data, R = 1000) {
  boot_samples <- replicate(R, {
    resampled_residuals <- rnorm(nrow(data), mean = 0, sd = sd(resid(model)))
    new_y <- fitted(model) + resampled_residuals
    new_model <- lm(new_y ~ dist_stephansdom_km + dist_schonbrunn_km + 
                      dist_train_station_km + accomodates + bathrooms + 
                      room_type + review_scores_rating + host_listings_count + 
                      neighbourhood, data = data)
    return(coef(new_model))
  }, simplify = FALSE)
  
  do.call(rbind, boot_samples)
}

set.seed(123)
parametric_bootstrap_results <- parametric_boot_function(model_with_neighbourhood, data, R = 1000)

parametric_ci <- apply(parametric_bootstrap_results, 2, function(x) quantile(x, c(0.025, 0.975)))


colnames(nonparametric_ci) <- colnames(parametric_ci)
comparison_table <- data.frame(
  Coefficient = colnames(parametric_ci),
  Parametric_Lower = parametric_ci[1, ],
  Parametric_Upper = parametric_ci[2, ],
  Nonparametric_Lower = nonparametric_ci[1, ],
  Nonparametric_Upper = nonparametric_ci[2, ]
)
print(comparison_table)


plot_data <- data.frame(
  Coefficient = colnames(parametric_ci),
  Method = rep(c("Parametric", "Nonparametric"), each = ncol(parametric_ci)),
  Lower = c(parametric_ci[1, ], nonparametric_ci[1, ]),
  Upper = c(parametric_ci[2, ], nonparametric_ci[2, ])
)

ggplot(plot_data, aes(x = Coefficient, ymin = Lower, ymax = Upper, color = Method)) +
  geom_errorbar(position = position_dodge(width = 0.5), width = 0.3) +
  labs(title = "Comparison of Parametric and Nonparametric Bootstrap CIs",
       x = "Coefficients",
       y = "Confidence Interval") +
  coord_flip() +
  theme_minimal()
