library(lme4)
library(tidyverse)
library(insight)

data <- read_csv('/Users/tommipremoli8/Desktop/Progetti/ams-exam/Data/vienna_listings_no_outliers.csv')

# newdata <- data %>% mutate(log_price_dollars = log(price_dollars + 1))

ggplot(data) +
  geom_point(aes(dist_stephansdom_km, price_dollars, col=neighbourhood))

ggplot(data) +
  geom_point(aes(accommodates, price_dollars)) +
  facet_wrap(~neighbourhood)

ggplot(data) +
  geom_point(aes(apt_age_days, price_dollars)) +
  facet_wrap(~neighbourhood)

# Standard linear model
fit_lm <- lm(price_dollars ~ dist_stephansdom_km + dist_schonbrunn_km + dist_train_station_km + room_type + 
               accommodates + bathrooms + beds + cleaning_service + air_conditioning + self_checkin + host_acceptance_rate + 
               host_listings_count + number_of_reviews + apt_age_days + review_scores_rating + review_scores_accuracy + 
               review_scores_cleanliness + review_scores_checkin + review_scores_communication + review_scores_location + 
               review_scores_value + reviews_per_month, 
             data = data)
summary(fit_lm)

ggplot(data, aes(x = dist_stephansdom_km, y = price_dollars)) +
  geom_point() +
  geom_smooth(method = "lm",se = F)

ggplot(data, aes(x = accommodates, y = price_dollars, colour = neighbourhood)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm",se = F) +
  theme_minimal()

ggplot(data, aes(x = dist_schonbrunn_km, y = price_dollars)) +
  geom_point() +
  geom_smooth(method = "lm",se = F)

# LM with Neighborhood as dummy variables
fit_lm_no_pooling <- lm(
  price_dollars ~ 
    (dist_stephansdom_km + dist_schonbrunn_km + dist_train_station_km +
       room_type + accommodates + bathrooms + beds + cleaning_service +
       air_conditioning + self_checkin + host_acceptance_rate +
       host_listings_count + number_of_reviews + apt_age_days +
       review_scores_rating + review_scores_accuracy + review_scores_cleanliness +
       review_scores_checkin + review_scores_communication + review_scores_location +
       review_scores_value + reviews_per_month) * neighbourhood,
  data = data
)

summary(fit_lm_no_pooling)

ggplot(data, aes(x = accommodates, y = price_dollars, colour = neighbourhood)) +
  geom_point(size = 1, alpha = 0.3) +
  geom_smooth(method = "lm",se = F) +
  labs(
    title = "Relationship between Number of Accommodates and Price",
    x = "Accomodates",
    y = "Price (Dollars)",
    colour = "Neighbourhood"
  ) +
  theme_minimal()

# Random Intercept model
fit_lmm_rand_intercept <- lmer(price_dollars ~ dist_stephansdom_km + dist_schonbrunn_km + dist_train_station_km +
                                 room_type + accommodates + bathrooms + beds + cleaning_service +
                                 air_conditioning + self_checkin + host_acceptance_rate +
                                 host_listings_count + number_of_reviews + apt_age_days +
                                 review_scores_rating + review_scores_accuracy + review_scores_cleanliness +
                                 review_scores_checkin + review_scores_communication + review_scores_location +
                                 review_scores_value + reviews_per_month + (1 | neighbourhood), data = data)

summary(fit_lmm_rand_intercept)

fixef(fit_lmm_rand_intercept) # Fixed effects

ranef(fit_lmm_rand_intercept) # Random effect
lattice::dotplot(ranef(fit_lmm_rand_intercept))

ggplot(data, aes(x = accommodates, y = price_dollars, colour = neighbourhood)) +
  geom_point(size = 1, alpha = 0.2) +
  geom_line(data = cbind(data, pred = predict(fit_lmm_rand_intercept)), aes(y = pred), size = 1) +
  theme_minimal()

ggplot(data, aes(x = dist_stephansdom_km, y = price_dollars, colour = neighbourhood)) +
  geom_point(size = 1, alpha = 0.2) +
  geom_line(data = cbind(data, pred = predict(fit_lmm_rand_intercept)), aes(y = pred), size = 1) +
  theme_minimal()

(sigma2_eps <- get_variance_residual(fit_lmm_rand_intercept))
(sigma2_b <- get_variance_random(fit_lmm_rand_intercept))
(PVRE <- sigma2_b/(sigma2_b+sigma2_eps))


# Random Intercept and slopes model
fit_lmm_rand_int_and_slope <- lmer(price_dollars ~ dist_stephansdom_km + dist_schonbrunn_km + dist_train_station_km +
                             room_type + accommodates + bathrooms + beds + cleaning_service +
                             air_conditioning + self_checkin + host_acceptance_rate +
                             host_listings_count + number_of_reviews + apt_age_days +
                             review_scores_rating + review_scores_accuracy + review_scores_cleanliness +
                             review_scores_checkin + review_scores_communication + review_scores_location +
                             review_scores_value + reviews_per_month + 
                             (dist_stephansdom_km + dist_schonbrunn_km + accommodates + review_scores_rating | neighbourhood),
                           data = data)
summary (fit_lmm_rand_int_and_slope)

fixef(fit_lmm_rand_int_and_slope)
ranef(fit_lmm_rand_int_and_slope)

lattice::dotplot(ranef(fit_lmm_rand_int_and_slope))

ggplot(data, aes(x = accommodates, y = price_dollars, colour = neighbourhood)) +
  geom_point(alpha = 0.2) +
  geom_line(data = cbind(data, pred = predict(fit_lmm_rand_int_and_slope)), aes(y = pred), linewidth = 1) +
  labs(
    title = "Relazione tra Distanza da Stephansdom e Prezzo Logaritmico",
    x = "Distanza da Stephansdom (km)",
    y = "Prezzo Logaritmico (dollari)",
    colour = "Quartiere"
  ) +
  theme_minimal()

# Random Slopes model
fit_lmm_rand_slope <- lmer(
  price_dollars ~ dist_stephansdom_km + dist_schonbrunn_km + dist_train_station_km +
    room_type + accommodates + bathrooms + beds + cleaning_service +
    air_conditioning + self_checkin + host_acceptance_rate +
    host_listings_count + number_of_reviews + apt_age_days +
    review_scores_rating + review_scores_accuracy + review_scores_cleanliness +
    review_scores_checkin + review_scores_communication + review_scores_location +
    review_scores_value + reviews_per_month +
    (dist_stephansdom_km + dist_schonbrunn_km + accommodates + review_scores_rating - 1 | neighbourhood),
  data = data
)

fixef(fit_lmm_rand_slope)
ranef(fit_lmm_rand_slope)

lattice::dotplot(ranef(fit_lmm_rand_slope))

ggplot(data, aes(x = accommodates, y = price_dollars, colour = neighbourhood)) +
  geom_point(alpha = 0.2) +
  geom_line(data = cbind(data, pred = predict(fit_lmm_rand_slope)), aes(y = pred), size = 1) 

# Prediction
hat_y_lm <- predict(fit_lm) 
hat_y_mem <- predict(fit_lmm_rand_int_and_slope)
hat_y_LMnopooling <- predict(fit_lm_no_pooling)
hat_y_intmod <- predict(fit_lmm_rand_intercept)
hat_y_slomod <- predict(fit_lmm_rand_slope)

yardstick::rmse_vec(truth = data$price_dollars,estimate = hat_y_lm)
yardstick::rmse_vec(truth = data$price_dollars,estimate = hat_y_mem) # Random Intercept and slopes model
yardstick::rmse_vec(truth = data$price_dollars,estimate = hat_y_LMnopooling) # LM with Neighborhood as dummy variables
yardstick::rmse_vec(truth = data$price_dollars,estimate = hat_y_intmod) # Random Intercept model
yardstick::rmse_vec(truth = data$price_dollars,estimate = hat_y_slomod)# Random Slopes model

plot(fit_lmm_rand_int_and_slope)
qqnorm(resid(fit_lmm_rand_int_and_slope))
qqline(resid(fit_lmm_rand_int_and_slope), col='red', lwd=2)

anova(fit_lmm_rand_int_and_slope, fit_lm) 


# Boxplot neighbourhood
ggplot(data, aes(x = neighbourhood, y = price_dollars)) +
  geom_boxplot() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90)) +
  labs(x = "Neighbourhoods", y = "Price ($)")


ggplot(data, aes(accommodates, price_dollars)) +
  geom_point() +
  facet_wrap(~ neighbourhood, nrow = 4) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Accommodates", y = "Price ($)") +
  coord_cartesian(ylim = c(0, 360))

ggplot(data, aes(dist_stephansdom_km, price_dollars)) +
  geom_point() +
  facet_wrap(~ neighbourhood, nrow = 4) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "dist_stephansdom_km", y = "Price ($)") +
  coord_cartesian(ylim = c(0, 360))

ggplot(data, aes(dist_schonbrunn_km, price_dollars)) +
  geom_point() +
  facet_wrap(~ neighbourhood, nrow = 4) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "dist_schonbrunn_km", y = "Price ($)") +
  coord_cartesian(ylim = c(0, 360))

ggplot(data, aes(review_scores_rating, price_dollars)) +
  geom_point() +
  facet_wrap(~ neighbourhood, nrow = 4) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "review_scores_rating", y = "Price ($)") +
  coord_cartesian(ylim = c(0, 360))
