library(lme4)
library(tidyverse)
library(insight)
library(sjPlot)
library(sf)
library(tmap)

data <- read_csv('/Users/tommipremoli8/Desktop/Progetti/ams-exam/Data/vienna_listings_no_outliers.csv')

pal <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#bcbd22", "#9467bd", 
  "#8c564b", "#e377c2", "#7f7f7f", "#d62728", "#17becf",
  "#393b79", "#637939", "#8c6d31", "#843c39", "#7b4173",
  "#6baed6", "#fd8d3c", "#78c679", "#e6550d", "#a1d99b",
  "#756bb1", "#dadaeb", "#fdae6b"
)


ggplot(data) +
  geom_point(aes(accommodates, price_dollars)) +
  facet_wrap(~neighbourhood)

ggplot(data) +
  geom_point(aes(apt_age_days, price_dollars)) +
  facet_wrap(~neighbourhood)

# Standard linear model
fit_lm <- lm(price_dollars ~ dist_stephansdom_km + dist_schonbrunn_km + dist_train_station_km + room_type + 
               accommodates + bathrooms + cleaning_service + air_conditioning + self_checkin + host_acceptance_rate + 
               host_listings_count + number_of_reviews + apt_age_days + review_scores_rating + reviews_per_month, 
             data = data)
summary(fit_lm)

ggplot(data, aes(x = dist_stephansdom_km, y = price_dollars)) +
  geom_point() +
  geom_smooth(method = "lm",se = F)

ggplot(data, aes(x = accommodates, y = price_dollars, colour = neighbourhood)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm",se = F) +
  theme_minimal()

ggplot(data, aes(x = review_scores_rating, y = price_dollars, colour = neighbourhood)) +
  geom_point(alpha = 0.05, size = 0.8) +
  geom_smooth(method = "lm",se = F) +
  theme_minimal()

ggplot(data, aes(x = review_scores_location, y = price_dollars, colour = neighbourhood)) +
  geom_point(alpha = 0.05, size = 1) +
  geom_smooth(method = "lm",se = F) +
  theme_minimal()

# LM with Neighborhood as dummy variables
fit_lm_no_pooling <- lm(
  price_dollars ~ 
    (dist_stephansdom_km + dist_schonbrunn_km + dist_train_station_km +
       room_type + accommodates + bathrooms + cleaning_service +
       air_conditioning + self_checkin + host_acceptance_rate +
       host_listings_count + number_of_reviews + apt_age_days +
       review_scores_rating + reviews_per_month) * neighbourhood,
  data = data
)

summary(fit_lm_no_pooling)

ggplot(data, aes(x = accommodates, y = price_dollars, colour = neighbourhood)) +
  geom_point(size = 1, alpha = 0.05) +
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
                                 room_type + accommodates + bathrooms + cleaning_service +
                                 air_conditioning + self_checkin + host_acceptance_rate +
                                 host_listings_count + number_of_reviews + apt_age_days +
                                 review_scores_rating + reviews_per_month + (1 | neighbourhood), data = data)

summary(fit_lmm_rand_intercept)

fixef(fit_lmm_rand_intercept) 
ranef(fit_lmm_rand_intercept) 

lattice::dotplot(ranef(fit_lmm_rand_intercept))


plot_model(
  fit_lmm_rand_intercept, type = "pred", terms = c("accommodates", "neighbourhood"),
  pred.type = "re", ci.lvl = NA) +                       
  scale_color_manual(values = pal) +   
  labs(
    title = "Prediction of the Effect of Accommodates on Prices.",
    x = "Accommodates",
    y = "Price (Dollars)",
    colour = "Neighbourhood"
  ) +
  theme_minimal()

plot_model(
  fit_lmm_rand_intercept, type = "pred", terms = c("review_scores_rating", "neighbourhood"),
  pred.type = "re", ci.lvl = NA) +                       
  scale_color_manual(values = pal) +   
  labs(
    title = "Prediction of the Effect of Accommodates on Prices.",
    x = "Review Scores Rating",
    y = "Price (Dollars)",
    colour = "Neighbourhood"
  ) +
  theme_minimal()

plot_model(
  fit_lmm_rand_intercept, type = "pred", terms = c("dist_stephansdom_km", "neighbourhood"),
  pred.type = "re", ci.lvl = NA) +                       
  scale_color_manual(values = pal) +   
  labs(
    title = "Prediction of the Effect of Accommodates on Prices.",
    x = "Distance from Stephansdom (km)",
    y = "Price (Dollars)",
    colour = "Neighbourhood"
  ) +
  theme_minimal()

(sigma2_eps <- get_variance_residual(fit_lmm_rand_intercept))
(sigma2_b <- get_variance_random(fit_lmm_rand_intercept))
(PVRE <- sigma2_b/(sigma2_b+sigma2_eps))


# Random Intercept and slopes model
fit_lmm_rand_int_and_slope <- lmer(price_dollars ~ dist_stephansdom_km + dist_schonbrunn_km + dist_train_station_km +
                             room_type + accommodates + bathrooms + cleaning_service +
                             air_conditioning + self_checkin + host_acceptance_rate +
                             host_listings_count + number_of_reviews + apt_age_days +
                             review_scores_rating + reviews_per_month + 
                             (dist_stephansdom_km + accommodates + review_scores_rating | neighbourhood),
                           data = data)
summary (fit_lmm_rand_int_and_slope)

fixef(fit_lmm_rand_int_and_slope)
ranef(fit_lmm_rand_int_and_slope)

lattice::dotplot(ranef(fit_lmm_rand_int_and_slope))

plot_model(
  fit_lmm_rand_int_and_slope, type = "pred", terms = c("accommodates", "neighbourhood"),
  pred.type = "re", ci.lvl = NA) +                       
  scale_color_manual(values = pal) +   
  labs(
    title = "Prediction of the Effect of Accommodates on Prices.",
    x = "Accommodates",
    y = "Price (Dollars)",
    colour = "Neighbourhood"
  ) +
  theme_minimal()

ggplot(data, aes(x = accommodates, y = price_dollars, colour = neighbourhood)) +
  geom_point(alpha = 0.2) +
  geom_line(data = cbind(data, pred = predict(fit_lmm_rand_int_and_slope)), aes(y = pred), linewidth = 1) +
  scale_color_manual(values = pal) +
  theme_minimal()

plot_model(
  fit_lmm_rand_int_and_slope, type = "pred", terms = c("review_scores_rating", "neighbourhood"),
  pred.type = "re", ci.lvl = NA) +                       
  scale_color_manual(values = pal) +   
  labs(
    title = "Prediction of the Effect of Accommodates on Prices.",
    x = "Review Scores Rating",
    y = "Price (Dollars)",
    colour = "Neighbourhood"
  ) +
  theme_minimal()

ggplot(data, aes(x = review_scores_rating, y = price_dollars, colour = neighbourhood)) +
  geom_point(alpha = 0.2) +
  geom_line(data = cbind(data, pred = predict(fit_lmm_rand_int_and_slope)), aes(y = pred), linewidth = 1) +
  scale_color_manual(values = pal) +
  theme_minimal()

plot_model(
  fit_lmm_rand_int_and_slope, type = "pred", terms = c("host_acceptance_rate", "neighbourhood"),
  pred.type = "re", ci.lvl = NA) +                       
  scale_color_manual(values = pal) +   
  labs(
    title = "Prediction of the Effect of Accommodates on Prices.",
    x = "Distance from Stephansdom (km)",
    y = "Price (Dollars)",
    colour = "Neighbourhood"
  ) +
  theme_minimal()

ggplot(data, aes(x = host_acceptance_rate, y = price_dollars, colour = neighbourhood)) +
  geom_point(alpha = 0.2) +
  geom_line(data = cbind(data, pred = predict(fit_lmm_rand_int_and_slope)), aes(y = pred), linewidth = 1) +
  scale_color_manual(values = pal) +
  theme_minimal()

# Random Slopes model
fit_lmm_rand_slope <- lmer(
  price_dollars ~ dist_stephansdom_km + dist_schonbrunn_km + dist_train_station_km +
    room_type + accommodates + bathrooms + cleaning_service +
    air_conditioning + self_checkin + host_acceptance_rate +
    host_listings_count + number_of_reviews + apt_age_days +
    review_scores_rating + reviews_per_month +
    (dist_stephansdom_km + accommodates + review_scores_rating - 1 | neighbourhood),
  data = data
)

fixef(fit_lmm_rand_slope)
ranef(fit_lmm_rand_slope)

lattice::dotplot(ranef(fit_lmm_rand_slope))

plot_model(
  fit_lmm_rand_slope, type = "pred", terms = c("accommodates", "neighbourhood"),
  pred.type = "re", ci.lvl = NA) +                       
  scale_color_manual(values = pal) +   
  labs(
    title = "Prediction of the Effect of Accommodates on Prices.",
    x = "Accommodates",
    y = "Price (Dollars)",
    colour = "Neighbourhood"
  ) +
  theme_minimal()

ggplot(data, aes(x = accommodates, y = price_dollars, colour = neighbourhood)) +
  geom_point(alpha = 0.2) +
  geom_line(data = cbind(data, pred = predict(fit_lmm_rand_slope)), aes(y = pred), size = 1) +
  theme_minimal()

plot_model(
  fit_lmm_rand_slope, type = "pred", terms = c("reviews_per_month", "neighbourhood"),
  pred.type = "re", ci.lvl = NA) +                       
  scale_color_manual(values = pal) +   
  labs(
    title = "Prediction of the Effect of Accommodates on Prices.",
    x = "Review Scores Rating",
    y = "Price (Dollars)",
    colour = "Neighbourhood"
  ) +
  theme_minimal()

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

plot(fit_lmm_rand_intercept)
qqnorm(resid(fit_lmm_rand_intercept))
qqline(resid(fit_lmm_rand_intercept), col='red', lwd=2)

plot(fit_lmm_rand_slope)
qqnorm(resid(fit_lmm_rand_slope))
qqline(resid(fit_lmm_rand_slope), col='red', lwd=2)

anova(fit_lmm_rand_int_and_slope, fit_lm) 


# Boxplot neighbourhood
ggplot(data, aes(x = neighbourhood, y = price_dollars)) +
  geom_boxplot() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90)) +
  labs(x = "Neighbourhoods", y = "Price ($)") +
  theme_minimal()


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
  geom_smooth(method = 'lm') +
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

ggplot(data, aes(accommodates, price_dollars)) +
  geom_point(size = 0.2) +
  geom_line(aes(y = predict(fit_lmm_rand_int_and_slope), 
  group = neighbourhood, 
  color = neighbourhood)) +
  labs(x = "accommodates",
  y = "price_dollars") + 
  theme_minimal()


### Map visualization
## Random Intercept Model
vienna_shapefile <- st_read("/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Year/Advanced Multivariate Statistics/BEZIRKSGRENZEOGD/BEZIRKSGRENZEOGDPolygon.shp")
vienna_shapefile$NAMEK <- iconv(vienna_shapefile$NAMEK, from = "latin1", to = "UTF-8")

ranefs <- ranef(fit_lmm_rand_intercept)$neighbourhood
ranefs_df_int <- data.frame(neighbourhood = rownames(ranefs), ranef_value = ranefs[, 1])

vienna_shapefile <- vienna_shapefile %>%
  left_join(ranefs_df_int, by = c("NAMEK" = "neighbourhood"))

ggplot(vienna_shapefile) +
  geom_sf(aes(fill = ranef_value)) +  
  scale_fill_viridis_c(option = "inferno", direction = -1) +            
  theme_minimal() +                   
  labs(title = "Map of Vienna Districts with Random Effects",
       subtitle = "Random Intercept Model",
       fill = "Random Effect")

## Random Intercept and Slope Model
ranefs_int_slo_mod <- data.frame(
  neighbourhood = c("Alsergrund", "Brigittenau", "Döbling", "Donaustadt", "Favoriten", 
                    "Floridsdorf", "Hernals", "Hietzing", "Innere Stadt", "Josefstadt", 
                    "Landstraße", "Leopoldstadt", "Liesing", "Margareten", "Mariahilf", 
                    "Meidling", "Neubau", "Ottakring", "Penzing", "Rudolfsheim-Fünfhaus", 
                    "Simmering", "Währing", "Wieden"),
  # Intercept, slope for dist_stephansdom_km, accommodates, review_scores_rating
  ranef_intercept = c(61.791, 15.131, -9.866, -6.233, -15.385, -7.015, -41.111, -10.780, 
                      14.163, 40.483, -11.093, -22.556, -17.188, -7.553, 24.102, -7.138, 
                      39.957, -10.464, -15.826, -4.690, -12.467, -15.555, 19.292),
  ranef_dist_stephansdom_km = c(-24.291, -3.357, 3.794, 2.486, 7.539, 6.525, 11.510, 6.744, 
                                -12.879, -13.741, 2.248, 8.142, 6.700, 3.148, -14.927, 6.331, 
                                -17.271, 5.943, 9.765, 5.074, 5.499, 6.616, -11.597),
  ranef_accommodates = c(2.553, 1.236, 1.005, 1.085, -2.108, -0.413, 1.119, -2.647, 4.654, 
                         1.182, -0.164, -0.284, -0.271, -1.277, 1.894, -0.0003, 1.885, 
                         -1.096, -3.821, -1.019, -2.674, -1.683, 0.842),
  ranef_review_scores_rating = c(-4.354, -4.326, -0.353, 2.888, 0.195, -3.667, -0.305, 2.235, 
                                 4.681, -1.816, 2.609, 0.600, 0.673, 0.326, 4.150, -2.355, 
                                 1.259, -2.401, -1.398, -1.844, 2.102, -0.730, 1.829)
)

vienna_shapefile <- left_join(vienna_shapefile, ranefs_data, by = c("NAMEK" = "neighbourhood"))

ranefs_long <- ranefs_data %>%
  gather(key = "effect", value = "value", -neighbourhood)

vienna_shapefile_long <- left_join(vienna_shapefile, ranefs_long, by = c("NAMEK" = "neighbourhood"))

ggplot(vienna_shapefile_long) +
  geom_sf(aes(fill = value)) + 
  scale_fill_viridis_c(option = "inferno", direction = -1) +     
  theme_minimal() + 
  facet_wrap(~effect) +         
  labs(title = "Random Effects Map of Vienna Districts",
       subtitle = "Random Intercept and Slope Model",
       fill = "Random Effect")
