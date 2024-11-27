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

# Standard linear model
fit_lm <- lm(price_dollars ~ dist_stephansdom_km + dist_schonbrunn_km + dist_train_station_km + room_type + 
               accomodates + bathrooms + cleaning_service + air_conditioning + self_checkin + host_acceptance_rate + 
               host_listings_count + number_of_reviews + apt_age_days + review_scores_rating + reviews_per_month, 
             data = data)
summary(fit_lm)

ggplot(data, aes(x = dist_stephansdom_km, y = price_dollars)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm",se = F) + 
  theme_minimal()

ggplot(data, aes(x = accomodates, y = price_dollars, colour = neighbourhood)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm",se = F) +
  theme_minimal()

ggplot(data, aes(x = dist_stephansdom_km, y = price_dollars, colour = neighbourhood)) +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth(method = "lm",se = F) +
  theme_minimal()

# LM with Neighborhood as dummy variables
fit_lm_no_pooling <- lm(
  price_dollars ~ 
    (dist_stephansdom_km + dist_schonbrunn_km + dist_train_station_km +
       room_type + accomodates + bathrooms + cleaning_service +
       air_conditioning + self_checkin + host_acceptance_rate +
       host_listings_count + number_of_reviews + apt_age_days +
       review_scores_rating + reviews_per_month) * neighbourhood,
  data = data
)

summary(fit_lm_no_pooling)

ggplot(data, aes(x = accomodates, y = price_dollars, colour = neighbourhood)) +
  geom_point(size = 1, alpha = 0.05) +
  geom_smooth(method = "lm",se = F) +
  labs(
    title = "Relationship between Number of accomodates and Price",
    x = "Accomodates",
    y = "Price (Dollars)",
    colour = "Neighbourhood"
  ) +
  theme_minimal()

# Random Intercept model
fit_lmm_rand_intercept <- lmer(price_dollars ~ dist_stephansdom_km + dist_schonbrunn_km + dist_train_station_km +
                                 room_type + accomodates + bathrooms + cleaning_service +
                                 air_conditioning + self_checkin + host_acceptance_rate +
                                 host_listings_count + number_of_reviews + apt_age_days +
                                 review_scores_rating + reviews_per_month + (1 | neighbourhood), data = data)

summary(fit_lmm_rand_intercept)

fixef(fit_lmm_rand_intercept) 
ranef(fit_lmm_rand_intercept) 

lattice::dotplot(ranef(fit_lmm_rand_intercept))

plot_model(
  fit_lmm_rand_intercept, type = "pred", terms = c("accomodates", "neighbourhood"),
  pred.type = "re", ci.lvl = NA) +                       
  scale_color_manual(values = pal) +   
  labs(
    title = "Prediction of the Effect of accomodates on Prices.",
    x = "accomodates",
    y = "Price (Dollars)",
    colour = "Neighbourhood"
  ) +
  theme_minimal()

plot_model(
  fit_lmm_rand_intercept, type = "pred", terms = c("review_scores_rating", "neighbourhood"),
  pred.type = "re", ci.lvl = NA) +                       
  scale_color_manual(values = pal) +   
  labs(
    title = "Prediction of the Effect of accomodates on Prices.",
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
    title = "Prediction of the Effect of accomodates on Prices.",
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
                             room_type + accomodates + bathrooms + cleaning_service +
                             air_conditioning + self_checkin + host_acceptance_rate +
                             host_listings_count + number_of_reviews + apt_age_days +
                             review_scores_rating + reviews_per_month + 
                             (dist_stephansdom_km + room_type + review_scores_rating | neighbourhood),
                           data = data)
summary (fit_lmm_rand_int_and_slope)

fixef(fit_lmm_rand_int_and_slope)
ranef(fit_lmm_rand_int_and_slope)

lattice::dotplot(ranef(fit_lmm_rand_int_and_slope))

plot_model(
  fit_lmm_rand_int_and_slope, type = "pred", terms = c("accomodates", "neighbourhood"),
  pred.type = "re", ci.lvl = NA) +                       
  scale_color_manual(values = pal) +   
  labs(
    title = "Prediction of the Effect of accomodates on Prices.",
    x = "accomodates",
    y = "Price (Dollars)",
    colour = "Neighbourhood"
  ) +
  theme_minimal()

plot_model(
  fit_lmm_rand_int_and_slope, type = "pred", terms = c("review_scores_rating", "neighbourhood"),
  pred.type = "re", ci.lvl = NA) +                       
  scale_color_manual(values = pal) +   
  labs(
    title = "Prediction of the Effect of accomodates on Prices.",
    x = "Review Scores Rating",
    y = "Price (Dollars)",
    colour = "Neighbourhood"
  ) +
  theme_minimal()

plot_model(
  fit_lmm_rand_int_and_slope, type = "pred", terms = c("host_acceptance_rate", "neighbourhood"),
  pred.type = "re", ci.lvl = NA) +                       
  scale_color_manual(values = pal) +   
  labs(
    title = "Prediction of the Effect of accomodates on Prices.",
    x = "Distance from Stephansdom (km)",
    y = "Price (Dollars)",
    colour = "Neighbourhood"
  ) +
  theme_minimal()

# Random Slopes model
fit_lmm_rand_slope <- lmer(
  price_dollars ~ dist_stephansdom_km + dist_schonbrunn_km + dist_train_station_km +
    room_type + accomodates + bathrooms + cleaning_service +
    air_conditioning + self_checkin + host_acceptance_rate +
    host_listings_count + number_of_reviews + apt_age_days +
    review_scores_rating + reviews_per_month +
    (dist_stephansdom_km + room_type + review_scores_rating - 1 | neighbourhood),
  data = data
)

fixef(fit_lmm_rand_slope)
ranef(fit_lmm_rand_slope)

lattice::dotplot(ranef(fit_lmm_rand_slope))

plot_model(
  fit_lmm_rand_slope, type = "pred", terms = c("accomodates", "neighbourhood"),
  pred.type = "re", ci.lvl = NA) +                       
  scale_color_manual(values = pal) +   
  labs(
    title = "Prediction of the Effect of accomodates on Prices.",
    x = "accomodates",
    y = "Price (Dollars)",
    colour = "Neighbourhood"
  ) +
  theme_minimal()

plot_model(
  fit_lmm_rand_slope, type = "pred", terms = c("reviews_per_month", "neighbourhood"),
  pred.type = "re", ci.lvl = NA) +                       
  scale_color_manual(values = pal) +   
  labs(
    title = "Prediction of the Effect of accomodates on Prices.",
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
yardstick::rmse_vec(truth = data$price_dollars,estimate = hat_y_slomod) # Random Slopes model

plot(fit_lmm_rand_int_and_slope)
qqnorm(resid(fit_lmm_rand_int_and_slope))
qqline(resid(fit_lmm_rand_int_and_slope), col='red', lwd=2)

anova(fit_lmm_rand_int_and_slope, fit_lm)
anova(fit_lmm_rand_int_and_slope, fit_lmm_rand_intercept) 


# Boxplot neighbourhood
ggplot(data, aes(x = neighbourhood, y = price_dollars)) +
  geom_boxplot() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90)) +
  labs(x = "Neighbourhoods", y = "Price ($)") +
  theme_minimal()

ggplot(data, aes(dist_stephansdom_km, price_dollars)) +
  geom_point() +
  facet_wrap(~ neighbourhood, nrow = 4) +
  geom_smooth(method = 'lm') +
  theme_bw() +
  labs(x = "dist_stephansdom_km", y = "Price ($)") +
  coord_cartesian(ylim = c(0, 360))

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
                    "Floridsdorf", "Hernals", "Innere Stadt", "Josefstadt", 
                    "Landstraße", "Leopoldstadt", "Margareten", "Mariahilf", 
                    "Meidling", "Neubau", "Ottakring", "Penzing", "Rudolfsheim-Fünfhaus", 
                    "Simmering", "Währing", "Wieden"),
  ranef_intercept = c(47.683, 21.443, -21.181, -13.044, -7.476, -7.795, -35.303, 32.675, 
                      28.335, -4.039, -21.914, -19.002, 23.419, -10.769, 39.877, -5.377, 
                      -18.888, 14.502, -27.065, -22.429, 6.348),
  ranef_dist_stephansdom_km = c(-15.328, -2.677, 8.990, 6.093, 6.586, 6.854, 12.650, -16.825, 
                                -11.826, 0.517, 7.816, 3.550, -15.974, 7.373, -17.100, 6.840, 
                                10.144, 6.317, 1.138, 5.976, -11.114),
  ranef_room_typePrivate_room = c(-2.944, 0.375, -0.093, 3.764, 2.466, -0.295, -1.955, 5.017, 
                                  -1.192, 0.354, 0.108, -4.276, -2.002, 3.323, 0.276, 2.069, 
                                  2.467, 10.816, -5.145, -4.147, -8.986),
  ranef_review_scores_rating = c(-3.394, -5.020, -1.376, 0.390, -2.783, -4.241, -0.766, 7.435, 
                                 1.014, 1.133, 0.186, 1.387, 6.675, -1.963, 2.816, -4.005, 
                                 -2.190, -6.689, 6.413, 0.242, 4.735)
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
