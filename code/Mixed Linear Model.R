library(lme4)
library(tidyverse)
library(insight)
library(sjPlot)
library(sf)
library(tmap)
library(RColorBrewer) 

data <- read_csv('/Users/tommipremoli8/Desktop/Progetti/ams-exam/data/vienna_listings_no_outliers.csv')

# Standard linear model (complete pooling)
fit_lm <- lm(price_dollars ~ dist_stephansdom_km + dist_schonbrunn_km + dist_train_station_km + room_type + 
               accomodates + bathrooms + cleaning_service + air_conditioning + self_checkin + host_acceptance_rate + 
               host_listings_count + number_of_reviews + apt_age_days + review_scores_rating + reviews_per_month, 
             data = data)
summary(fit_lm)

ggplot(data, aes(x = dist_stephansdom_km, y = price_dollars)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm",se = F) + 
  labs(title = "Effect of Distance from Stephansdom on Prices") +
  theme_minimal()

# LM with Neighborhood as dummy variables (no pooling)
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

# Random Intercept model (partial-pooling)
fit_lmm_rand_intercept <- lmer(price_dollars ~ dist_stephansdom_km + dist_schonbrunn_km + dist_train_station_km +
                                 room_type + accomodates + bathrooms + cleaning_service +
                                 air_conditioning + self_checkin + host_acceptance_rate +
                                 host_listings_count + number_of_reviews + apt_age_days +
                                 review_scores_rating + reviews_per_month + (1 | neighbourhood), data = data)

summary(fit_lmm_rand_intercept)

fixef(fit_lmm_rand_intercept) 
ranef(fit_lmm_rand_intercept) 

lattice::dotplot(ranef(fit_lmm_rand_intercept))

colors = colorRampPalette(brewer.pal(11, "Spectral"))(21)
sjPlot::plot_model(
  fit_lmm_rand_intercept, 
  type="pred", 
  terms=c("bathrooms", "neighbourhood"), 
  pred.type="re", 
  ci.lvl=NA, 
  color = colors) +
  theme_minimal()

(sigma2_eps <- get_variance_residual(fit_lmm_rand_intercept))
(sigma2_b <- get_variance_random(fit_lmm_rand_intercept))
(PVRE <- sigma2_b/(sigma2_b+sigma2_eps))


# Random Intercept and Slopes Model 
fit_lmm_rand_int_and_slope <- lmer(price_dollars ~ dist_stephansdom_km + dist_schonbrunn_km + dist_train_station_km +
                             room_type + accomodates + bathrooms + cleaning_service +
                             air_conditioning + self_checkin + host_acceptance_rate +
                             host_listings_count + number_of_reviews + apt_age_days +
                             review_scores_rating + reviews_per_month + 
                             (bathrooms | neighbourhood),
                           data = data)
summary (fit_lmm_rand_int_and_slope)

fixef(fit_lmm_rand_int_and_slope)
ranef(fit_lmm_rand_int_and_slope)

lattice::dotplot(ranef(fit_lmm_rand_int_and_slope))

colors = colorRampPalette(brewer.pal(11, "Spectral"))(21)
sjPlot::plot_model(
  fit_lmm_rand_int_and_slope,
  type = "pred",
  terms = c("bathrooms", "neighbourhood"),
  pred.type = "re",
  ci.lvl = NA,
  colors = colors  
) +
  theme_minimal() +
  labs(
    title = "Effect of bathrooms on prices per neighbourhood",
    x = "Number of Bathrooms",
    y = "Predicted price (dollars)",
    color = "Neighbourhood"
  ) 

# Random Slopes model
fit_lmm_rand_slope <- lmer(
  price_dollars ~ dist_stephansdom_km + dist_schonbrunn_km + dist_train_station_km +
    room_type + accomodates + bathrooms + cleaning_service +
    air_conditioning + self_checkin + host_acceptance_rate +
    host_listings_count + number_of_reviews + apt_age_days +
    review_scores_rating + reviews_per_month +
    (bathrooms - 1 | neighbourhood),
  data = data
)

fixef(fit_lmm_rand_slope)
ranef(fit_lmm_rand_slope)

lattice::dotplot(ranef(fit_lmm_rand_slope))

sjPlot::plot_model(
  fit_lmm_rand_slope, 
  type="pred", 
  terms=c("bathrooms", "neighbourhood"), 
  pred.type="re", 
  ci.lvl=NA,
  color = colors)

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

anova(fit_lmm_rand_int_and_slope, fit_lm)
anova(fit_lmm_rand_int_and_slope, fit_lmm_rand_intercept) 

ggplot(data, aes(dist_stephansdom_km, price_dollars)) +
  geom_point() +
  facet_wrap(~ neighbourhood, nrow = 4) +
  geom_smooth(method = 'lm') +
  theme_bw() +
  labs(x = "Distance from Stephansdom (km)", y = "Price ($)") +
  coord_cartesian(ylim = c(0, 360))

ggplot(data, aes(bathrooms, price_dollars)) +
  geom_point() +
  facet_wrap(~ neighbourhood, nrow = 4) +
  geom_smooth(method = 'lm') +
  theme_bw() +
  labs(x = "Distance from Stephansdom (km)", y = "Price ($)") +
  coord_cartesian(ylim = c(0, 360))

### Map visualization
## Random Intercept Model
vienna_shapefile <- st_read("/Users/tommipremoli8/Desktop/Progetti/ams-exam/data/Vienna_Districts Shape/BEZIRKSGRENZEOGDPolygon.shp")
vienna_shapefile$NAMEK <- iconv(vienna_shapefile$NAMEK, from = "latin1", to = "UTF-8")

ranefs <- ranef(fit_lmm_rand_intercept)$neighbourhood
ranefs_df_int <- data.frame(neighbourhood = rownames(ranefs), ranef_value = ranefs[, 1])

vienna_shapefile <- vienna_shapefile %>%
  left_join(ranefs_df_int, by = c("NAMEK" = "neighbourhood"))

ggplot(vienna_shapefile) +
  geom_sf(aes(fill = ranef_value)) +  
  scale_fill_viridis_c(option = "inferno", direction = -1, na.value = "white") +            
  theme_minimal() +                   
  labs(title = "Map of Vienna Districts with Random Effects",
       subtitle = "Random Intercept Model",
       fill = "Random Effect") +
  geom_sf_text(aes(label = NAMEK), size = 3, color = "white", 
               check_overlap = TRUE, nudge_y = 0.5, nudge_x = 0.5) 

## Random Intercept and Slope Model
ranefs_int_slo_mod <- data.frame(
  neighbourhood = c("Alsergrund", "Brigittenau", "Döbling", "Donaustadt", "Favoriten", 
                    "Floridsdorf", "Hernals", "Innere Stadt", "Josefstadt", 
                    "Landstraße", "Leopoldstadt", "Margareten", "Mariahilf", 
                    "Meidling", "Neubau", "Ottakring", "Penzing", "Rudolfsheim-Fünfhaus", 
                    "Simmering", "Währing", "Wieden"),
  ranef_intercept = c(-5.566545, -16.412089, -6.959550, 3.725151, 7.944551,
                      -8.235761, -3.970154, 30.498173, 3.192449,
                      -9.742302, -11.300213, 2.177636, 3.138089,
                      5.940993, 1.918952, -8.242545, 8.681004, -2.371862,
                      4.157574, -4.708673, 6.135123),
  ranef_bathrooms = c(-2.6301249, -4.4912730, 1.8957045, -3.4876559, -8.9780055,
                      -4.5943602, -2.1981523, 23.6977187, 3.1639016,
                      6.5549991, -1.2127378, -7.2452676, 10.1758301,
                      -2.4755066, 9.6090208, -1.8555523, -0.2605348, 0.1347263,
                      -8.2922619, -8.1412043, 0.6307361)
)

vienna_shapefile <- left_join(vienna_shapefile, ranefs_int_slo_mod, by = c("NAMEK" = "neighbourhood"))

ranefs_long <- ranefs_int_slo_mod %>%
  gather(key = "effect", value = "value", -neighbourhood)

vienna_shapefile_long <- left_join(vienna_shapefile, ranefs_long, by = c("NAMEK" = "neighbourhood"))

ggplot(vienna_shapefile_long) +
  geom_sf(aes(fill = value)) + 
  scale_fill_viridis_c(option = "inferno", direction = -1, na.value = "white") +     
  theme_minimal() + 
  facet_wrap(~effect) +         
  labs(title = "Random Effects Map of Vienna Districts",
       subtitle = "Random Intercept and Slope Model",
       fill = "Random Effect") +
  geom_sf_text(aes(label = NAMEK), size = 3, color = "white", 
               check_overlap = TRUE, nudge_y = 0.5, nudge_x = 0.5) 

