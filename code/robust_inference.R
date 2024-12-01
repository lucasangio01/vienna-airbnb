library(tidyverse)
library(MASS)
library(robustbase)
library(olsrr)
library(caret)
library(glmnet)
library(car)


############################ IMPORT DATA #######################################


data <- read_csv(".//data/vienna_listings_no_outliers.csv")
View(data)

regression_variables <- data %>%
  dplyr::select("price_dollars", "dist_stephansdom_km":"dist_train_station_km", "room_type":"reviews_per_month")
View(regression_variables)

numeric_variables <- regression_variables[sapply(regression_variables, is.numeric)]
View(numeric_variables)


####################### MODELS #################################################


# Linear model

linear_model <- lm(data = regression_variables, formula = price_dollars ~ .)
summary(linear_model)
plot(linear_model)
rse_linear <- summary(linear_model)$sigma
residuals_linear <- linear_model$residuals
ols_plot_resid_lev(linear_model)

control <- trainControl(method = "cv", number = 10)
cv_linear <- train(price_dollars ~ ., data = data, method = "lm", trControl = control)
cv_linear$results$RMSE


# Robust models

robust_huber <- rlm(price_dollars ~ ., data = data, psi = psi.huber)
summary(robust_huber)
hu_weights2 <- hu_weights[order(robust_huber$w), ]
hu_weights2[1:15, ]
rse_robust_huber <- summary(robust_huber)$sigma
residuals_robust_huber <- robust_huber$residuals

robust_bisquare <- rlm(price_dollars ~ ., data = data, psi = psi.bisquare)
summary(robust_bisquare)
bi_weights <- data.frame(neighbourhood = data$neighbourhood, resid = robust_bisquare$resid, weight = robust_bisquare$w)
bi_weights2 <- bi_weights[order(robust_bisquare$w), ]
bi_weights2[1:15, ]
rse_robust_bisquare <- summary(robust_bisquare)$sigma
residuals_robust_bisquare <- robust_bisquare$residuals

robust_mcd <- covMcd(x = numeric_variables, alpha = 0.75)


# Linear model - Step selection

linear_model_selected <- stepAIC(linear_model, direction = "both", trace = FALSE)
summary(linear_model_selected)
selected_formula <- formula(linear_model_selected)
cv_linear_selected <- train(selected_formula, data = data, method = "lm", trControl = control)
cv_linear_selected$results$RMSE


# Robust models - Step selection

robust_huber_selected <- rlm(selected_formula, data = data, psi = psi.huber)
summary(robust_huber_selected)

robust_bisquare_selected <- rlm(selected_formula, data = data, psi = psi.bisquare)
summary(robust_bisquare_selected)
