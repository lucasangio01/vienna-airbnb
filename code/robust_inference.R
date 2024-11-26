library(tidyverse)
library(MASS)
library(robustbase)
library(olsrr)
library(caret)
library(glmnet)
library(car)


############################ IMPORT DATA #######################################

data_regression <- read_csv("C:/Users/sangi/Documents/GitHub/ams-exam//data/vienna_listings_final.csv") %>%
  dplyr::select(ID:review_scores_rating, reviews_per_month)
View(data_regression)

regression_variables <- data_regression %>%
  dplyr::select("price_dollars", "dist_stephansdom_km":"dist_train_station_km", "room_type":"reviews_per_month")


####################### MODELS WITH OUTLIERS ###################################


# Linear model

linear_model <- lm(data = regression_variables, formula = price_dollars ~ .)
summary(linear_model)
plot(linear_model)
rse_linear <- summary(linear_model)$sigma
residuals_linear <- linear_model$residuals
ols_plot_resid_lev(linear_model)

control <- trainControl(method = "cv", number = 10)
cv_linear <- train(price_dollars ~ ., data = data_regression, method = "lm", trControl = control)
cv_linear$results$RMSE


# Robust models

robust_huber <- rlm(price_dollars ~ ., data = data_regression, psi = psi.huber)
summary(robust_huber)
hu_weights <- data.frame(neighbourhood = data_regression$neighbourhood, resid = robust_huber$resid, weight = robust_huber$w)
hu_weights2 <- hu_weights[order(robust_huber$w), ]
hu_weights2[1:15, ]
rse_robust_huber <- summary(robust_huber)$sigma
residuals_robust_huber <- robust_huber$residuals

robust_bisquare <- rlm(price_dollars ~ ., data = data_regression, psi = psi.bisquare)
summary(robust_bisquare)
bi_weights <- data.frame(neighbourhood = data_regression$neighbourhood, resid = robust_bisquare$resid, weight = robust_bisquare$w)
bi_weights2 <- bi_weights[order(robust_bisquare$w), ]
bi_weights2[1:15, ]
rse_robust_bisquare <- summary(robust_bisquare)$sigma
residuals_robust_bisquare <- robust_bisquare$residuals


# Linear model - Step selection

linear_model_selected <- stepAIC(linear_model, direction = "both", trace = FALSE)
summary(linear_model_selected)
selected_formula <- formula(linear_model_selected)
cv_linear_selected <- train(selected_formula, data = data_regression, method = "lm", trControl = control)
cv_linear_selected$results$RMSE


# Robust models - Step selection

robust_huber_selected <- rlm(selected_formula, data = data_regression, psi = psi.huber)
summary(robust_huber_selected)

robust_bisquare_selected <- rlm(selected_formula, data = data_regression, psi = psi.bisquare)
summary(robust_bisquare_selected)


####################### REMOVE OUTLIERS ########################################


cooks_distance <- cooks.distance(linear_model)
abs_std_res <- abs(stdres(linear_model))
threshold_cook <- 4 / nrow(data_regression)
residual_threshold <- 2
leverage_values <- hatvalues(linear_model)
predictors <- setdiff(names(data_regression), "price_dollars")
studentized_residuals <- rstudent(linear_model)
z_scores <- (data_regression$price_dollars - mean(data_regression$price_dollars)) / sd(data_regression$price_dollars)

outliers_manual_rows <- c(4374, 4375, 4376, 4573, 7159, 8580)
outliers_cook <- which(cooks_distance > threshold_cook)
outliers_residual <- which(abs_std_res > residual_threshold)
outliers_leverage <- which(leverage_values > (2 * length(coefficients(linear_model)) / nrow(data_regression)))
outliers_studres <- which(abs(studentized_residuals) > 3)
outliers_z_score <- which(abs(z_scores) > 3)

all_outliers <- union(outliers_manual_rows, union(outliers_cook, union(outliers_residual, union(outliers_leverage, union(outliers_z_score, outliers_studres)))))

data_no_outliers <- data_regression[-all_outliers, ]


######################## MODELS WITHOUT OUTLIERS ###############################


# Linear model

regression_variables_no_outliers <- data_no_outliers %>%
  dplyr::select("price_dollars", "dist_stephansdom_km":"dist_train_station_km", "room_type":"reviews_per_month")

linear_model_no_outliers <- lm(price_dollars ~ ., data = regression_variables_no_outliers)
summary(linear_model_no_outliers)
ols_plot_resid_lev(linear_model_no_outliers)

cv_linear_no_outliers <- train(price_dollars ~ ., data = data_no_outliers, method = "lm", trControl = control)
cv_linear_no_outliers$results$RMSE


# Robust model

robust_huber_no_outliers <- rlm(price_dollars ~ ., data = data_no_outliers, psi = psi.huber)
summary(robust_huber_no_outliers)


# Linear model - Step selection

linear_model_selected_no_outliers <- stepAIC(linear_model_no_outliers, direction = "both", trace = FALSE)
summary(linear_model_selected_no_outliers)
selected_formula_no_outliers <- formula(linear_model_selected_no_outliers)

cv_linear_selected_no_outliers <- train(selected_formula_no_outliers, data = data_no_outliers, method = "lm", trControl = control)
cv_linear_selected_no_outliers$results$RMSE
