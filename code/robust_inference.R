library(tidyverse)
library(MASS)
library(robustbase)
library(olsrr)
library(caret)
library(glmnet)


############################ IMPORT DATA #######################################

data <- read_csv("./data/vienna_listings_final.csv")
View(data)


####################### MODELS WITH OUTLIERS ###################################


# Linear model

linear_model <- lm(data = data, formula = price_dollars ~ .)
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
hu_weights <- data.frame(neighbourhood = data$neighbourhood, resid = robust_huber$resid, weight = robust_huber$w)
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


####################### REMOVE OUTLIERS ########################################


cooks_distance <- cooks.distance(linear_model)
abs_std_res <- abs(stdres(linear_model))
threshold_cook <- 4 / nrow(data)
residual_threshold <- 2
leverage_values <- hatvalues(linear_model)
predictors <- setdiff(names(data), "price_dollars")
studentized_residuals <- rstudent(linear_model)
z_scores <- (data$price_dollars - mean(data$price_dollars)) / sd(data$price_dollars)

outliers_manual_rows <- c(4374, 4375, 4376, 4573, 7159, 8580)
outliers_cook <- which(cooks_distance > threshold_cook)
outliers_residual <- which(abs_std_res > residual_threshold)
outliers_leverage <- which(leverage_values > (2 * length(coefficients(linear_model)) / nrow(data)))
outliers_studres <- which(abs(studentized_residuals) > 3)
outliers_z_score <- which(abs(z_scores) > 3)

all_outliers <- union(outliers_manual_rows, union(outliers_cook, union(outliers_residual, union(outliers_leverage, union(outliers_z_score, outliers_studres)))))

data_no_outliers <- data[-all_outliers, ]

write.csv(data_no_outliers, "./data/vienna_listings_no_outliers.csv", row.names = FALSE)


######################## MODELS WITHOUT OUTLIERS ###############################


# Linear model

linear_model_no_outliers <- lm(price_dollars ~ ., data = data_no_outliers)
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
