library(tidyverse)
library(MASS)
library(robustbase)
library(olsrr)
library(caret)
library(glmnet)
library(car)
library(lmtest)
library(randomForest)
library(xgboost)
library(e1071)


############################ IMPORT DATA #######################################

set.seed(123)

data <- read_csv("https://raw.githubusercontent.com/lucasangio01/vienna-airbnb/refs/heads/main/data/vienna_listings_no_outliers.csv")
View(data)

regression_variables <- data %>%
  dplyr::select("price_dollars", "dist_stephansdom_km":"dist_train_station_km", "room_type":"reviews_per_month")
View(regression_variables)

numeric_variables <- regression_variables[sapply(regression_variables, is.numeric)]
View(numeric_variables)

regr_split <- createDataPartition(regression_variables$price_dollars, p = 0.75, list = FALSE)
regr_trainset <- regression_variables[regr_split, ]
regr_testset <- regression_variables[-regr_split, ]

num_regr_split <- createDataPartition(numeric_variables$price_dollars, p = 0.75, list = FALSE)
num_regr_trainset <- numeric_variables[num_regr_split, ]
num_regr_testset <- numeric_variables[-num_regr_split, ]



############################## LINEAR MODEL ####################################

linear_model <- lm(data = regr_trainset, formula = price_dollars ~ .)
linear_pred <- predict(linear_model, regr_testset)
summary(linear_model)

vif(linear_model)
dwtest(linear_model)

plot(linear_model)
ols_plot_resid_lev(linear_model)

linear_eval <- cbind(regr_testset$price_dollars, linear_pred)
colnames(linear_eval) <- c("Actual", "Predicted")
linear_eval <- as.data.frame(linear_eval)

ggplot(linear_eval, aes(x = linear_pred, y = regr_testset$price_dollars)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red") + 
  labs(title = "Linear Model - Predicted vs actual values", x = "Predicted value", y = "Actual value")

linear_rsq <- 1 - (sum((regr_testset$price_dollars - linear_pred)^2) / sum((regr_testset$price_dollars - mean(regr_testset$price_dollars))^2))
linear_residuals <- regr_testset$price_dollars - linear_pred
linear_rmse <- sqrt(mean(linear_residuals^2))



######################### ROBUST MODEL - HUBER #################################

robust_huber <- rlm(price_dollars ~ ., data = regr_trainset, psi = psi.huber)
huber_pred <- predict(robust_huber, regr_testset)
summary(robust_huber)

vif(robust_huber)
plot(robust_huber)
ols_plot_resid_lev(robust_huber)

huber_eval <- cbind(regr_testset$price_dollars, huber_pred)
colnames(huber_eval) <- c("Actual", "Predicted")
huber_eval <- as.data.frame(huber_eval)

ggplot(huber_eval, aes(x = huber_eval$Predicted, y = huber_eval$Actual)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red", lwd = 1) +
  labs(title = "Huber Robust Model - Predicted vs Actual Values", x = "Predicted Value", y = "Actual Value")

huber_rsq <- 1 - (sum((regr_testset$price_dollars - huber_pred)^2) / sum((regr_testset$price_dollars - mean(regr_testset$price_dollars))^2))
huber_residuals <- regr_testset$price_dollars - huber_pred
huber_rmse <- sqrt(mean(huber_residuals^2))


################## ROBUST MODEL - BISQUARE #####################################

robust_bisquare <- rlm(price_dollars ~ ., data = regr_trainset, psi = psi.bisquare)
bisquare_pred <- predict(robust_bisquare, regr_testset)
summary(robust_bisquare)

plot(robust_bisquare)
ols_plot_resid_lev(robust_bisquare)

bisquare_eval <- cbind(regr_testset$price_dollars, bisquare_pred)
colnames(bisquare_eval) <- c("Actual", "Predicted")
bisquare_eval <- as.data.frame(bisquare_eval)

ggplot(bisquare_eval, aes(x = bisquare_eval$Predicted, y = bisquare_eval$Actual)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  labs(title = "Bisquare Robust Model - Predicted vs Actual Values", x = "Predicted Value", y = "Actual Value")

bisquare_rsq <- 1 - (sum((regr_testset$price_dollars - bisquare_pred)^2) / sum((regr_testset$price_dollars - mean(regr_testset$price_dollars))^2))
bisquare_residuals <- regr_testset$price_dollars - bisquare_pred
bisquare_rmse <- sqrt(mean(bisquare_residuals^2))


############################# RANDOM FOREST ####################################

random_forest <- randomForest(price_dollars ~ ., data = regr_trainset, ntree = 1000, importance = TRUE)
rf_pred <- predict(random_forest, regr_testset)

rf_eval <- cbind(regr_testset$price_dollars, rf_pred)
colnames(rf_eval) <- c("Actual", "Predicted")
rf_eval <- as.data.frame(rf_eval)

ggplot(rf_eval, aes(x = rf_eval$Predicted, y = rf_eval$Actual)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  labs(title = "Random Forest Model - Predicted vs Actual Values", x = "Predicted Value", y = "Actual Value")

rf_rsq <- 1 - (sum((regr_testset$price_dollars - rf_pred)^2) / sum((regr_testset$price_dollars - mean(regr_testset$price_dollars))^2))
rf_residuals <- regr_testset$price_dollars - rf_pred
rf_rmse <- sqrt(mean(rf_residuals^2))

rf.vimp <- varImp(random_forest)


############################# XGBOOST ##########################################

xgb_xtrain <- as.matrix(num_regr_trainset %>% dplyr::select(-price_dollars))
xgb_ytrain <- num_regr_trainset$price_dollars

xgb_xtest <- as.matrix(num_regr_testset %>% dplyr::select(-price_dollars))
xgb_ytest <- num_regr_testset$price_dollars

xgb_train_data <- xgb.DMatrix(data = xgb_xtrain, label = xgb_ytrain)
xgb_test_data <- xgb.DMatrix(data = xgb_xtest, label = xgb_ytest)

xgb_params <- list(
  objective = "reg:squarederror", booster = "gbtree", eta = 0.1, max_depth = 6, 
  subsample = 0.80, colsample_bytree = 0.7, eval_metric = "rmse")

xgb_model <- xgb.train(params = xgb_params, data = xgb_train_data, nrounds = 100,
  watchlist = list(train = xgb_train_data, test = xgb_test_data), early_stopping_rounds = 10,
  print_every_n = 10)

xgb_pred <- predict(xgb_model, xgb_xtest)
xgb_eval <- cbind(num_regr_testset$price_dollars, xgb_pred)
colnames(xgb_eval) <- c("Actual", "Predicted")
xgb_eval <- as.data.frame(xgb_eval)

ggplot(xgb_eval, aes(x = xgb_eval$Predicted, y = xgb_eval$Actual)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  labs(title = "XGBoost Model - Predicted vs Actual Values", x = "Predicted Value", y = "Actual Value")

xgb_rsq <- 1 - (sum((num_regr_testset$price_dollars - xgb_pred)^2) / sum((num_regr_testset$price_dollars - mean(num_regr_testset$price_dollars))^2))
xgb_residuals <- regr_testset$price_dollars - xgb_pred
xgb_rmse <- sqrt(mean(xgb_residuals^2))


#################################### SVR #######################################

svr_model <- svm(price_dollars ~ ., data = regr_trainset, type = "eps-regression", 
                 kernel = "radial", cost = 10, epsilon = 0.1)
svr_pred <- predict(svr_model, regr_testset)
svr_eval <- cbind(regr_testset$price_dollars, svr_pred)
colnames(svr_eval) <- c("Actual", "Predicted")
svr_eval <- as.data.frame(svr_eval)

ggplot(svr_eval, aes(x = svr_eval$Predicted, y = svr_eval$Actual)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  labs(title = "Support Vector Regression Model - Predicted vs Actual Values", x = "Predicted Value", y = "Actual Value")

svr_rsq <- 1 - (sum((regr_testset$price_dollars - svr_pred)^2) / sum((regr_testset$price_dollars - mean(regr_testset$price_dollars))^2))
svr_residuals <- regr_testset$price_dollars - svr_pred
svr_rmse <- sqrt(mean(svr_residuals^2))


########################### OTHER ##############################################

variables1 <- c("(Intercept)", "dist_stephansdom_km", "dist_schonbrunn_km", "dist_train_station_km", 
               "room_typePrivate room", "accomodates", "bathrooms", "cleaning_service", 
               "air_conditioning", "self_checkin", "host_acceptance_rate", 
               "host_listings_count", "number_of_reviews", "apt_age_days", 
               "review_scores_rating", "reviews_per_month")
variables2 <- c("Huber robust model", "Bisquare robust model")
rsquares1 <- c(huber_rsq, bisquare_rsq)
rmse1 <- c(huber_rmse, bisquare_rmse)

huber_p_values <- c(0.6892, 0.0000, 7.4e-08, 0.0614, 0.0000, 0.0000, 0.0000, 0.0077, 
              0.0000, 0.0002, 0.0000, 0.0006, 0.2566, 3.1e-08, 0.0000, 0.0000)
huber_significance <- c(" ", "***", "***", ".", "***", "***", "***", "**", "***", 
                  "***", "***", "***", " ", "***", "***", "***")
huber_regression_summary <- data.frame(Variable = variables1, PValue = huber_p_values, Significance = huber_significance)
huber_vs_bisquare <- data.frame(Variable = variables2, RSquare = rsquares1, RMSE = rmse1)


new_models1 <- c("XGBoost", "Bisquare robust", "Huber robust", "SVR", "Random Forest")
new_models2 <- c("Random Forest", "XGBoost", "SVR", "Huber robust", "Bisquare robust")
rsq_values <- c(rf_rsq, xgb_rsq, svr_rsq, huber_rsq, bisquare_rsq)
rmse_values <- c(xgb_rmse, bisquare_rmse, huber_rmse, svr_rmse, rf_rmse)

rsq_plot <- barplot(rsq_values, names.arg = new_models2, col = c("firebrick4", "darkorange3", "gold4", "darkgreen", "darkslateblue"),
  main = "R-Squared Comparison", xlab = "", ylab = "R-Squared", ylim = c(0, 1), cex.names = 0.8)
text(x = rsq_plot, y = rsq_values, label = round(rsq_values, 3), pos = 3, cex = 0.8)

rmse_plot <- barplot(rmse_values, names.arg = new_models1, col = c("darkorange3", "darkslateblue", "darkgreen", "gold4", "firebrick4"),
  main = "RMSE Comparison", xlab = "", ylab = "RMSE", ylim = c(0, max(rmse_values) + 10), cex.names = 0.8)
text(x = rmse_plot, y = rmse_values, label = round(rmse_values, 2), pos = 3, cex = 0.8)

################################################################################

# possible plots to add:
#
# huber & bisquare (all data)
# random forest (all data)
# xgboost (only numeric)
# svr (all data)
