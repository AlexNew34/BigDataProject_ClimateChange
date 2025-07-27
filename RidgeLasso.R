#Load packages
pacman::p_load(tidyverse, tidymodels,vip,glmnet,caret,randomForest,datasets, gridExtra, pdp, e1071)

#Dataframe load (impute means for NA values)
df <- read.csv("./Final_Cleaned_CC.csv")
df$aus_freq[is.na(df$aus_freq)] <- mean(df$aus_freq,na.rm = TRUE)
df$aus_land[is.na(df$aus_land)] <- mean(df$aus_land,na.rm = TRUE)
df$gl_avg_sea[is.na(df$gl_avg_sea)] <- mean(df$gl_avg_sea,na.rm = TRUE)
df$gl_land[is.na(df$gl_land)] <- mean(df$gl_land,na.rm = TRUE)

set.seed(123)
### Second model (Ridge and Lasso):
splitIndex <- createDataPartition(df$aus_freq, p = 0.8, list = FALSE)
train_df <- df[splitIndex, ]
test_df  <- df[-splitIndex, ]
#Split training data into X values (independent variables) and y (dependent variables).
train_X <- as.matrix(train_df[, setdiff(names(df), "aus_freq")])
train_y <- train_df$aus_freq
#Split testing data into X values (independent variables) and y (dependent variables).
test_X <- as.matrix(test_df[, setdiff(names(df), "aus_freq")])
test_y <- test_df$aus_freq

#Ridge Regression (alpha = 0)
#Create a cross-validation for fitting generalized linear models.
cv_model <- cv.glmnet(train_X, train_y, alpha = 0)
#Find best lambda
best_lambda <- cv_model$lambda.min
cat("Best lambda:", best_lambda, "\n")
#Use best lambda for final ridge regression model.
final_model_ridge <- glmnet(train_X, train_y, alpha = 0, lambda = best_lambda)
cat("Ridge coefficients:\n")
print(coef(final_model_ridge))
#Calculate the RMSE, MAE, and Rsquared.
predictions <- predict(final_model_ridge, newx = test_X)
rmse <- sqrt(mean((test_y - predictions)^2))
mae <- mean(abs(test_y - predictions))
rsqed <- final_model_ridge$dev.ratio
cat("RMSE on testing data:", rmse, "\n")
cat("MAE on testing data:", mae, "\n")
cat("RSQ:", rsqed, "\n")

#######################################
#Third model Lasso regression (alpha = 1)
cv_model <- cv.glmnet(train_X, train_y, alpha = 1)
best_lambda <- cv_model$lambda.min
cat("Best lambda:", best_lambda, "\n")
final_model_lasso <- glmnet(train_X, train_y, alpha = 1, lambda = best_lambda)
cat("Lasso coefficients:\n")
print(coef(final_model_lasso))
predictions <- predict(final_model_lasso, newx = test_X)
rmse <- sqrt(mean((test_y - predictions)^2))
mae <- mean(abs(test_y - predictions))
rsqed <- final_model_lasso$dev.ratio
cat("RMSE on testing data:", rmse, "\n")
cat("MAE on testing data:", mae, "\n")
cat("RSQ:", rsqed, "\n")
