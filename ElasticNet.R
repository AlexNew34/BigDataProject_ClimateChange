#Load packages
pacman::p_load(tidyverse, tidymodels,vip,glmnet,caret,randomForest,datasets, gridExtra, pdp, e1071)

#Dataframe load (impute means for NA values)
df <- read.csv("./Final_Cleaned_CC.csv")
df$aus_freq[is.na(df$aus_freq)] <- mean(df$aus_freq,na.rm = TRUE)
df$aus_land[is.na(df$aus_land)] <- mean(df$aus_land,na.rm = TRUE)
df$gl_avg_sea[is.na(df$gl_avg_sea)] <- mean(df$gl_avg_sea,na.rm = TRUE)
df$gl_land[is.na(df$gl_land)] <- mean(df$gl_land,na.rm = TRUE)

set.seed(123)
splitIndex <- createDataPartition(df$aus_freq, p = 0.8, list = FALSE)
train_df <- df[splitIndex, ]
test_df  <- df[-splitIndex, ]

#Split training data into X values (independent variables) and y (dependent variables).
train_X <- as.matrix(train_df[, setdiff(names(df), "aus_freq")])
train_y <- train_df$aus_freq
#Split testing data into X values (independent variables) and y (dependent variables).
test_X <- as.matrix(test_df[, setdiff(names(df), "aus_freq")])
test_y <- test_df$aus_freq

### Fourth model (Elastic Net Regression).
#Create 5 cross-validations for fitting generalized linear models.
train_control <- trainControl(method = "cv", number = 5)
#Create all possible wanted values to be used to tune the model.
tune_grid <- expand.grid(
  alpha = seq(0, 1, by = 0.05),
  lambda = seq(0, 10, by = 0.2)
)

#Tune the model via RMSE.
tuned_model <- train(
  x = train_X,
  y = train_y,
  method = "glmnet",
  trControl = train_control,
  tuneGrid = tune_grid,
  metric = "RMSE"
)

cat("Best alpha:", tuned_model$bestTune$alpha, "\n")
cat("Best lambda:", tuned_model$bestTune$lambda, "\n")
cat("Best RMSE:", min(tuned_model$results$RMSE), "\n")
#Calculate RMSE, MAE and Rsquared.
final_predictions <- predict(tuned_model, newdata = test_X)
rmse <- sqrt(mean((test_y - final_predictions)^2))
mae <- mean(abs(test_y - final_predictions))
tuned_model$bestTune
rsqu<-tuned_model$results[971,]
cat("RMSE on testing data:", rmse, "\n")
cat("MAE on testing data:", mae, "\n")
plot(tuned_model)
#Print coefficients of the tuned model.
best_tune <- tuned_model$bestTune
best_alpha <- best_tune$alpha
best_lambda <- best_tune$lambda
coefficients <- coef(tuned_model$finalModel, s = best_lambda)
print(coefficients)
