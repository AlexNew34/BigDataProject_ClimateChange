#Load packages
pacman::p_load(tidyverse, tidymodels,vip,glmnet,caret,randomForest,datasets, gridExtra, pdp, e1071)

#Dataframe load (impute means for NA values)
df <- read.csv("./Final_Cleaned_CC.csv")
df$aus_freq[is.na(df$aus_freq)] <- mean(df$aus_freq,na.rm = TRUE)
df$aus_land[is.na(df$aus_land)] <- mean(df$aus_land,na.rm = TRUE)
df$gl_avg_sea[is.na(df$gl_avg_sea)] <- mean(df$gl_avg_sea,na.rm = TRUE)
df$gl_land[is.na(df$gl_land)] <- mean(df$gl_land,na.rm = TRUE)

set.seed(123)
#Split into training set and testing set (80%).
trainIndex <- createDataPartition(df$aus_freq, p = 0.8, list = FALSE)
traindf <- df[trainIndex, ]
testdf <- df[-trainIndex, ]

#Create a full random forest model.
rf_model <- randomForest(aus_freq ~ ., data = traindf)
print(rf_model)

#Create 10 cross-validations for hyperparameter fitting in random forest.
trainControl <- trainControl(method = "cv", number = 10, search = "grid")
tuneGrid <- expand.grid(
  .mtry = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
)

#Use training data to fit a full random forest model while tuning, via RMSE values.
tuned_rf <- train(aus_freq ~ ., data = traindf, method = "rf", 
                  trControl = trainControl, tuneGrid = tuneGrid, metric = "RMSE")

#Show best results
print(tuned_rf)

#Create the random forest model.
rf_model <- randomForest(aus_freq ~ ., data = traindf, mtry = 4, importance = TRUE)

#Show importance of each variable.
importance_values <- importance(rf_model)
print(importance_values)
varImpPlot(rf_model)

#Calculate RMSE and MAE.
predictions <- predict(tuned_rf, newdata = testdf)
rmse <- sqrt(mean((testdf$aus_freq - predictions)^2))
mae <- mean(abs(testdf$aus_freq - predictions))
cat("RMSE on testing data:", rmse, "\n")
cat("MAE on testing data:", mae, "\n")
