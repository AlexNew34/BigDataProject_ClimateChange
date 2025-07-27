#Load packages.
pacman::p_load(tidyverse, tidymodels,vip,glmnet,caret,randomForest,datasets, gridExtra, pdp, e1071)

#Dataframe load (impute means for NA values)
df <- read.csv("./Final_Cleaned_CC.csv")
df$aus_freq[is.na(df$aus_freq)] <- mean(df$aus_freq,na.rm = TRUE)
df$aus_land[is.na(df$aus_land)] <- mean(df$aus_land,na.rm = TRUE)
df$gl_avg_sea[is.na(df$gl_avg_sea)] <- mean(df$gl_avg_sea,na.rm = TRUE)
df$gl_land[is.na(df$gl_land)] <- mean(df$gl_land,na.rm = TRUE)

### First model (Simple Linear Regression):
set.seed(123)
#Split into training set and testing set (80%).
splitIndex <- createDataPartition(df$aus_freq, p = 0.8, list = FALSE)
train_df <- df[splitIndex, ]
test_df  <- df[-splitIndex, ]

#Full linear regression model.
lm1 <- lm(aus_freq ~ ., data = df)
#Remove until statistical significant variables left.
lm2 <- lm(aus_freq ~ . -(gl_avg_carbon +aus_land + year + aus_avg_prec + gl_freq + gl_land + gl_avg_prec + aus_avg_temp), data = train_df)
summary(lm1)
summary(lm2)
#Calculate RMSE and MAE.
predictions <- predict(lm2, newdata = test_df)
rmse <- sqrt(mean((test_df$aus_freq - predictions)^2))
mae <- mean(abs(test_df$aus_freq - predictions))
cat("RMSE on testing data:", rmse, "\n")
cat("MAE on testing data:", mae, "\n")
