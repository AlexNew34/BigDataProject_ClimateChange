#Load packages
pacman::p_load(MASS,tidymodels, tidyr, rpart, rpart.plot)

#Dataframe load (impute means for NA values)
df <- read.csv("./Final_Cleaned_CC.csv")
df$aus_freq[is.na(df$aus_freq)] <- mean(df$aus_freq,na.rm = TRUE)
df$aus_land[is.na(df$aus_land)] <- mean(df$aus_land,na.rm = TRUE)
df$gl_avg_sea[is.na(df$gl_avg_sea)] <- mean(df$gl_avg_sea,na.rm = TRUE)
df$gl_land[is.na(df$gl_land)] <- mean(df$gl_land,na.rm = TRUE)

set.seed(123)
#Split into training set and testing set (80%).
data_split <- initial_split(df, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

#Make a decision tree regression model
tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("regression")
tree_fit <- tree_spec %>%
  fit(aus_freq ~ ., data = train_data)

#Calculate RMSE, R-squared and MAE
predictions <- tree_fit %>%
  predict(test_data) %>%
  pull(.pred)
metrics <- metric_set(rmse, rsq, mae)
model_performance <- test_data %>%
  mutate(predictions = predictions) %>%
  metrics(truth = aus_freq, estimate = predictions)
#Show the RMSE, Rsquared and MAE.
print(model_performance)
#Show decision tree rules or plot.
rpart.plot(tree_fit$fit, type = 4, extra = 101, under = TRUE, cex = 0.8, box.palette = "auto")
rules <- rpart.rules(tree_fit$fit)
print(rules)
