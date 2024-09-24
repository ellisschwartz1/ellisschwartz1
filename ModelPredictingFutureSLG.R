library(tidyverse)
library(dslabs)
library(dplyr)
install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
require(xgboost)
require(caTools)
require(caret)
require(car)
library(corrplot)
library(glmnet)
require(randomForest)
require(gbm)
library(e1071)
library(class)
library(mgcv)
setwd("/Users/ellisschwartz/Desktop/80grade/Slug")
slug = read_csv("/Users/ellisschwartz/Desktop/80grade/Slug/fulltest.csv")
baseline = read_csv("/Users/ellisschwartz/Desktop/80grade/Slug/23vs24slg.csv")
#finding baseline prediction of how well last years slugging predicts this years
baseline_wide <- baseline|>
  mutate(year = paste0("slg_", substr(year, 3, 4)))|>
  pivot_wider(names_from = year, values_from = slg_percent)
slg_one <- baseline_wide|>
  filter(!is.na(slg_22) & !is.na(slg_23))|>
  select(-slg_24)
slg_two <- baseline_wide|>
  filter(!is.na(slg_23) & !is.na(slg_24))|>
  select(-slg_22)
slg_one<-slg_one|>
  select(-`last_name, first_name`,-player_id)|>
  rename(slg_last=slg_22,slg_this=slg_23)
slg_two<-slg_two|>
  select(-`last_name, first_name`,-player_id)|>
  rename(slg_last=slg_23,slg_this=slg_24)
slg_comb <- rbind(slg_one,slg_two)
set.seed(123)
train_indices_base <- sample(seq_len(nrow(slg_comb)), size = 0.8 * nrow(slg_comb))
train_data_base <- slg_comb[train_indices_base, ]
test_data_base <- slg_comb[-train_indices_base, ]
baseline_lm <- lm(slg_this ~ slg_last, data = train_data_base)
summary(baseline_lm)
confint(baseline_lm)
predictions_base <- predict(baseline_lm, newdata = test_data_base)
mse_base <- mean((test_data_base$slg_this - predictions_base)^2)
rmse_base <- sqrt(mse_base)
print(mse_base)
print(rmse_base)
#the baseline model has an adjusted R^2 of .1903, very low 
#linear model with added variables
slug_new<-slug|>
  filter(player_age > 22 & player_age < 37 & year != 2024)|>
  select(-`last_name, first_name`)
#creating a dataframe with this years slg and metrics and next years slg
slg_2122 <- slug_new|>
  filter(year != 2023)
p_ids_12 <- slg_2122 |>
  group_by(player_id) |>
  filter(year %in% c(2021, 2022)) |>
  summarise(n_years = n_distinct(year)) |>
  filter(n_years == 2)|>
  pull(player_id)
df_2021 <- slg_2122 |>
  filter(year == 2021, player_id %in% p_ids_12) |>
  select(player_id, everything())|>
  rename(slg_this = slg_percent)
df_2022 <- slg_2122 |>
  filter(year == 2022, player_id %in% p_ids_12) |>
  select(player_id, slg_percent) |>
  rename(slg_next = slg_percent)
df_12 <- df_2021 %>%
  left_join(df_2022, by = "player_id")|>
  select(-year,-player_id)
#same for 22-23
slg_2223 <- slug_new|>
  filter(year != 2021)
p_ids_23 <- slg_2223 |>
  group_by(player_id) |>
  filter(year %in% c(2022, 2023)) |>
  summarise(n_years = n_distinct(year)) |>
  filter(n_years == 2)|>
  pull(player_id)
df_2022_2 <- slg_2223 |>
  filter(year == 2022, player_id %in% p_ids_23) |>
  select(player_id, everything())|>
  rename(slg_this = slg_percent)
df_2023 <- slg_2223 |>
  filter(year == 2023, player_id %in% p_ids_23) |>
  select(player_id, slg_percent) |>
  rename(slg_next = slg_percent)
df_23 <- df_2022_2 %>%
  left_join(df_2023, by = "player_id")|>
  select(-year,-player_id)
slug_new<-rbind(df_12,df_23)
#now i have my data for my modeling
slug|>
  ggplot(aes(x=player_age,y=slg_percent))+geom_point()

d<- slug_new|>
  group_by(player_age)|>
  summarize(avgslug=mean(slg_this), players = n())
# the peak for avg slg for players comes between ages 28 and 31
slug_new|>
  ggplot(aes(x=oz_contact_percent,y=slg_this))+geom_point()+geom_smooth()

d|>
  filter(player_age>22 & player_age < 37)|>
  ggplot(aes(x=player_age,y=avgslug))+geom_line()+geom_smooth()
slug_new|>
  ggplot(aes(x=launch_angle_avg, y=slg_this))+geom_point()+geom_smooth()
colSums(is.na(slug))
slug_new <- slug_new|>
  select(-xslg,-exit_velocity_avg,-barrel_batted_rate,-avg_best_speed,-flyballs_percent)
#linear regression model
set.seed(123)
train_indices <- sample(seq_len(nrow(slug_new)), size = 0.8 * nrow(slug_new))
train_data <- slug_new[train_indices, ]
test_data <- slug_new[-train_indices, ]

linmod<-lm(slg_next ~ player_age + slg_this + launch_angle_avg + solidcontact_percent + hard_hit_percent + oz_contact_percent + meatball_swing_percent + iz_contact_percent + pull_percent + linedrives_percent,data=train_data)
summary(linmod)  
#looking for multicollinearity
vif(linmod)
num<-slug_new[sapply(slug_new,is.numeric)]
dd<- cor(num)
#correlation matrix
corrplot(dd, method = "color", type = "upper", tl.col = "black", tl.cex = 0.8)
#test set
predictions_lin <- predict(linmod, newdata = test_data)
mse_lin <- mean((test_data$slg_next - predictions_lin)^2)
rmse_lin <- sqrt(mse_lin)
print(mse_lin)
print(rmse_lin)
#ridge regression
x_train <- train_data|>
  select(-slg_next)
x_test <- test_data|>
  select(-slg_next)
y_train <- train_data$slg_next
y_test <- test_data$slg_next
x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)
model_ridge <- cv.glmnet(x_train,y_train, alpha = 0)
best_lambda_ridge <- model_ridge$lambda.min
print(best_lambda)
predictions_ridge<- predict(model_ridge, s = best_lambda_ridge, newx = x_test)
predictions_ridge <- as.vector(predictions_ridge)
mse_ridge <- mean((y_test - predictions_ridge)^2)
rmse_ridge <- sqrt(mse_ridge)
print(mse_ridge)
print(rmse_ridge)
#lower mse and rmse, need cross validation
#refitting ridge model
ridge_model_best <- glmnet(x_train, y_train, alpha = 0, lambda = best_lambda_ridge)
predictions_ridge_new <- predict(ridge_model_best, newx = x_test)
predictions_ridge_new <- as.vector(predictions_ridge_new)
mse_ridge_new <- mean((y_test - predictions_ridge_new)^2)
rmse_ridge_new <- sqrt(mse_ridge_new)
print(mse_ridge_new)
print(rmse_ridge_new)
#the mse and rmse for the ridge model are better than the baseline model 
#random forest
rf_model <- randomForest(x=x_train,y=y_train)
print(rf_model)
rf_predictions <- predict(rf_model, newdata = x_test)
accuracy <- mean(rf_predictions == y_test)
print(accuracy)
mse_rf <- mean((y_test - rf_predictions)^2)
print(mse_rf)
rmse_rf <- sqrt(mse_rf)
print(rmse_rf)
#gbm
gbm_model <- gbm(
  formula = slg ~ .,
  distribution = "gaussian",
  data = data.frame(x_train, slg = y_train),
  n.trees = 1000,
  interaction.depth = 3,
  shrinkage = 0.01,
  bag.fraction = 0.5,
  n.minobsinnode = 10,
  cv.folds = 5,
  verbose = TRUE)
gbm_predictions <- predict(gbm_model, newdata = data.frame(x_test), n.trees = 1000)
mse_gbm <- mean((y_test - gbm_predictions)^2)
print(mse_gbm)
rmse_gbm <- sqrt(mse_gbm)
print(rmse_gbm)
best_trees <- gbm.perf(gbm_model, method = "cv")
residuals <- y_test - gbm_predictions
plot(residuals)
abline(h = 0, col = "red")
summary(gbm_model)
train_predictions <- predict(gbm_model, newdata = data.frame(x_train), n.trees = 1000)
mse_train_gbm <- mean((y_train - train_predictions)^2)
rmse_train_gbm <- sqrt(mean((y_train - train_predictions)^2))
print(mse_train_gbm)
#tuned gbm model
tuneGrid <- expand.grid(
  interaction.depth = c(1, 3, 5),
  n.trees = c(100, 500, 1000),
  shrinkage = c(0.01, 0.1),
  n.minobsinnode = 10)
train_control <- trainControl(method = "cv", number = 5)
gbm_tuned <- train(slg ~ ., data = data.frame(x_train, slg = y_train),
                   method = "gbm",
                   trControl = train_control,
                   tuneGrid = tuneGrid)
print(gbm_tuned)
best_gbm_model <- gbm_tuned$finalModel
gbm_predictions_tuned <- predict(best_gbm_model, newdata = data.frame(x_test), n.trees = gbm_tuned$bestTune$n.trees)
mse_tuned <- mean((y_test - gbm_predictions_tuned)^2)
rmse_tuned <- sqrt(mse_tuned)
print(mse_tuned)
print(paste("Improvement in RMSE:", rmse_gbm - rmse_tuned))
#this model has the lowest mse and rmse, with .00150 and .03876. It also produced a very small train and test rmse, indicating that the model is not overfitted.
#xgboost
x_train_matrix <- as.matrix(x_train)
x_test_matrix <- as.matrix(x_test)
xgb_model <- xgboost(
  data = x_train_matrix,
  label = y_train,
  max.depth = 3,
  eta = 0.1,
  nrounds = 100,
  objective = "reg:squarederror")
xgb_predictions <- predict(xgb_model, newdata = x_test_matrix)
mse_xgb <- mean((y_test - xgb_predictions)^2)
print(mse_xgb)
rmse_xgb <- sqrt(mse_xgb)
print(rmse_xgb)
#gam
gam_model <- gam(slg_next ~ player_age + slg_this + launch_angle_avg + solidcontact_percent + hard_hit_percent + oz_contact_percent + meatball_swing_percent + iz_contact_percent + pull_percent + linedrives_percent, data = train_data)
summary(gam_model)
predictions_gam <- predict(gam_model, newdata = test_data)
mse_gam <- mean((predictions_gam - test_data$slg_next)^2)
print(mse_gam)
#lasso
lasso_model <- glmnet(x_train, y_train, alpha = 1)
cv_model_lasso <- cv.glmnet(x_train, y_train, alpha = 1)
best_lambda_lasso <- cv_model_lasso$lambda.min
predictions_lasso <- predict(lasso_model, s = best_lambda_lasso, newx = x_test)
mse_lasso <- mean((predictions_lasso - y_test)^2)
print(mse_lasso)
#elastic net
elastic_net_model <- glmnet(x_train, y_train, alpha = 0.9)
cv_model_el <- cv.glmnet(x_train, y_train, alpha = 0.9)
best_lambda_el <- cv_model_el$lambda.min
predictions_el <- predict(elastic_net_model, s = best_lambda_el, newx = x_test)
mse_el <- mean((predictions_el - y_test)^2)
print(mse_el)
print(best_lambda)

#using the best model(ridge regression) to predict a player's 2024 slugging percentage
slug_pred<-slug|>
  filter(player_age > 22 & player_age < 37)|>
  select(-`last_name, first_name`)
slg_2324 <- slug_pred|>
  filter(year != 2021,year!=2022)
p_ids_34 <- slg_2324 |>
  group_by(player_id) |>
  filter(year %in% c(2023, 2024)) |>
  summarise(n_years = n_distinct(year)) |>
  filter(n_years == 2)|>
  pull(player_id)
df_2023 <- slg_2324 |>
  filter(year == 2023, player_id %in% p_ids_34) |>
  select(player_id, everything())|>
  rename(slg_this = slg_percent)
df_2024 <- slg_2324 |>
  filter(year == 2024, player_id %in% p_ids_34) |>
  select(player_id, slg_percent) |>
  rename(slg_next = slg_percent)
df_34 <- df_2023 %>%
  left_join(df_2024, by = "player_id")|>
  select(-year,-player_id)
df_34<-df_34|>
  select(-xslg,-exit_velocity_avg,-barrel_batted_rate,-avg_best_speed,-flyballs_percent)
x_test_34 <- df_34|>
  select(-slg_next)
y_test_34 <- df_34$slg_next
x_test_34 <- as.matrix(x_test_34)
predictions_ridge_2024 <- predict(ridge_model_best, newx = x_test_34,s=best_lambda_ridge)
predictions_ridge_2024 <- as.vector(predictions_ridge_2024)
mse_ridge_2024 <- mean((y_test_34 - predictions_ridge_2024)^2)
rmse_ridge_2024 <- sqrt(mse_ridge_2024)
print(mse_ridge_2024)
print(rmse_ridge_2024)
coefficients_ridge_new <- coef(ridge_model_best, s = best_lambda_ridge)
coefficients_df_new <- as.data.frame(as.matrix(coefficients_ridge_new))
colnames(coefficients_df_new) <- c("Coefficient")
print(coefficients_df_new)
#testing on linear regression
predictions_lin_24 <- predict(linmod, newdata = df_34)
mse_lin_24 <- mean((df_34$slg_next - predictions_lin_24)^2)
rmse_lin_24 <- sqrt(mse_lin_24)
print(mse_lin_24)
print(rmse_lin_24)
#rf 2024
rf_pred_24 <- predict(rf_model, newdata = x_test_34)
mse_rf_24 <- mean((y_test_34 - rf_pred_24)^2)
print(mse_rf_24)
rmse_rf_24 <- sqrt(mse_rf_24)
print(rmse_rf_24)
#gbm
gbm_pred_24 <- predict(gbm_model, newdata = data.frame(x_test_34), n.trees = 1000)
mse_gbm_24 <- mean((y_test_34 - gbm_pred_24)^2)
print(mse_gbm_24)
rmse_gbm_24 <- sqrt(mse_gbm_24)
print(rmse_gbm_24)
#gam
predictions_gam_24 <- predict(gam_model, newdata = df_34)
mse_gam_24 <- mean((predictions_gam_24 - df_34$slg_next)^2)
print(mse_gam_24)
#lasso
predictions_lasso_24<- predict(lasso_model, s = best_lambda_lasso, newx = x_test_34)
mse_lasso_24 <- mean((predictions_lasso_24 - y_test_34)^2)
print(mse_lasso_24)
#xgboost
xgb_predictions_24 <- predict(xgb_model, newdata = x_test_34)
mse_xgb_24 <- mean((y_test_34 - xgb_predictions_24)^2)
print(mse_xgb_24)
#comparing ridge model to xSLG
df_34_xslg <- df_2023 %>%
  left_join(df_2024, by = "player_id")|>
  select(-year,-player_id)
df_34_xslg<-df_34_xslg|>
  select(-slg_next,-exit_velocity_avg,-barrel_batted_rate,-avg_best_speed,-flyballs_percent)
x_test_xslg <- df_34_xslg|>
  select(-xslg)
y_test_xslg <- df_34_xslg$xslg
x_test_xslg <- as.matrix(x_test_xslg)
predictions_ridge_xslg <- predict(ridge_model_best, newx = x_test_xslg,s=best_lambda_ridge)
predictions_ridge_xslg <- as.vector(predictions_ridge_xslg)
mse_ridge_xslg <- mean((y_test_xslg - predictions_ridge_xslg)^2)
print(mse_ridge_xslg)
#testing with lasso
predictions_lasso_xslg<- predict(lasso_model, s = best_lambda_lasso, newx = x_test_xslg)
mse_lasso_xslg <- mean((predictions_lasso_xslg - y_test_xslg)^2)
print(mse_lasso_xslg)
