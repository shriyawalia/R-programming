# Load the readxl package
library(readxl)

# Read the Excel file into R
cars <- read_excel("Rfinal.xlsx")

cars

dim(cars)

str(cars)

summary(cars)



# Exclude non-numeric columns from pairs(checking relationship between numerical data)
pairs(cars[, sapply(cars, is.numeric)])

#Converting data into dummies
library(fastDummies)

data_dummy <- fastDummies::dummy_cols(cars, remove_first_dummy = TRUE,remove_selected_columns = TRUE)



# Diagnostic plots on entire data set (checking for outliers)
lm_1 <- lm(Pris ~ . , data = data_dummy)
summary(lm_1)

# Plot diagnostic plots
par(mfrow=c(2,2))
plot(lm_1)


# Remove outliers
data_dummy <- data_dummy[-128,]
data_dummy <- data_dummy[-283,]
data_dummy <- data_dummy[-579,]


#Run diagnostic plots again
lm_1_1 <- lm(Pris ~ . , data = data_dummy)
summary(lm_1_1)

# Plot diagnostic plots
par(mfrow=c(2,2))
plot(lm_1_1)


# Remove outliers
data_dummy <- data_dummy[-282,]
data_dummy <- data_dummy[-577,]
data_dummy <- data_dummy[-424,]

#Run diagnostic plots again

lm_1_2 <- lm(Pris ~ . , data = data_dummy)
summary(lm_1_2)

# Plot diagnostic plots
par(mfrow=c(2,2))
plot(lm_1_2)

#Remove outliers

data_dummy <- data_dummy[-261,]
data_dummy <- data_dummy[-585,]
data_dummy <- data_dummy[-575,]

#Run diagnostic plots again

lm_1_3 <- lm(Pris ~ . , data = data_dummy)
summary(lm_1_3)

# Plot diagnostic plots
par(mfrow=c(2,2))
plot(lm_1_3)

# Remove outliers
data_dummy <- data_dummy[-583,]
data_dummy <- data_dummy[-574,]

#Run diagnostic plots again

lm_1_4 <- lm(Pris ~ . , data = data_dummy)
summary(lm_1_4)

# Plot diagnostic plots
par(mfrow=c(2,2))
plot(lm_1_4)


# Train, val, test, took example for Antonio's github

spec = c(train = .6, validate = .2, test = .2)

set.seed(123)
g = sample(cut(
  seq(nrow(data_dummy)), 
  nrow(data_dummy)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(data_dummy, g)

car_train <- res$train
car_val <- res$validate
car_test <- res$test


#Will begin with fitting the lm() on training data

lm_train <- lm(Pris ~ . , data = car_train)
summary(lm_train)


correlation_matrix <- cor(car_train)

# Check for high correlations
high_correlation_threshold <- 0.7
high_correlation_pairs <- which(abs(correlation_matrix) > high_correlation_threshold & correlation_matrix != 1, arr.ind = TRUE)

if (length(high_correlation_pairs) > 0) {
  print("Highly correlated variables:")
  print(high_correlation_pairs)
} else {
  print("No highly correlated variables found.")
}


#Checked correlation between the features, and it appears that modellår and miltal are highly correlated
#so we remove miltal

car_train_modelyear <- car_train[,-2]

#Fitting the lm() on training data without miltal

lm_train_1 <- lm(Pris ~ . , data = car_train_modelyear)
summary(lm_train_1)

#Remove Märke dacia because it's NA

car_train_modelyear_1 <- car_train_modelyear[, !colnames(car_train_modelyear) == "Märke_Dacia"]

#Fitting the lm() again

lm_train_1_1 <- lm(Pris ~ . , data = car_train_modelyear_1)
summary(lm_train_1_1)

#Now will check multicollinearity

# Load the car package for vif() function
library(car)


vif(lm_train_1_1)


#checking high vif values
vif_values <- vif(lm_train_1_1)

# Filter out variables with VIF > 10
high_vif_variables <- names(vif_values)[vif_values > 10]

# Print variables with VIF > 10
print(high_vif_variables)


# Remove the "Biltyp" variable from the car_train dataset
car_train_without_biltyp <- subset(car_train_modelyear_1, select = -c(Biltyp_Halvkombi, Biltyp_Kombi, Biltyp_Sedan, Biltyp_SUV))


#Fitting model again
lm_train_withoutbiltyp <- lm(Pris ~ . , data = car_train_without_biltyp)
summary(lm_train_withoutbiltyp) 

#checking high vif values
vif_values_1 <- vif(lm_train_withoutbiltyp)

# Filter out variables with VIF > 10
high_vif_variables_1 <- names(vif_values_1)[vif_values > 10]

# Print variables with VIF > 10
print(high_vif_variables_1)

#Got a reduced adj. R^2 so will try removing Märke this time

# Remove the "Märke" variable from the car_train dataset
car_train_without_märke <- subset(car_train_modelyear_1, select = -c(Märke_Audi, Märke_BMW,`Märke_Mercedes-Benz`, Märke_Kia, Märke_Skoda, Märke_Volkswagen, Märke_Volvo))


#Fitting model again
lm_train_withoutmärke <- lm(Pris ~ . , data = car_train_without_märke)
summary(lm_train_withoutmärke) 

# Plot diagnostic plots
par(mfrow=c(2,2))
plot(lm_train_withoutmärke)

#checking high vif values
vif_values_2 <- vif(lm_train_withoutmärke)

# Filter out variables with VIF > 10
high_vif_variables_2 <- names(vif_values_2)[vif_values > 10]

# Print variables with VIF > 10
print(high_vif_variables_2)

#Trying it by just removing very high vif values(biltyp:halvkombi,kombi,SUV)

car_train_without_märkebiltyp <- subset(car_train_without_märke, select = -c(Biltyp_Halvkombi , Biltyp_Kombi, Biltyp_SUV))

#Fitting model again
lm_train_without_märkebiltyp <- lm(Pris ~ . , data = car_train_without_märkebiltyp)
summary(lm_train_without_märkebiltyp) 

#Will try to do another VIF

#checking high vif values
vif_values <- vif(lm_train_without_märkebiltyp)

# Filter out variables with VIF > 5
high_vif_variables <- names(vif_values)[vif_values > 10]

# Print variables with VIF > 5
print(high_vif_variables)

# Plot diagnostic plots
par(mfrow=c(2,2))
plot(lm_train_without_märkebiltyp)

#Dont have any high VIF values so we proceed further 


#Doing best subset selection (will check which feature combo is the best)

# Load the 'leaps' package
library(leaps)

# Perform best subset regression
best_subset <- regsubsets(Pris ~ ., data = car_train_without_märkebiltyp, nvmax = 10,really.big = TRUE)

# Summarize results
best_subset_summ<- summary(best_subset)


names(best_subset_summ)
par(mfrow = c(1, 1))
plot(best_subset_summ$adjr2)
best_subset_summ$adjr2

#I will proceed with 5 predictors

coef(best_subset, 5)
plot(best_subset, scale = "adjr2")

#Fitting model again with 5 predictors
lm_train_with5 <- lm(Pris ~ Modellår+Hästkrafter+Bränsle_Diesel+Växellåda_Manuell+ Märke_Porsche , data =car_train_without_märkebiltyp)
summary(lm_train_with5) 


library(Metrics)


# Evaluation on validation data

# Predictions on validation data
val_pred_m1 <- predict(lm_train, newdata = car_val)
val_pred_m2 <- predict(lm_train_1_1, newdata = car_val)
val_pred_m3 <- predict(lm_train_withoutbiltyp, newdata = car_val)
val_pred_m4 <- predict(lm_train_withoutmärke, newdata = car_val)
val_pred_m5 <- predict(lm_train_without_märkebiltyp, newdata = car_val)
val_pred_m6 <- predict(lm_train_with5, newdata = car_val)

# Calculate MAPE for validation data
calculate_mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

results <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"),
  RMSE_val_data = c(rmse(car_val$Pris, val_pred_m1),
                    rmse(car_val$Pris, val_pred_m2),
                    rmse(car_val$Pris, val_pred_m3),
                    rmse(car_val$Pris, val_pred_m4),
                    rmse(car_val$Pris, val_pred_m5),
                    rmse(car_val$Pris, val_pred_m6)),
  MAPE_val_data = c(calculate_mape(car_val$Pris, val_pred_m1),
                    calculate_mape(car_val$Pris, val_pred_m2),
                    calculate_mape(car_val$Pris, val_pred_m3),
                    calculate_mape(car_val$Pris, val_pred_m4),
                    calculate_mape(car_val$Pris, val_pred_m5),
                    calculate_mape(car_val$Pris, val_pred_m6)),
  Adj_R_squared = c(summary(lm_train)$adj.r.squared,
                    summary(lm_train_1_1)$adj.r.squared,
                    summary(lm_train_withoutbiltyp)$adj.r.squared,
                    summary(lm_train_withoutmärke)$adj.r.squared,
                    summary(lm_train_without_märkebiltyp)$adj.r.squared,
                    summary(lm_train_with5)$adj.r.squared),
  BIC = c(BIC(lm_train), BIC(lm_train_1_1), BIC(lm_train_withoutbiltyp), BIC(lm_train_withoutmärke), BIC(lm_train_without_märkebiltyp), BIC(lm_train_with5))
)

# Print evaluation results
results

# Evaluation on test data

# Predictions on test data
test_pred_m1 <- predict(lm_train, newdata = car_test)
test_pred_m2 <- predict(lm_train_1_1, newdata = car_test)
test_pred_m3 <- predict(lm_train_withoutbiltyp, newdata = car_test)
test_pred_m4 <- predict(lm_train_withoutmärke, newdata = car_test)
test_pred_m5 <- predict(lm_train_without_märkebiltyp, newdata = car_test)
test_pred_m6 <- predict(lm_train_with5, newdata = car_test)

# Calculate MAPE for test data
results_test <- data.frame(
  Model_test = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"),
  RMSE_test_data = c(rmse(car_test$Pris, test_pred_m1),
                     rmse(car_test$Pris, test_pred_m2),
                     rmse(car_test$Pris, test_pred_m3),
                     rmse(car_test$Pris, test_pred_m4),
                     rmse(car_test$Pris, test_pred_m5),
                     rmse(car_test$Pris, test_pred_m6)),
  MAPE_test_data = c(calculate_mape(car_test$Pris, test_pred_m1),
                     calculate_mape(car_test$Pris, test_pred_m2),
                     calculate_mape(car_test$Pris, test_pred_m3),
                     calculate_mape(car_test$Pris, test_pred_m4),
                     calculate_mape(car_test$Pris, test_pred_m5),
                     calculate_mape(car_test$Pris, test_pred_m6))
)

# Print evaluation results for test data
results_test

# Lasso Regression

library(glmnet)
library(Metrics)
# Define predictors and target variable
X_train <- model.matrix(Pris ~ ., data = car_train)[,-1] # Remove intercept column
y_train <- car_train$Pris

# Fit Lasso model
lasso_model <- cv.glmnet(X_train, y_train, alpha = 1)

# Choose lambda with minimum cross-validation error
best_lambda <- lasso_model$lambda.min

# Refit the model using the best lambda
lasso_final_model <- glmnet(X_train, y_train, alpha = 1, lambda = best_lambda)

# Model evaluation on validation set
X_val <- model.matrix(Pris ~ ., data = car_val)[,-1]
y_val <- car_val$Pris

lasso_val_pred <- predict(lasso_final_model, s = best_lambda, newx = X_val)
lasso_rmse <- rmse(y_val, lasso_val_pred)
lasso_mape <- mean(abs((y_val - lasso_val_pred) / y_val)) * 100

# Model evaluation on test set
X_test <- model.matrix(Pris ~ ., data = car_test)[,-1]
y_test <- car_test$Pris

lasso_test_pred <- predict(lasso_final_model, s = best_lambda, newx = X_test)
lasso_test_rmse <- rmse(y_test, lasso_test_pred)
lasso_test_mape <- mean(abs((y_test - lasso_test_pred) / y_test)) * 100

# Print Lasso model evaluation metrics
cat("Lasso Validation RMSE:", lasso_rmse, "\n")
cat("Lasso Validation MAPE:", lasso_mape, "%\n")
cat("Lasso Test RMSE:", lasso_test_rmse, "\n")
cat("Lasso Test MAPE:", lasso_test_mape, "%\n")



# Create a data frame with the new data
X_new <- data.frame(
  Miltal = 9960,
  Modellår = 2019,
  Hästkrafter = 136,
  Län_Gotland = 0, 
  Län_Jönköping = 0,
  Län_Kalmar = 0,
  Län_Östergötland = 0,
  Län_Skåne = 0,
  Län_Stockholm = 1,  
  `Län_Västra Götalands` = 0,
  Bränsle_Diesel = 1,
  Bränsle_El = 0,
  `Bränsle_Miljöbränsle/Hyb` = 0,
  Växellåda_Manuell = 1,
  Biltyp_Coupé = 1,
  Biltyp_Halvkombi = 0,
  Biltyp_Kombi = 0,
  Biltyp_Sedan = 0,  
  Biltyp_SUV = 0,
  Drivning_Tvåhjulsdriven = 1,
  Färg_Brun = 0,
  Färg_Grå = 0,  
  Färg_Grön = 0,
  Färg_Orange = 0,
  Färg_Röd = 0,
  Färg_Silver = 0,
  Färg_Svart = 1,
  Färg_Vit = 0,
  Märke_Audi = 0,
  Märke_BMW = 0,
  Märke_Chevrolet = 0,
  Märke_Chrysler = 0,
  Märke_Citroen = 0,
  Märke_Cupra = 0,
  Märke_Dacia = 0,
  Märke_Fiat = 0,
  Märke_Ford = 0,
  Märke_Honda = 0,
  Märke_Hyundai = 0,
  Märke_Jaguar = 0,
  Märke_Jeep = 0,
  Märke_Kia = 0,
  Märke_Land_Rover = 0,
  Märke_Lexus = 0,
  Märke_Mazda = 0,
  Märke_Mercedes_Benz = 1,
  Märke_MINI = 0,
  Märke_Mitsubishi = 0,
  Märke_Nissan = 0,
  Märke_Opel = 0,
  Märke_Peugeot = 0,
  Märke_Polestar = 0,
  Märke_Porsche = 0,
  Märke_Renault = 0,
  Märke_Seat = 0,
  Märke_Skoda = 0,
  Märke_Subaru = 0,
  Märke_Suzuki = 0,
  Märke_Tesla = 0,
  Märke_Toyota = 0,
  Märke_Volkswagen = 0,
  Märke_Volvo = 0 
)

# Print the new data frame
print(X_new)

# Use the predict function to predict the car price
predicted_price <- predict(lasso_final_model, newx = X_new, s = best_lambda)

# Print the predicted price
cat("Predicted Price of the Car:", predicted_price, "\n")
