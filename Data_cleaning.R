#setting seed for reproducible results
set.seed(12345)

#Read data and check the first few rows
diabetes <- read.csv(file='diabetes.csv', header=TRUE)
head(diabetes)

#altering 0 values in glucose, blood pressure and BMI as medians of that predictors
diabetes$Glucose[diabetes$Glucose==0] <- median(diabetes$Glucose[diabetes$Glucose>0])
diabetes$BloodPressure[diabetes$BloodPressure==0] <- median(diabetes$BloodPressure[diabetes$BloodPressure>0])
diabetes$BMI[diabetes$BMI==0] <- median(diabetes$BMI[diabetes$BMI>0])

#for skin thickness, filling missing data (zeros) by building a linear regression with BMI
#selecting rows where thickness is 0
invalid_skin_rows <- as.integer(rownames(diabetes[diabetes$SkinThickness==0,]))

#filtering data with respect to valid and invalid data rows (where thickness is 0)
invalid_BMI <- diabetes[invalid_skin_rows,'BMI']
valid_BMI <- diabetes[-invalid_skin_rows,'BMI']
valid_skin <- diabetes[-invalid_skin_rows,'SkinThickness']

#building a linear model and predicting for missing thicknesses
linear_model_data <- data.frame(BMI=valid_BMI, Thickness=valid_skin)
linear_model_for_skin <- lm(Thickness ~ BMI, data = linear_model_data)

par(mfrow = c(2,2))
plot(linear_model_for_skin)
mtext("Model Diagnostics for Skin Thickness", side=3, line=22, at=-0.01)
cat('When we plot the regression diagnostics graphs, we seem to satisfy the assumptions. From Residuals vs Fitted plot, 
there is no clear pattern. The red trend line is nearly flat, the model seems to capture true linear relationship. 
Also, from the same plot, we can see that he variability of residuals are not that pronounced. Thus, we can say 
that the model satisfies constant variance assumption adequately. Also, from Normal Q-Q plot, model seems to 
satisfy normality of errors.')

predict_skin <- predict(linear_model_for_skin, newdata = data.frame(BMI=invalid_BMI))

#finding errors of our linear regression model by predicting for valid data onlv and creating random errors
predict_error <- predict(linear_model_for_skin, newdata = data.frame(BMI=valid_BMI))
error_lm <- valid_skin - predict_error
error_mean <- mean(error_lm)
error_sd <- sd(error_lm)
random_errors <- rnorm(length(predict_skin), mean=error_mean, sd=error_sd)

#finding final predictions by adding random errors to our predictions (this step is done to reduce collinearity between predictors)
final_prediction_skin <- predict_skin + random_errors
diabetes$SkinThickness[invalid_skin_rows] <- round(final_prediction_skin)

# Create prediction grid to see regression line
xgrid <- list(BMI = seq(min(valid_BMI), max(valid_BMI), len=201))
# Perform prediction to see regression line
fitted_values <- predict(linear_model_for_skin, newdata = xgrid)

par(mfrow = c(1,1))
plot(valid_BMI, valid_skin, pch=19, col = "darkgray", xlab = "BMI", ylab = "Skin Thickness", main = "Linear Regression between BMI and Skin Thickness")
lines(xgrid$BMI, fitted_values, lwd=2)

#for insulin, filling missing data (zeros) by building a log linear regression with glucose
#selecting rows where insulin is 0
invalid_insulin_rows <- as.integer(rownames(diabetes[diabetes$Insulin==0,]))

#filtering data with respect to valid and invalid data rows (where insulin is 0)
invalid_glucose <- diabetes[invalid_insulin_rows,'Glucose']
valid_glucose <- diabetes[-invalid_insulin_rows,'Glucose']
valid_insulin <- diabetes[-invalid_insulin_rows,'Insulin']

plot(valid_glucose, log(valid_insulin), pch=19, col = "black", xlab = "Glucose", ylab = "ln(Insulin)", main = "Ln Transformed Insulin vs Glucose")
cat('When we natural log transform insulin, they seem to follow a fairly linear relationship.')

#building a log linear model and predicting for missing insulin data
log_model_data <- data.frame(Glucose=valid_glucose, Insulin=valid_insulin)
log_model_for_insulin <- lm(log(Insulin) ~ Glucose, data = log_model_data)

par(mfrow = c(2,2))
plot(log_model_for_insulin)
mtext("Model Diagnostics for Insulin", side=3, line=22, at=-0.005)

cat('When we plot the regression diagnostics graphs, we seem to satisfy the assumptions. From Residuals vs Fitted plot, 
there is no clear pattern. The red trend line is nearly flat, the model seems to capture true linear relationship. 
Also, from the same plot, we can see that he variability of residuals are not that pronounced. Thus, we can say 
that the model satisfies constant variance assumption adequately. Also, from Normal Q-Q plot, there are some 
values not fitted to normal distribution but it seems reasonable to assume the normality of errors.')

predict_insulin <- exp(predict(log_model_for_insulin, newdata = data.frame(Glucose=invalid_glucose)))

#finding errors of our log linear regression model by predicting for valid data onlv and creating random errors
predict_error_insulin <- exp(predict(log_model_for_insulin, newdata = data.frame(Glucose=valid_glucose)))
error_insulin <- valid_insulin - predict_error_insulin
error_mean_insulin <- mean(error_insulin)
error_sd_insulin <- sd(error_insulin)/10
random_errors_insulin <- rnorm(length(predict_insulin), mean=error_mean_insulin, sd=error_sd_insulin)

#finding final predictions by adding random errors to our predictions (this step is done to reduce collinearity between predictors)
final_prediction_insulin <- predict_insulin + random_errors_insulin
diabetes$Insulin[invalid_insulin_rows] <- round(final_prediction_insulin)

# Create prediction grid to see regression line
xgrid <- list(Glucose = seq(min(valid_glucose), max(valid_glucose), len=201))
# Perform prediction to see regression line
fitted_values <- exp(predict(log_model_for_insulin, newdata = xgrid))

par(mfrow = c(1,1))
plot(valid_glucose, valid_insulin, pch=19, col = "darkgray", xlab = "Glucose", ylab = "Insulin", main = "Log Linear Regression between Glucose and Insulin")
lines(xgrid$Glucose, fitted_values, lwd=2)

#exporting the new data
write.csv(diabetes,"diabetes_corrected.csv", row.names = FALSE)


