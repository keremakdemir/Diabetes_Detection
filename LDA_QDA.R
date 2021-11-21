#import libraries
library(caret)
library(glue)
library(pROC)
library(ggplot2)
library(rsample)
library(klaR)

#setting a seed for reproducible results
set.seed(1234)

#Read data and check the first few rows
diabetes <- read.csv(file='diabetes_corrected.csv', header=TRUE)
head(diabetes)

#changing outcome to factor
diabetes$Outcome <- as.factor(diabetes$Outcome)

#generating test and training data
index <- initial_split(diabetes,
                       prop = 0.8)
train_set <- training(index)
test_set <- testing(index)

NIR <- (table(diabetes$Outcome)/nrow(diabetes))
glue('No information rate is {round(NIR["0"],2)}')

#LDA model
caret_lda <- train(Outcome ~.,
                   data = train_set,
                   method = "lda",
                   trControl = trainControl(method = "repeatedcv", number = 5,
                                            repeats = 50))

lda_predict <- predict(caret_lda, newdata = test_set, type = 'raw')
lda_result <- confusionMatrix(data=lda_predict, reference=test_set$Outcome)
glue('Accuracy of LDA model is {round(lda_result$overall["Accuracy"],3)}')

lda_predict_prob <- predict(caret_lda, newdata = test_set, type = 'prob')

roccurve_lda <- roc(response = test_set$Outcome, predictor = lda_predict_prob[,2], quiet = TRUE)
ggroc(roccurve_lda, legacy.axes = TRUE, lwd=1) + theme_bw(base_size = 18) + ggtitle("ROC Curve for LDA") + theme(plot.title = element_text(hjust = 0.5))
glue("Area under curve for LDA model is {round(auc(roccurve_lda),3)}")

#QDA model
caret_qda <- train(Outcome ~.,
                   data = train_set,
                   method = "qda",
                   trControl = trainControl(method = "repeatedcv", number = 5,
                                            repeats = 50))

qda_predict <- predict(caret_qda, newdata = test_set, type = 'raw')
qda_result <- confusionMatrix(data=qda_predict, reference=test_set$Outcome)
glue('Accuracy of QDA model is {round(qda_result$overall["Accuracy"],3)}')

qda_predict_prob <- predict(caret_qda, newdata = test_set, type = 'prob')

roccurve_qda <- roc(response = test_set$Outcome, predictor = qda_predict_prob[,2], quiet = TRUE)
ggroc(roccurve_qda, legacy.axes = TRUE, lwd=1) + theme_bw(base_size = 18) + ggtitle("ROC Curve for QDA") + theme(plot.title = element_text(hjust = 0.5))
glue("Area under curve for QDA model is {round(auc(roccurve_qda),3)}")

cat('It seems like assuming same covariance matrix for each class may be a good approximation as accuracy of LDA is higher.')

#QDA model enhanced (with only few predictors)
caret_qda <- train(Outcome ~Glucose+BMI+Pregnancies+Age,
                   data = train_set,
                   method = "qda",
                   trControl = trainControl(method = "repeatedcv", number = 5,
                                            repeats = 50))

qda_predict <- predict(caret_qda, newdata = test_set, type = 'raw')
qda_result <- confusionMatrix(data=qda_predict, reference=test_set$Outcome)
glue('Accuracy of enhanced QDA model is {round(qda_result$overall["Accuracy"],3)}')

qda_predict_prob <- predict(caret_qda, newdata = test_set, type = 'prob')

roccurve_qda <- roc(response = test_set$Outcome, predictor = qda_predict_prob[,2], quiet = TRUE)
ggroc(roccurve_qda, legacy.axes = TRUE, lwd=1) + theme_bw(base_size = 18) + ggtitle("ROC Curve for Enhanced QDA") + theme(plot.title = element_text(hjust = 0.5))
glue("Area under curve for enhanced QDA model is {round(auc(roccurve_qda),3)}")

#LDA model enhanced (with only few predictors)
caret_lda <- train(Outcome ~Glucose+BMI+Pregnancies+Age,
                   data = train_set,
                   method = "lda",
                   trControl = trainControl(method = "repeatedcv", number = 5,
                                            repeats = 50))

lda_predict <- predict(caret_lda, newdata = test_set, type = 'raw')
lda_result <- confusionMatrix(data=lda_predict, reference=test_set$Outcome)
glue('Accuracy of enhanced LDA model is {round(lda_result$overall["Accuracy"],3)}')

lda_predict_prob <- predict(caret_lda, newdata = test_set, type = 'prob')

roccurve_lda <- roc(response = test_set$Outcome, predictor = lda_predict_prob[,2], quiet = TRUE)
ggroc(roccurve_lda, legacy.axes = TRUE, lwd=1) + theme_bw(base_size = 18) + ggtitle("ROC Curve for Enhanced LDA") + theme(plot.title = element_text(hjust = 0.5))
glue("Area under curve for enhanced LDA model is {round(auc(roccurve_lda),3)}")

cat('Again, accuracy of enhanced LDA with only glucose, BMI, pregnancies, and age is higher than enhanced QDA.')




