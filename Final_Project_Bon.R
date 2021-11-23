#####################################################
#Final Project
#####################################################
#Import library
library(caret)
library(ISLR2)
library(rsample)
library(pROC)
library(ggplot2)
library(klaR)
library(splines)
library(tidyverse)
library(h2o)

#Import diabetes Table and view
# Read diabetes data, please change address as your reference location
diabetes <- read.csv("C:/Users/bonhs/OneDrive/Documents/NCSU/NCSU ST563/R/diabetes_good.csv", header = TRUE)

#Initial all ROC plot
pdf(paste("ALL ROC BY BST SPLIT.pdf"))

#View data
head(diabetes)
table(diabetes$Outcome)


#Define train and test data with Boost sample split
set.seed(2)
bst_sample<-bootstraps(diabetes,times=5)





#Initialization
KNN_ROC<-list()
KNN_Error_Rate<-rep(NA,5)
KNN_AUC<-rep(NA,5)

#Perfrom KNN
for (i in 1:5){
train<-training(bst_sample$splits[[i]])
test<-testing(bst_sample$splits[[i]])


## K values for tuning
kgrid <- expand.grid(k = seq(2,51))
train$Outcome <- as.factor(train$Outcome)
## LOOCV tuning
tr <- trainControl(method = "cv",
                   number = 5)

## Train k
fit <- train(Outcome ~ .,
             data = train,
             method = "knn",
             tuneGrid = kgrid,
             trControl = tr)

#Plot tune result
plot(fit)

#View tuned K
fit$bestTune$k

# Refit the model with best K
tuned_knn_class <- train(Outcome ~ .,
                         data = train,
                         method = "knn",
                         tuneGrid = expand.grid(k = fit$bestTune$k),
                         trControl = trainControl(method = "none"))

#Confusion matrix
knn_pre<-predict(tuned_knn_class,newdata=test)
knn_result<-confusionMatrix(data=as.factor(test$Outcome),reference=knn_pre)
#Accuracy rate
knn_result$overall[[2]]

#Error rate
test_error_rate_knn<-1-knn_result$overall[[2]]
KNN_Error_Rate[i]<-test_error_rate_knn

#Plot ROC and AUC
knn_pre_ROC<-predict(tuned_knn_class,newdata=test,type = "prob")
knn_roccurve <- roc(response = test$Outcome,
                predictor = knn_pre_ROC[,2])

KNN_ROC[[i]]<-ggroc(knn_roccurve, legacy.axes = TRUE, lwd=2) +  theme_bw(base_size = 18) + ggtitle(paste("KNN ROC BST",i))
KNN_AUC[i]<-auc(knn_roccurve)
}

#View ROC
KNN_ROC
#View Error Rate
mean(KNN_Error_Rate)
#View AUC
mean(KNN_AUC)




#Initialization
NB_ROC<-list()
NB_Error_Rate<-rep(NA,5)
NB_AUC<-rep(NA,5)


#Perfrom NaiveBayes W/O Kernel
for (i in 1:5){
train<-training(bst_sample$splits[[i]])
test<-testing(bst_sample$splits[[i]])
nb <- NaiveBayes(as.factor(Outcome) ~ .,
                      data = train,
                      usekernel = F)

#Confusion matrix
ns_pre<-predict(nb,newdata=test,type = "response")
ns_result<-confusionMatrix(data=as.factor(test$Outcome),reference=ns_pre$class)
#Accuracy rate
ns_result$overall[[2]]

#Error rate
test_error_rate_ns<-1-ns_result$overall[[2]]
NB_Error_Rate[i]<-test_error_rate_ns

ns_pre_ROC<-predict(nb,newdata=test,type = "prob")
ns_roccurve <- roc(response = test$Outcome,
                    predictor = ns_pre_ROC$posterior[,2])
NB_ROC[[i]]<-ggroc(ns_roccurve, legacy.axes = TRUE, lwd=2) +  theme_bw(base_size = 18) + ggtitle(paste("NaiveBayes w/o Kernel ROC BST",i))

NB_AUC[i]<-auc(ns_roccurve)
}

#View ROC
NB_ROC
#View Error Rate
mean(NB_Error_Rate)
#View AUC
mean(NB_AUC)



#Initialization
K_NB_ROC<-list()
K_NB_Error_Rate<-rep(NA,5)
K_NB_AUC<-rep(NA,5)

#Perfrom NaiveBayes With Kernel
for (i in 1:5){
train<-training(bst_sample$splits[[i]])
test<-testing(bst_sample$splits[[i]])
nb_K <- NaiveBayes(as.factor(Outcome) ~ .,
                 data = train,
                 usekernel = T)

#Confusion matrix
ns_k_pre<-predict(nb_K,newdata=test,type = "response")
ns_k_result<-confusionMatrix(data=as.factor(test$Outcome),reference=ns_k_pre$class)
#Accuracy rate
ns_k_result$overall[[2]]

#Error rate
test_error_rate_ns_k<-1-ns_k_result$overall[[2]]
K_NB_Error_Rate[i]<-test_error_rate_ns_k


ns_k_pre_ROC<-predict(nb_K,newdata=test,type = "prob")
ns_k_roccurve <- roc(response = test$Outcome,
                   predictor = ns_k_pre_ROC$posterior[,2])
K_NB_ROC[[i]]<-ggroc(ns_k_roccurve, legacy.axes = TRUE, lwd=2) +  theme_bw(base_size = 18) + ggtitle(paste("NaiveBayes w/ Kernel ROC BST",i))

K_NB_AUC[i]<-auc(ns_k_roccurve)
}

#View ROC
K_NB_ROC
#View Error Rate
mean(K_NB_Error_Rate)
#View AUC
mean(K_NB_AUC)




#Initialization
L_SVM_ROC<-list()
L_SVM_Error_Rate<-rep(NA,5)
L_SVM_AUC<-rep(NA,5)

#SVM Linear Model
for (i in 1:5){
train<-training(bst_sample$splits[[i]])
test<-testing(bst_sample$splits[[i]])
# Tuning grid
tune_grid <- expand.grid(cost = exp(seq(-7,3,len=11)))
# Train the model
sv_caret <- train(as.factor(Outcome) ~ .,
                  data = train,
                  method = "svmLinear2",
                  tuneGrid = tune_grid,
                  trControl = tr)


#Plot SVM
plot(sv_caret)

#View tune cost
sv_caret$bestTune

# Final model
sv_final <- train(as.factor(Outcome) ~ .,
                         data = train,
                         method = "svmLinear2",
                         tuneGrid = expand.grid(cost = sv_caret$bestTune),
                         trControl = trainControl(method = "none"))

#Confusion matrix
svm_pre<-predict(sv_final,newdata=test)
sv_result<-confusionMatrix(data=as.factor(test$Outcome),reference=svm_pre)
#Accuracy rate
sv_result$overall[[2]]

#Error rate
test_error_rate_svm<-1-sv_result$overall[[2]]
L_SVM_Error_Rate[i]<-test_error_rate_svm


sv_roccurve <- roc(response = test$Outcome,predictor=as.numeric(svm_pre))
L_SVM_ROC[[i]]<-ggroc(sv_roccurve, legacy.axes = TRUE, lwd=2) +  theme_bw(base_size = 18) + ggtitle(paste("SVM Linear Kernel ROC BST",i))

L_SVM_AUC[i]<-auc(sv_roccurve)
}


#View ROC
L_SVM_ROC
#View Error Rate
mean(L_SVM_Error_Rate)
#View AUC
mean(L_SVM_AUC)






#Initialization
R_SVM_ROC<-list()
R_SVM_Error_Rate<-rep(NA,5)
R_SVM_AUC<-rep(NA,5)

#SVM Linear Model
for (i in 1:5){
train<-training(bst_sample$splits[[i]])
test<-testing(bst_sample$splits[[i]])
# Train the model with Radial
sv_caret_radial <- train(as.factor(Outcome) ~ .,
                         data = train,
                         method = "svmRadial",
                         preProcess = c("center", "scale"),
                         tuneGrid = expand.grid(C = exp(seq(-7,3,len=11)),sigma = 10^(seq(-3,3,len=7))),
                         trControl = tr)


#Plot SVM
plot(sv_caret_radial)

#View tune cost
sv_caret_radial$bestTune

# Final model
sv_final_radial <- train(as.factor(Outcome) ~ .,
                  data = train,
                  method = "svmRadial",
                  preProcess = c("center", "scale"),
                  tuneGrid = expand.grid(sv_caret_radial$bestTune),
                  trControl = trainControl(method = "none"))

#Confusion matrix
svm_pre_radial<-predict(sv_final_radial,newdata=test)
sv_result_radial<-confusionMatrix(data=as.factor(test$Outcome),reference=svm_pre_radial)
#Accuracy rate
sv_result_radial$overall[[2]]

#Error rate
test_error_rate_svm_radial<-1-sv_result_radial$overall[[2]]
R_SVM_Error_Rate[i]<-test_error_rate_svm_radial


sv_roccurve_radial <- roc(response = test$Outcome,predictor=as.numeric(svm_pre_radial))
R_SVM_ROC[[i]]<-ggroc(sv_roccurve_radial, legacy.axes = TRUE, lwd=2) +  theme_bw(base_size = 18)+ ggtitle(paste("SVM Radial Kernel ROC BST",i))

R_SVM_AUC[i]<-auc(sv_roccurve_radial)
}

#View ROC
R_SVM_ROC
#View Error Rate
mean(R_SVM_Error_Rate)
#View AUC
mean(R_SVM_AUC)





#Initialization
Boost_Error_Rate<-rep(NA,5)
Boost_Error_Rate_p5<-rep(NA,5)
LR_Error_Rate<-rep(NA,5)
LR_Error_Rate_p5<-rep(NA,5)
RF_Error_Rate<-rep(NA,5)
RF_Error_Rate_p5<-rep(NA,5)
All_Error_Rate<-rep(NA,5)
All_Error_Rate_p5<-rep(NA,5)
Boost_AUC<-rep(NA,5)
LR_AUC<-rep(NA,5)
RF_AUC<-rep(NA,5)
Best_AUC<-rep(NA,5)
All_AUC<-rep(NA,5)

for (i in 1:5){
  train<-training(bst_sample$splits[[i]])
  test<-testing(bst_sample$splits[[i]])
  
  # initialize the h2o
  h2o.init()
  
  
  # create the train and test h2o data frames
  train_df_h2o<-as.h2o(train)
  test_df_h2o<-as.h2o(test)
  
  
  
  # Identify predictors and response
  y <- "Outcome"
  x <- setdiff(names(train), y)
  train_df_h2o[,y] <- as.factor(train_df_h2o[,y])
  test_df_h2o[,y] <- as.factor(test_df_h2o[,y])  
  
  # Number of CV folds (to generate level-one data for stacking)
  nfolds <- 5
  # 1. Generate a 3-model ensemble (GBM + RF + Logistic)
  # Train & Cross-validate a GBM
  my_gbm <- h2o.gbm(x = x,
                    y = y,
                    training_frame = train_df_h2o,
                    nfolds = nfolds,
                    keep_cross_validation_predictions = TRUE,
                    seed = 5)
  # Train & Cross-validate a RF
  my_rf <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = train_df_h2o,
                            nfolds = nfolds,
                            keep_cross_validation_predictions = TRUE,
                            seed = 5)
  # Train & Cross-validate a LR
  my_lr <- h2o.glm(x = x,
                   y = y,
                   training_frame = train_df_h2o,
                   family = c("binomial"),
                   nfolds = nfolds,
                   keep_cross_validation_predictions = TRUE,
                   seed = 5)
  # Train a stacked random forest ensemble using the GBM, RF and LR above
  ensemble <- h2o.stackedEnsemble(x = x,
                                  y = y,
                                  metalearner_algorithm="drf",
                                  training_frame = train_df_h2o,
                                  base_models = list(my_gbm, my_rf, my_lr))
  # Eval ensemble performance on a test set
  perf_en <- h2o.performance(ensemble, newdata = test_df_h2o)
  
  # Compare to base learner performance on the test set
  perf_gbm_test <- h2o.performance(my_gbm, newdata = test_df_h2o)
  perf_rf_test <- h2o.performance(my_rf, newdata = test_df_h2o)
  perf_lr_test <- h2o.performance(my_lr, newdata = test_df_h2o)
  max(perf_gbm_test@metrics$thresholds_and_metric_scores$f1)
  
  
  a<-h2o.confusionMatrix(perf_gbm_test, thresholds = NULL, metrics = NULL)
  Boost_Error_Rate[i]<-a$Error[3]
  a_p5<-h2o.confusionMatrix(perf_gbm_test, thresholds = 0.5, metrics = NULL)
  Boost_Error_Rate_p5[i]<-a_p5$Error[3]
  
  b<-h2o.confusionMatrix(perf_rf_test, thresholds = NULL, metrics = NULL)
  RF_Error_Rate[i]<-b$Error[3]
  b_p5<-h2o.confusionMatrix(perf_rf_test, thresholds = 0.5, metrics = NULL)
  RF_Error_Rate_p5[i]<-b_p5$Error[3]
  
  c<-h2o.confusionMatrix(perf_lr_test, thresholds = NULL, metrics = NULL)
  LR_Error_Rate[i]<-c$Error[3]
  c_p5<-h2o.confusionMatrix(perf_lr_test, thresholds = 0.5, metrics = NULL)
  LR_Error_Rate_p5[i]<-c_p5$Error[3]
  
  
  d<-h2o.confusionMatrix(perf_en, thresholds = NULL, metrics = NULL)
  All_Error_Rate[i]<-d$Error[3]
  d_p5<-h2o.confusionMatrix(perf_en, thresholds = 0.5, metrics = NULL)
  All_Error_Rate_p5[i]<-d_p5$Error[3]
  
  plot(perf_gbm_test, type = "roc")
  legend("bottomright",
         legend = paste("Boost ROC BST",i))
  plot(perf_rf_test, type = "roc")
  legend("bottomright",
         legend = paste("Random Forest ROC BST",i))
  plot(perf_lr_test, type = "roc")
  legend("bottomright",
         legend = paste("Logistic ROC BST",i))
  plot(perf_en, type = "roc")
  legend("bottomright",
         legend = paste("Stacking ROC BST",i))
  
  Boost_AUC[i]<-h2o.auc(perf_gbm_test)
  RF_AUC[i]<-h2o.auc(perf_rf_test)
  LR_AUC[i]<-h2o.auc(perf_lr_test)
  
  Best_AUC[i]<- max(h2o.auc(perf_gbm_test), h2o.auc(perf_rf_test), h2o.auc(perf_lr_test))
  All_AUC[i]<- h2o.auc(perf_en)
  print(sprintf("Best Base-learner Test AUC:  %s", Best_AUC[i]))
  print(sprintf("Ensemble Test AUC:  %s", All_AUC[i]))
}

#View Result
mean(Boost_Error_Rate)
#View Result at thresholds=0.5
mean(Boost_Error_Rate_p5)
#View Result
mean(LR_Error_Rate)
#View Result at thresholds=0.5
mean(LR_Error_Rate_p5)
#View Result
mean(RF_Error_Rate)
#View Result at thresholds=0.5
mean(RF_Error_Rate_p5)
#View Stacking Result
mean(All_Error_Rate)
#View Stacking Result at thresholds=0.5
mean(All_Error_Rate_p5)
#View Result
mean(Boost_AUC)
#View Result
mean(RF_AUC)
#View Result
mean(LR_AUC)
#View Best of Base Result
mean(Best_AUC)
#View Stacking Result
mean(All_AUC)

#Save all ROC plots
dev.off()


#Report matrix
report <- rbind(KNN_Error_Rate,NB_Error_Rate,K_NB_Error_Rate,L_SVM_Error_Rate,R_SVM_Error_Rate,Boost_Error_Rate,LR_Error_Rate,RF_Error_Rate,All_Error_Rate,
                KNN_AUC,       NB_AUC,       K_NB_AUC,       L_SVM_AUC,       R_SVM_AUC,       Boost_AUC,       LR_AUC,       RF_AUC,       All_AUC)
report <- cbind(report, rowMeans(report))
colnames(report) <- c("BST_Splt_1", "BST_Splt_2", "BST_Splt_3", "BST_Splt_4", "BST_Splt_5", "AVE")
write.csv(report, file = "Model_Comparison.csv")









