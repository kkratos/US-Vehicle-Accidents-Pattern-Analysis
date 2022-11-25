library(ggplot2)
library(datasets)
library(caTools)
library(dplyr)
library(caret)

cat("\f")

df = readxl::read_xlsx("./HW3/wdbc1.xlsx")

# Split the data
set.seed(88)

df$Diagnosis = factor(df$Diagnosis, labels=c("0", "1"))

split <- sample.split(df[,2:32]$Diagnosis, SplitRatio = 0.8)

# train and test labels without label
df_train <- subset(df[,2:32], split==TRUE)
df_test <- subset(df[,2:32], split==FALSE)

# logistic regression will all attributes

fit_logit <- train(x=df_train[,2:31], y=as.factor(unlist(df_train[,1])), method="glm", family=binomial())
fit_predict <- predict(fit_logit, newdata = df_test)

cm = confusionMatrix(df_test$Diagnosis, fit_predict)
print(cm)

err_metric=function(CM)
{
  TP =CM[1,1]
  FP =CM[1,2]
  FN =CM[2,1]
  TN =CM[2,2]
  precision =(TP)/(TP+FP)
  recall_score =(FP)/(FP+TN)
  f1_score=2*((precision*recall_score)/(precision+recall_score))
  accuracy_model  =(TP+TN)/(TP+TN+FP+FN)
  False_positive_rate =(FP)/(FP+TN)
  False_negative_rate =(FN)/(FN+TP)
  print(paste("Accuracy of the model: ",round(accuracy_model,2)))
  print(paste("Precision value of the model: ",round(precision,2)))
  print(paste("Recall value of the model: ",round(recall_score,2)))
  print(paste("f1 score of the model: ",round(f1_score,2)))
}

err_metric(cm$table)


# ROC Curve
library(ROCR)

# Prediction function
ROCRpred = prediction(predictTrain, df_train$Diagnosis)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

# Evaluate model using Area Under the ROC Curve (AUC)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Make predictions on test set
PredictTest = predict(model2, type="response", newdata=test)

# Confusion matrix for threshold of 0.5
cm <- table(df_test$Diagnosis, PredictTest > 0.5)
cm
