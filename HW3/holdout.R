library(dplyr)
library(readxl)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(caTools)
library(party)

df = readxl::read_xlsx("./HW3/wdbc1.xlsx")

# Split the data
set.seed(88)

# remove the id column which is not required
df <- df[, -1]
df$Diagnosis <- factor(df$Diagnosis)

spl = sample.split(df$Diagnosis, SplitRatio = 0.8)

df_train = subset(df, spl == TRUE)
df_test = subset(df, spl == FALSE)

# model k-fold
fitControl <- trainControl(method = "cv", number = 10, savePredictions = TRUE)
dtree_kfold <- train(Diagnosis ~., data = df_train, method="rpart", trControl=fitControl)

x <- as.vector(dtree_kfold$resample$Accuracy)
print(mean(x))

# Precit on test dataset
#dtree_pred <- predict(dtree_kfold, df_test)

# confusion matric
#cm = confusionMatrix(dtree_pred, df_test$Diagnosis)
#print(cm)

#plot(dtree_pred)

# err_metric=function(CM)
# {
#   TP =CM[1,1]
#   FP =CM[1,2]
#   FN =CM[2,1]
#   TN =CM[2,2]
#   precision =(TP)/(TP+FP)
#   recall_score =(FP)/(FP+TN)
#   f1_score=2*((precision*recall_score)/(precision+recall_score))
#   accuracy_model  =(TP+TN)/(TP+TN+FP+FN)
#   False_positive_rate =(FP)/(FP+TN)
#   False_negative_rate =(FN)/(FN+TP)
#   print(paste("Precision value of the model: ",round(precision,2)))
#   print(paste("Accuracy of the model: ",round(accuracy_model,3)))
#   print(paste("Recall value of the model: ",round(recall_score,2)))
#   print(paste("False Positive rate of the model: ",round(False_positive_rate,2)))
#   print(paste("False Negative rate of the model: ",round(False_negative_rate,2)))
#   print(paste("f1 score of the model: ",round(f1_score,2)))
# }
# 
# err_metric(cm$table)
