library(caret)
library(e1071)

cat("\f")
df <- readxl::read_excel('./HW3/wdbc1.xlsx')
head(df)

# Drop the id number as it is not required in the analysis
df <- df[-1]

df$Diagnosis <- factor(df$Diagnosis, levels=c("B", "M"),
                       labels=c("Benign", "Malignant"))

# Create normalize function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Split the data

set.seed(88)

data_split <- floor(0.8 * nrow(df))

# df_n <- as.data.frame(lapply(select(df, -Diagnosis), normalize))

train_ind <- sample(seq_len(nrow(df)), size = data_split)
df_train <- df[train_ind, 2:31]
df_test <- df[-train_ind, 2:31]

df_train_labels <- df[train_ind, 1]
df_test_labels <- df[-train_ind, 1]

nb <- naiveBayes(df_train, df_train_labels)

# Make predictions on training set
predictTrain = predict(nb, newdata = df_train, type="class")

# Evaluate the model
predictTest <- predict(nb, df_test, type = "class")

cm = confusionMatrix(predictTest, df_test_labels$Diagnosis)
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
  print(paste("Accuracy of the model: ",round(accuracy_model,3)))
  print(paste("Precision value of the model: ",round(precision,3)))
  print(paste("Recall value of the model: ",round(recall_score,3)))
  print(paste("f1 score of the model: ",round(f1_score,3)))
}

err_metric(cm$table)

library(ROCR)

# Prediction function
#ROCRpred = prediction(as.numeric(predictTrain), as.numeric(unlist(df_train_labels)))
ROCRpred = prediction(as.numeric(predictTrain), as.numeric(df_train_labels$Diagnosis))

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf, main="Naive Bayes")

# Add colors
plot(ROCRperf, colorize=TRUE)

abline(a=0 , b=1)

# Add threshold labels
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,0,by=0.1), text.adj=c(-0.2,1.7), main="Naive Bayes")

# Evaluate model using Area Under the ROC Curve (AUC)
as.numeric(performance(ROCRpred, "auc")@y.values)