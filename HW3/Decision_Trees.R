library(dplyr)
library(readxl)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(caTools)
library(party)
library(pROC)

df = readxl::read_xlsx("./HW3/wdbc1.xlsx")

# Split the data
set.seed(88)

# remove the id column which is not required
df <- df[, -1]
df$Diagnosis <- factor(df$Diagnosis)

spl = sample.split(df$Diagnosis, SplitRatio = 0.8)
df_train = subset(df, spl == TRUE)
df_test = subset(df, spl == FALSE)

# model
dtree <- rpart(Diagnosis~., data=df_train, method ="class")
rpart.plot(dtree, extra= 104)

# Make predictions on training set
predictTrain = predict(dtree, type="class")

# Make predictions
PredictTest = predict(dtree, df_test, type = "class")

#Confusion Matrix for Decision Tree
cm = confusionMatrix(df_test$Diagnosis, PredictTest)
print(cm)

confusionMatrix(df_test$Diagnosis, PredictTest, mode = "prec_recall")

library(ROCR)

# Prediction function
ROCRpred = prediction(as.numeric(predictTrain), as.numeric(df_train$Diagnosis))

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,0,by=0.1), text.adj=c(-0.2,1.7), main="Decision Tree")

# Evaluate model using Area Under the ROC Curve (AUC)
as.numeric(performance(ROCRpred, "auc")@y.values)