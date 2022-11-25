library(dplyr)
library(readxl)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(caTools)
library(ipred)

cat("\f")

df <- readxl::read_excel("./HW3/wdbc1.xlsx")

# Split the data
set.seed(88)

#spl = sample.split(df$Diagnosis, SplitRatio = 0.8)

#df_train = subset(df, spl==TRUE)
#df_test = subset(df, spl==FALSE)

# Bootstrap
trainCtrl = trainControl(method = "boot", number = 20)
train_bootstrap = train(factor(Diagnosis)~., data = df, method="rpart", trControl=trainCtrl, tuneLength=0)
#test_bootstrap = predict(train_bootstrap, df_test, type="raw")

print(train_bootstrap)