library(dplyr)
library(caTools)
library(ggplot2)
library(caret)

cat("\f")

# Read data
df <- readxl::read_excel("./HW3/wdbc1.xlsx")
df <- df[-1]

df$Diagnosis = factor(df$Diagnosis, labels=c("0", "1"))

table(df$Diagnosis)

# Split the data in to train and test
set.seed(88)

spl = sample.split(df$Diagnosis, SplitRatio = 0.8)

df_train = subset(df, spl==TRUE)
df_test = subset(df, spl==FALSE)

fit_logit <- train(Diagnosis~., data=df_train, method="glm", family="binomial")
fit_predict <- predict(fit_logit, newdata = df_test)

cm = confusionMatrix(df_test$Diagnosis, fit_predict)
print(cm)

confusionMatrix(df_test$Diagnosis, fit_predict, mode="prec_recall")
