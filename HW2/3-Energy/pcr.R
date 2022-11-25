library(readxl)
library(ggplot2)
library(dplyr)
library(pls)
library(Metrics)

energyData = readxl::read_excel("HW2_assignment/3-Energy/Energy.xlsx")

# split data 
split <- sample.split(energyData$Y1, SplitRatio=0.7)
train <- subset(energyData, split == "TRUE")
test <- subset(energyData, split == "FALSE")

# PCR
pcr_model <- pcr(Y1 ~ ., data=train, scale=TRUE)
summary(pcr_model)

pcr_pred <- predict(pcr_model, test, ncomp=5)
rmse(actual= test$Y1, predicted=as.numeric(pcr_pred))

# PCA
pca<- prcomp(~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8,
                 data = train,
                 scale = TRUE)
summary(pca)
screeplot(pca, type='l', main='Screeplot')

# Transform our test observation into linearly fit 
transform_test <- as.data.frame(predict(pca, test)[,1:5])

new_train <- as.data.frame(cbind(train$Y1, pca$x[,1:5]))
colnames(new_train)[1] <- "Y1"
str(new_train)

# Regression
pcr_lm_model <- lm(Y1~., data=new_train)
summary(pcr_lm_model)

pcr_predictions <- predict(pcr_lm_model, transform_test)
summary(pcr_predictions)

