library(dplyr)
library(readxl)
library(caTools)
library(corrplot)
cat("\f")

set.seed(42)

# read the data
housing = read_excel('./6/house_prediction.xlsx')
colnames(housing)

#summary
summary(housing)

split <- sample(seq_len(nrow(housing)), size = floor(0.75 * nrow(housing)))

train <- housing[split, ]
test <- housing[-split, ]

# all variables
model1 <- lm(PRICE ~ . , data=train)
summary(model1)

# Training the model on train dataset
model <- lm(PRICE ~ SQFT + Acres + Miles_to_Resort, data=train)
summary(model)

# Sum of Squared Errors
SSE = sum(model$residuals^2)
SSE

cor(train$SQFT, train$PRICE)
cor(train$SQFT, train$Acres)
cor(housing)

# Make test set predictions
predictTest = predict(model, newdata=test)
predictTest

# Compute R-squared
SSE = sum((test$PRICE - predictTest)^2)
SST = sum((test$PRICE - mean(housing$PRICE))^2)
1 - SSE/SST


