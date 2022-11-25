library(ggplot2)
library(datasets)
library(caTools)
library(dplyr)

cat("\f")

# Look at structure
str(iris)

# Table outcome
table(iris$Species)

# Randomly split data
set.seed(88)

# only 2 species virginica and versicolor
iris_vv <- iris %>% filter(Species %in% c("virginica", "versicolor"))

iris_vv$Species<-ifelse(iris_vv$Species=="versicolor",1,0)

split <- sample.split(iris_vv, SplitRatio=0.7)

train <- subset(iris_vv, split == "TRUE")
test <- subset(iris_vv, split == "FALSE")

# logistic regression will all attributes

model1 <- glm(Species ~ Sepal.Length + Sepal.Width + Petal.Width + Petal.Length, data=train, family=binomial)
summary(model1)
# 
# lr_data <- data.frame(
#   predictor = model1$linear.predictors,
#   prob = model1$fitted.values,
#   Species = train$Species
# )
# 
# ggplot(lr_data, aes(x = predictor, y = prob, color = Species)) + 
#   geom_point()

# logistic regression removing variable with max P value (Sepal.Length)
model2 <- glm(Species ~ Sepal.Width + Petal.Width + Petal.Length, data=train, family=binomial)
summary(model2)

lr_data <- data.frame(
  predictor = model2$linear.predictors,
  prob = model2$fitted.values,
  Species = train$Species
)

ggplot(lr_data, aes(x = predictor, y = prob, color = Species)) + 
  geom_point()

# Make predictions on training set
predictTrain = predict(model2, type="response")

# Analyze predictions
summary(predictTrain)
tapply(predictTrain, train$Species, mean)

# Confusion matrix for threshold of 0.5
cm <- table(train$Species, predictTrain > 0.5)
cm 

# Sensitivity and specificity

# Confusion matrix for threshold of 0.7
table(train$Species, predictTrain > 0.7)

# Confusion matrix for threshold of 0.2
table(train$Species, predictTrain > 0.2)

# Sensitivity and specificity

library(ROCR)

# Prediction function
ROCRpred = prediction(predictTrain, train$Species)

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
confusionmatrix <- table(test$Species, PredictTest > 0.5)
confusionmatrix

accuracy <- (confusionmatrix[1, 1] + confusionmatrix[2, 2]) / sum(confusionmatrix) * 100
accuracy