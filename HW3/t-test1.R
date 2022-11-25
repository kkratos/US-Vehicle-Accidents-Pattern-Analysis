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

df_tx <- data.frame(dtree_kfold$resample$Accuracy)

pred <- dtree_kfold$pred
pred$equal <- ifelse(pred$pred == pred$obs, 1,0)

eachfold1 <- pred %>%                                        
  group_by(Resample) %>%                         
  summarise_at(vars(equal),                     
               list(Accuracy = mean))              
dt_acc <- eachfold1$Accuracy

# Precit on test dataset
dtree_pred <- predict(dtree_kfold, df_test)

# confusion matric
cm = confusionMatrix(dtree_pred, df_test$Diagnosis)
print(cm)


######## Naive Bayes ########

df_nb <- df
# Split the data

set.seed(88)

data_split <- floor(0.8 * nrow(df_nb))

# df_n <- as.data.frame(lapply(select(df, -Diagnosis), normalize))

train_ind <- sample(seq_len(nrow(df_nb)), size = data_split)
df_train <- df_nb[train_ind, 2:31]
df_test <- df_nb[-train_ind, 2:31]

df_train_labels <- df_nb[train_ind, 1]
df_test_labels <- df_nb[-train_ind, 1]

# nb <- naiveBayes(df_train, df_train_labels)

trctrl <- trainControl(method = "cv", number = 10, savePredictions=TRUE)
nb <- train(factor(Diagnosis) ~., data = df, method = "naive_bayes", trControl=trctrl, tuneLength = 0)
nb

pred <- nb$pred
pred$equal <- ifelse(pred$pred == pred$obs, 1,0)

eachfold2 <- pred %>%                                        
  group_by(Resample) %>%                         
  summarise_at(vars(equal),                     
               list(Accuracy = mean))              
nb_acc <- eachfold2$Accuracy


x <- data.frame("Accuracy 1" = c(nb_acc),
                "Accuracy 2" = c(dt_acc))
