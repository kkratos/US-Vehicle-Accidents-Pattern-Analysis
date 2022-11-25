library(tidyverse)
library(readr)
library(readxl)
library(dplyr)
library(caTools)
library(class)

cat("\f")

df <- readxl::read_excel("./HW3/wdbc1.xlsx")
str(df)

# Drop the ID Feature
df <- df[-1]

# Relabel Diagnosis
df$Diagnosis[df$Diagnosis == 'B'] <- "Benign"
df$Diagnosis[df$Diagnosis == 'M'] <- "Malignant"

# Create normalize function
# normalize <- function(x) {
#   return ((x - min(x)) / (max(x) - min(x)))
# }

# Wisconsin breast cancer data normalized
#df_norm <- as.data.frame(lapply(select(df, -Diagnosis), normalize))

# Split data to test and train
set.seed(88)

data_split <- floor(0.8* nrow(df))
train_ind <- sample(seq_len(nrow(df)), size = data_split)

# train and test labels without label
df_train <- df[train_ind,2:31]
df_test <- df[-train_ind,2:31]

# train and test label
df_train_labels <- df[train_ind, 1]
df_test_labels <- df[-train_ind, 1]

# KNN Model - k=10

df_test_pred <- knn(train = df_train, test = df_test, cl = df_train_labels$Diagnosis, k = 10)
cm = confusionMatrix(table(df_test_labels$Diagnosis, df_test_pred))
print(cm)

confusionMatrix(table(df_test_labels$Diagnosis, df_test_pred), mode="prec_recall")