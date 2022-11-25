library(readxl)
library(ggplot2)
library(ggfortify)
library(dplyr)
library(pls)
library(caTools)

energyData = readxl::read_excel("HW2_assignment/3-Energy/Energy.xlsx")

attributes = select(energyData, c(1:8))
target = select(energyData, c(9))

# split data 
split <- sample.split(energyData$Y1, SplitRatio=0.7)
train <- subset(energyData, split == "TRUE")
test <- subset(energyData, split == "FALSE")

pcr_model <- pcr(Y1~., data = train, scale = TRUE)

summary(pcr_model)

scale_data <- as.data.frame(scale(attributes))
pca_res =  prcomp(~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8,
                  data = train,
                  scale = TRUE)

summary(pca_res)

pca_res

pca_data <- data.frame(pca_res$x, train)
head(pca_data)

#q1 = ggplot(pca_data, aes(x=PC1, y=PC2))+geom_point()
#plot_grid(q1, labels='AUTO')

# Variance
var = pca_res$sdev ^ 2 / sum(pca_res$sdev^2)

# Scree plot
qplot(c(1:8), var) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

# Regression

model <- lm(Y1 ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, data = train)
summary(model)

