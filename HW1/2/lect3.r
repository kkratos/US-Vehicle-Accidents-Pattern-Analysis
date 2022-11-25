library(dplyr)
library(readr)
library(tidyverse)
library(readxl)
library(ggplot2)

cat("\f")

data <- read_excel('dataset1.xlsx')

# dataset 1

x1 <- data[,1]
y1 <- data[,2]
data1 = data.frame(x = x1, y = y1)

summary(data1)

mean_x1 = mean(data1$x1)
std_x1 = sd(data1$x1)
corr1 <- cor(data1$x1, data1$y1)

model1 <- lm(data1$y1 ~ data1$x1, data=data1)
print(summary(model1))

# get residual
res1 <- resid(model1)

png(file='Q-Q-1.png')

qqnorm(res1)
qqline(res1, col = "steelblue") 

dev.off()

# dataset 2

x2 <- data[,3]
y2 <- data[,4]
data2 = data.frame(x = x2, y = y2)

mean_x2 <- mean(data2$x2)
std_x2 <- sd(data2$x2)
corr2 <- cor(data2$x2, data2$y2)

model2 <- lm(data2$y2 ~ data2$x2, data=data2)
print(summary(model2))

# get residual
png(file='Q-Q-2.png')

res2 <- resid(model2)

qqnorm(res2)
qqline(res2, col = "steelblue") 

dev.off()

# dataset 3

x3 <- data[,5]
y3 <- data[,6]
data3 <- data.frame(x = x3, y = y3)

mean_x3 <- mean(data3$x3)
std_x3 <- sd(data3$x3)
corr3 <- cor(data3$x3, data3$y3)

model3 <- lm(data3$y3 ~ data3$x3)
 
# get residual and plot normal probability plot
res3 <- resid(model3)

png(file='Q-Q-3.png')

qqnorm(res3)
qqline(res3, col = "steelblue") 

dev.off()

# dataset 4

x4 <- data[,7]
y4 <- data[,8]
data4 <- data.frame(x = x4, y = y4)

mean_x4 <- mean(data4$x4)
std_x4 <- sd(data4$x4)
corr4 <- cor(data4$x4, data4$y4)

model4<- lm(data4$y4 ~ data4$x4)

# get residual and plot normal probability plot
res4 <- resid(model4)

png(file='Q-Q-4.png')

qqnorm(res4)
qqline(res4, col = "steelblue") 

dev.off()

cat("\f")