library(dplyr)
library(readr)
library(tidyverse)
library(readxl)
library(ggplot2)
cat("\f")

data <- readxl::read_excel('./1/dataset1.xlsx')

# dataset 1

x1 <- data[,1]
y1 <- data[,2]
data1 = data.frame(x = x1, y = y1)

summary(data1)

mean_x1 = mean(data1$x1)
std_x1 = sd(data1$x1)
corr1 <- cor(data1$x1, data1$y1)

relation1 <- lm(data1$y1 ~ data1$x1)
print(summary(relation1))

png(file='./1/data1.png')

plot(data1$x1, data1$y1, col="blue", main='dataset 1',
     abline(lm(data1$y1~data1$x1)), xlab = "x",ylab = "y")

dev.off()

# dataset 2

x2 <- data[,3]
y2 <- data[,4]
data2 = data.frame(x = x2, y = y2)

mean_x2 <- mean(data2$x2)
std_x2 <- sd(data2$x2)
corr2 <- cor(data2$x2, data2$y2)

relation2 <- lm(data2$y2 ~ data2$x2)
print(summary(relation2))

png(file='./1/data2.png')

plot(data2$x2, data2$y2, col="blue", main='dataset 2',
     abline(lm(data2$y2~data2$x2)), xlab = "x",ylab = "y")

dev.off()

# dataset 3

x3 <- data[,5]
y3 <- data[,6]
data3 <- data.frame(x = x3, y = y3)

mean_x3 <- mean(data3$x3)
std_x3 <- sd(data3$x3)
corr3 <- cor(data3$x3, data3$y3)

relation3 <- lm(data3$y3 ~ data3$x3)
print(summary(relation3))

png(file='./1/data3.png')

plot(data3$x3, data3$y3, col="blue", main='dataset 3',
     abline(lm(data3$y3~data3$x3)), xlab = "x",ylab = "y")

dev.off()

# dataset 4

x4 <- data[,7]
y4 <- data[,8]
data4 <- data.frame(x = x4, y = y4)

mean_x4 <- mean(data4$x4)
std_x4 <- sd(data4$x4)
corr4 <- cor(data4$x4, data4$y4)

relation4 <- lm(data4$y4 ~ data4$x4)
print(summary(relation4))

png(file='./1/data4.png')

plot(data4$x4, data4$y4, col="blue", main='dataset 4',
     abline(lm(data4$y4~data4$x4)), xlab = "x",ylab = "y")

dev.off()

cat("\f")