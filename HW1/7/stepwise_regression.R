# R Script File for Linear Regression in R 
cat("\f")

library(dplyr)
library(readr)
library(tidyverse)
library(readxl)
library(ggplot2)
library(corrplot)
library(GGally)
library(ggplot2)

# Read in data
energy = read.csv("./7/energy.csv")
energy

# Look at structure and summary of data
str(energy)
summary(energy)

#Linear Regression
#model <- lm(Energy ~ ., data=energy)
#summary(model)

############ Linear Regression (one Variable) ###########
# Energy (x1) vs Unit.Produced(x2)
model = lm(Energy ~ Total.Equip.Run.Time + Max.Temp + Avg.Equip.Age, data=energy)
summary(model)


# Residual Model
res <- resid(model)
png(file='./7/Q-Q-1.png')

qqnorm(res)
qqline(res, col = "steelblue") 

dev.off()

pairs(~ Energy + Total.Equip.Run.Time + Avg.Equip.Age + Mean.Temp + Max.Temp + Min.Temp + Pct.Sun + Employee.Hours + Units.Produced, data= energy, main = "Energy")

# box plot for statistics

png(file="./7/res_boxplot.png")
boxplot(res, main="residuals", ylab='res')
dev.off()

