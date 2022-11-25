# R Script File for Linear Regression in R 
cat("\f")

library(dplyr)
library(readr)
library(tidyverse)
library(readxl)
library(ggplot2)
library(corrplot)
# Read in data
energy = read.csv("./7/energy.csv")
energy

# Look at structure and summary of data
str(energy)
summary(energy)

#Linear Regression
model <- lm(Energy ~ ., data=energy)
summary(model)

# graph of y vs each x's

png(file='./7/energy_x1.png')

plot(energy$Units.Produced, energy$Energy, col="blue", main='energy vs unit produced',
     abline(lm(energy$Energy~energy$Units.Produced)), xlab = "Unit Produced",ylab = "Energy")

dev.off()

# Total equip run time
png(file='./7/energy_x2.png')

plot(energy$Total.Equip.Run.Time, energy$Energy, col="blue", main='energy vs total equip run time',
     abline(lm(energy$Energy~energy$Total.Equip.Run.Time)), xlab = "Total Equip Run Time",ylab = "Energy")

dev.off()

# Employee Hours
png(file='./7/energy_x3.png')

plot(energy$Employee.Hours, energy$Energy, col="blue", main='energy vs Employee Hours',
     abline(lm(energy$Energy~energy$Employee.Hours)), xlab = "Employee Hours",ylab = "Energy")

dev.off()

# Employee Mean Temp
png(file='./7/energy_x4.png')

plot(energy$Mean.Temp, energy$Energy, col="blue", main='energy vs Mean Temp',
     abline(lm(energy$Energy~energy$Mean.Temp)), xlab = "Mean Temp",ylab = "Energy")

dev.off()

# Employee Min Temp
png(file='./7/energy_x5.png')

plot(energy$Min.Temp, energy$Energy, col="blue", main='energy vs Min Temp',
     abline(lm(energy$Energy~energy$Min.Temp)), xlab = "Min Temp",ylab = "Energy")

dev.off()


# Employee Max Temp
png(file='./7/energy_x6.png')

plot(energy$Max.Temp, energy$Energy, col="blue", main='energy vs Max Temp',
     abline(lm(energy$Energy~energy$Max.Temp)), xlab = "Max Temp",ylab = "Energy")

dev.off()

# Employee Pct Sun
png(file='./7/energy_x7.png')

plot(energy$Pct.Sun, energy$Energy, col="blue", main='energy vs Pct Sun',
     abline(lm(energy$Energy~energy$Pct.Sun)), xlab = "Pct Sun",ylab = "Energy")

dev.off()

# Employee Avg Equip Age
png(file='./7/energy_x8.png')

plot(energy$Avg.Equip.Age, energy$Energy, col="blue", main='energy vs Avg Equip Age',
     abline(lm(energy$Energy~energy$Avg.Equip.Age)), xlab = "Avg Equip Age",ylab = "Energy")

dev.off()
