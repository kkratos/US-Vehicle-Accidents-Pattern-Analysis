library(readxl)

cat("\f")

# Read in data
data = read_excel('./5/cigarettes_and_heartdisease.xlsx')

# Look at structure and summary of data
str(data)
summary(data)

# Linear Regression (one variable)
model1 = lm(CHD ~ CIG, data=data)
summary(model1)

# Linear Regression (one variable) without intercept
model2 = lm(CHD ~ CIG - 1, data=data)
summary(model2)

png(file="./5/CIG_CDH.png")

plot(data$CIG, data$CHD, col="blue", main='relationship
between cigarettes and heart disease in 21 countries',
     abline(lm(data$CHD~data$CIG)), xlab = "x",ylab = "y")

dev.off()

# Sum of Squared Errors
SSE = sum(model1$residuals^2)
SSE

# Compute R-squared
SST = sum((data$CHD - mean(data$CHD))^2)
1 - SSE/SST
