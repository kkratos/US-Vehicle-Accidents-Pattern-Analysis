library(dplyr)
library(readxl)

cat("\f")

# read the data
house = read_excel('./6/house_prediction.xlsx')
colnames(house)

# Linear Regression (two variables)

# SQFT + ACRES - 12
model12 = lm(PRICE ~ SQFT + Acres, data=house)
summary(model12)

# Sum of Squared Errors
SSE12 = sum(model12$residuals^2)
SSE12


# SQFT + Miles to resort - 13
model13 = lm(PRICE ~ SQFT + house$`Miles to Resort`, data=house)
summary(model13)

# Sum of Squared Errors
SSE13 = sum(model13$residuals^2)
SSE13

# SQFT + Miles to base - 23
model23 = lm(PRICE ~ Acres + house$`Miles to Resort`, data=house)
summary(model23)

# Sum of Squared Errors
SSE23 = sum(model23$residuals^2)
SSE23
