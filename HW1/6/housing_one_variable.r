library(dplyr)
library(readxl)

cat("\f")

# read the data
house = read_excel('./6/house_prediction.xlsx')
colnames(house)

# Look at structure and summary of data
str(house)
summary(house)

############ Linear Regression (one Variable) ###########
# price (x1) vs SQFT(x2)
model_SQFT = lm(PRICE ~ SQFT, data=house)
summary(model_SQFT)

# Sum of square errors
SSE_SQFT = sum(model_SQFT$residuals^2)
SSE_SQFT

plot(house$SQFT, house$PRICE, xlab = 'SQFT', ylab = 'PRICE', main='SQFT vs PRICE',
     abline(lm(house$PRICE~house$SQFT)))

# price (x1) vs ACRES(x2)
model_Acres = lm(PRICE ~ Acres, data=house)
summary(model_Acres)

SSE_Acres = sum(model_Acres$redisuals^2)
SSE_Acres

# price(x1) vs Miles to Resort(x2)
model_resort = lm(PRICE ~ house$`Miles to Resort`, data=house)
summary(model_resort)

SSE_resort = sum(model_resort$redisuals^2)
SSE_resort

# price(x1) vs Miles to base(x2)
model_base = lm(PRICE ~ house$`Miles to Base`, data=house)
summary(model_base)

SSE_base = sum(model_base$redisuals^2)
SSE_base

#compute R Squares

SSE = sum(model_SQFT$residuals^2)
SST = sum((house$PRICE - mean(house$PRICE))^2)
1 - SSE/SST



#cor(house)