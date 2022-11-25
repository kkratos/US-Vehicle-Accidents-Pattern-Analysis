library(dplyr)
library(readr)
library(readxl)
library(ggplot2)

cat("\f")

data <- read_excel('./4/student.xlsx')
data <- data %>% select(-`Student ID`)

print(glimpse(data))

grade_prob <- data$`Grade Prob`
grade_stat <- data$`Grade Stat`

# box plot for probability
png(file="./4/boxplot_prob.png")
boxplot(grade_prob, main = "probability grade", col='purple')

dev.off()

# box plot for statistics
png(file="./4/boxplot_stat.png")
boxplot(grade_stat, main="statistics grade", col='green')

dev.off()

# histogram plot for probability
png(file="./4/histplot_prob.png")
m <- mean(grade_prob)
std <- sqrt(var(grade_prob))
hist(grade_prob,xlab="grade", col="purple", main = "probability grade", border="black")
dev.off()

# box plot for statistics
png(file="./4/histplot_stat.png")

boxplot(grade_stat, xlab="grade", col="green", main="statistics grade", border="black")

dev.off()

cat("\f")