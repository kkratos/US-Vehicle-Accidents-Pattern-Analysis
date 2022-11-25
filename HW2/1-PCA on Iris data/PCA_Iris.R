cat("\f")

library(datasets)
library(ggplot2)
library(cowplot)
library(dplyr)
library(grid)

head(iris)
summary(iris)

# Scatter plot
p1 <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species))+geom_point()
p2 <- ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Species)) + geom_point()
p3 <- ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, color=Species)) + geom_point()
p4 <- ggplot(iris, aes(x=Sepal.Width, y=Petal.Width, color=Species)) + geom_point()

plot_grid(p1,p2,p3, p4, labels = "AUTO")

# Scatter plot
pairs(iris[1:4], main = "Iris Data", pch = 21, bg = c("red", "green", "blue")[unclass(iris$Species)])

# PCA
iris_pca <- iris %>% select(-Species) %>%
  scale() %>%
  prcomp() ->
  pca

summary(pca)
head(pca$x)

# add species data into the PCA data
pca_data <- data.frame(pca$x, Species=iris$Species)
head(pca_data)

q1 = ggplot(pca_data, aes(x=PC1, y=PC2, color=Species))+geom_point()
plot_grid(q1, labels='AUTO')

plot(pca, type="lines")

plot(pca)