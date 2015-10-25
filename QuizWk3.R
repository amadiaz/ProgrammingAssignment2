library(datasets)
data(iris)
?iris
View(iris)
only.virginica <- iris[which(iris$Species=='virginica'), ]
mean(only.virginica$Sepal.Length)

# vector of the means of the variables 'Sepal.Length', 'Sepal.Width', 'Petal.Length', and 'Petal.Width'?

cub <- apply(iris[, 1:4], 2, mean)
class(cub)

library(datasets)
data(mtcars)
?mtcars
View(mtcars)
# How can one calculate the average miles per gallon (mpg) by number of cylinders in the car (cyl)?
with(mtcars, tapply(mpg, cyl, mean))

# what is the absolute difference between the average horsepower of 4-cylinder cars and the average horsepower of 8-cylinder cars?
cyl4 <- mtcars[which(mtcars$cyl=='4'), ]
cyl8 <- mtcars[which(mtcars$cyl=='8'), ]
avg.hrs4 <- mean(cyl4$hp)
avg.hrs8 <- mean(cyl8$hp)
avg.hrs8 - avg.hrs4
debug(ls)
ls()
