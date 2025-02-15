data("mtcars")
View(mtcars)


#boxplot
boxplot(mtcars$mpg, main = "Boxplot for mpg in mtcars", col='lightblue')


#IQR Method
Q1 <- quantile(mtcars$mpg, 0.25)
Q3 <- quantile(mtcars$mpg, 0.75)
IQR_value <- Q3- Q1

#range of outliers
lower_bound <- Q1 -1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

outliers <- mtcars$mpg[mtcars$mpg < lower_bound | mtcars$mpg > upper_bound]
outliers

