data("mtcars")


#model
model <- lm(mpg ~ wt, data = mtcars)

#summary
summary(model)


#plotting
png("Linear Regression.png")
plot(mtcars$wt, mtcars$mpg,main="Linear Regression:: mpg vs wt",
     xlab = "Weight (wt)", ylab = "Miles per Gallon (mpg")

#add the regression line
abline(model, col='red')
