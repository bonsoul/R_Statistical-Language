data("mtcars")


#model
model <- lm(mpg ~ wt, data = mtcars)

#summary
summary(model)
