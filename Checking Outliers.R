data("mtcars")
View(mtcars)


#boxplot
png("boxplot_mpg.png")
boxplot(mtcars$mpg, main = "Boxplot for mpg in mtcars", col='lightblue')
dev.off()

#IQR Method
Q1 <- quantile(mtcars$mpg, 0.25)
Q3 <- quantile(mtcars$mpg, 0.75)
IQR_value <- Q3- Q1

#range of outliers
lower_bound <- Q1 -1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

outliers <- mtcars$mpg[mtcars$mpg < lower_bound | mtcars$mpg > upper_bound]
outliers


#z score

z_score <- scale(mtcars$mpg)


#threshold
threshold <- 3

outliers_zscsore <- mtcars$mpg[abs(z_score) > threshold]
outliers_zscsore


#histogram
png("histogram_mpg.png")
hist(mtcars$mpg, main="Histogram for mpg in mtcars", col="lighblue", border="black")

#modified Z-score
mad_value <- mad(mtcars$mpg)
median_value <- median(mtcars$mpg)

modified_score <- 0.6745 * (mtcars$mpg - median_value) / mad_value

#identify outliers
outliers_mad <- mtcars$mpg[abs(modified_score) > 3.5]
outliers_mad


# Kernel Density Plot for 'mpg'
png("Density_mpg.png")
plot(density(mtcars$mpg), main="Density Plot for mpg in mtcars", col="blue")

