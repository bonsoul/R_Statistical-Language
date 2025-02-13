# data manipulation

library(tidyverse)

data("iris")

#view
head(iris)


#reshape the data

iris_long <- iris %>%
  pivot_longer(
    cols = c(Sepal.Length,Sepal.Width,Petal.Width),
    names_to = "measurement",
    values_to = "value"
  )

#view
print(iris_long)


#mutate
iris_long <- iris_long %>%
  mutate(
    category = case_when(
      measurement == "Sepal.Length" & value > 5.5 ~ "High",
      measurement == "Sepal.Width" & value > 3.0 ~ "High",
      measurement == "Petal.Length" & value > 4.0 ~ "High",
      measurement == "Petal.Width" & value > 1.2 ~ "High",
      TRUE ~ "Low"
    )
  )

iris_long

print(n = 30,iris_long)
