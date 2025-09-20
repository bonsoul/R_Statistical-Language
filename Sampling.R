# Basic sampling
# Sample 5 numbers from 1 to 100 without replacement
sample(1:100, 5)

# Sample with replacement
# This allows repeated values
sample(1:100, 5, replace = TRUE)

# Sampling with probabilities
# Here, elements with higher probability are more likely to be chosen
values <- c("A", "B", "C", "D")
probabilities <- c(0.1, 0.2, 0.3, 0.4)
sample(values, 10, replace = TRUE, prob = probabilities)

# Random permutation of a vector
# If size is not given, sample() will shuffle the input
x <- 1:10
sample(x)  # shuffles 1–10

# Sampling rows from a data frame
df <- data.frame(id = 1:20, score = rnorm(20))
sampled_rows <- df[sample(nrow(df), 5), ]
print(sampled_rows)

