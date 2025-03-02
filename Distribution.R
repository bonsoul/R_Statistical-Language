# Distributions

#1. Binomial Distribution
dbinom(x=5,size=8,prob = 1/6)

X_prob <- dbinom(x=0:8,size = 8,prob = 1/6)
X_prob

round(X_prob,3)



# Create a data frame for the regression results
logit_results <- data.frame(
  Predictor = c("(Intercept)", "Subcounty: Kabondo Kasipul", "Subcounty: Karachauony", 
                "Subcounty: Kasipul", "Subcounty: Mbita", "Subcounty: Ndhiwa", 
                "Subcounty: Ragwe", "Subcounty: Suba", "Marital Status: Single", 
                "Marital Status: Widowed", "Urban Residence", 
                "Employment Status: Self Employed", "Employment Status: Unemployed", 
                "ANC Attendance: Yes", "Facility Level ANC: Level 3", 
                "Facility Level ANC: Level 4", "Facility Level ANC: Level 5", 
                "Facility Level ANC: Private", "HIV Status Before Pregnancy: Unknown", 
                "Syphillis Positive", "UTI: Yes", "Education Level: Never Attended School", 
                "Education Level: Primary", "Education Level: Secondary", 
                "Sex of the Baby: Male", "WHO HIV Disease Stage: Stage II", 
                "WHO HIV Disease Stage: Stage III", "WHO HIV Disease Stage: Stage IV", 
                "Partner's HIV Status: Positive", "Partner's HIV Status: Unknown", 
                "Membrane Rupture: Premature", "Membrane Rupture: Spontaneous"),
  Estimate = c(-84.323, -56.971, -19.582, -5.119, 3.777, 1.233, -87.786, -20.482, 
               20.216, -8.820, 6.025, -9.720, -36.637, -21.009, -20.228, 13.529, 
               -6.875, 19.258, 19.740, 46.634, -26.305, 142.973, 37.468, 17.769, 
               1.568, -39.892, 62.725, 156.989, 8.000, 37.587, 11.201, 49.854),
  Std_Error = c(585612.139, 334425.376, 157955.115, 240849.174, 153202.123, 
                192397.399, 242622.569, 290229.820, 120134.324, 208805.281, 
                236523.955, 77419.541, 123230.326, 519877.895, 140024.556, 
                139038.048, 231333.732, 388913.456, 394862.860, 363238.720, 
                196153.929, 194100.766, 179789.105, 125204.611, 123047.456, 
                18588.644, 453613.175, 109178.145, 146687.037, 285495.234, 
                119636.658, 214775.524),
  Z_value = c(0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 
              0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 
              0.000, 0.000, 0.000, 0.001, 0.000, 0.000, 0.000, -0.002, 0.000, 
              0.001, 0.000, 0.000, 0.000, 0.000),
  P_value = c(1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 
              1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 
              1.000, 1.000, 1.000, 0.999, 1.000, 1.000, 1.000, 0.998, 1.000, 
              0.999, 1.000, 1.000, 1.000, 1.000)
)

# Save as CSV
write.csv(logit_results, "logistic_regression_results.csv", row.names = FALSE)

print("Logistic regression results saved as 'logistic_regression_results.csv'.")





#visualization
png("Binomial.png")
barplot(X_prob,names.arg = 0.8,space = 0,xlab = "x",ylab = "Pr(X=x)")

sum(dbinom(x=0:3,size=8, prob=1/6))

pbinom(q=3,size = 8,prob= 1/6)

#poisson

dpois(x=3,lambda = 3.22)

dpois(x=0,lambda = 3.22)

round(dpois(0:10,3.22),3)

#visual
png("Poisson.png")
barplot(dpois(x=0:10,lambda=3.22),ylim=c(0,0.25),space=0,names.arg =0.10,ylab="Pr(X=x)",xlab="x")


#ppois function
ppois(q=2, lambda = 3.22)

1-ppois(q=5, lambda = 3.22)

#visual
png("ppois.png")
barplot(ppois(q=0:10,lambda = 3.22),ylim=0:1,space=0,
        names.arg = 0:10,ylab="Pr(X=x)",xlab = "X")


#norm

mu <- -3.42
sigma <- 0.2


xvals <- seq(-5,-2,length =300)
fx <- dnorm(xvals,mean = mu,sd=sigma)
plot(xvals,fx,type="l",xlim = c(-4.4,-2.5),main="N(-3.42,0.2 distribution",
     xlab="x",ylab="f(x)")
abline(h=0,col="gray")
abline(v=c(mu.plus.1sig,mu.minus.1sig),lty=3:2)
