# Distributions

#1. Binomial Distribution
dbinom(x=5,size=8,prob = 1/6)

X_prob <- dbinom(x=0:8,size = 8,prob = 1/6)
X_prob

round(X_prob,3)

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
