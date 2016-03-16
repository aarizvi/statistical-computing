rm(list=ls())
set.seed(333)
n <- 20
x <- rnorm(n, mean = 0.5, sd = 1)

xbar <- c()
for (i in 1:1000){
        isample <- sample(x, 20, replace=T)
        xbar[i] <- sum(isample)/n
}

values <- abs(xbar/(1/sqrt(20)))
test <- subset(values, values > 1.96, drop=FALSE)
length(test)/1000

hist(test,col="red",xlim=c(0,7))

plot(density(test), xlim=c(0,7))


###question 1b
rm(list=ls())
x <- seq(-10,10,length=447)
y1 <- dnorm(x,mean=0, sd=1)
plot(x,y1, type="l", lwd=1, xlim=c(-4,4))
y2 <- dnorm(x,mean=0.5, sd=1)
lines(x,y2, type="l", lwd=1, col="red")
points(test,y1, col="red")

points(test,y2, col="blue")



#question 3
rm(list=ls())
set.seed(333)
n <- 25
x <- exp(rnorm(n,mean=0,sd=1))
mu <- mean(x)
sigma <- sqrt(var(x))

estimate.skew <- sum((x-mu)^3/sigma^3)/n
estimate.skew

vboot <- c()
Tboot <- c()
for (j in 1:1000){
        for (i in 1:1000){
                boot.obs <- sample(x, n, replace=T)
                skew <- (boot.obs-mu)^3/sigma^3
                Tboot[i] <- mean(skew)
        }
        vboot[j] <- sqrt(var(Tboot))
}

upperbound <- estimate.skew + 1.96*vboot
lowerbound <- estimate.skew - 1.96*vboot

CIs<- cbind(lowerbound,upperbound)
true.skew <- (exp(1) + 2)*(sqrt(exp(1)-1))
true.skew
true.values <- subset(CIs, CIs[,1] <= true.skew & CIs[,2] >= true.skew, drop=FALSE)

length(true.values)/length(CIs) * 100

