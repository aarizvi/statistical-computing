
##########################
####### question 1A ######
##########################
rm(list=ls())
foverg <- function(theta){(sqrt(2/pi)*exp(theta^2/2)/theta)}

theta <- seq(0.000001, 10, length=1000)

opt.theta <-nlminb(0.00001,function(theta){foverg(theta)})$par
c <- nlminb(0.00001,function(theta){foverg(theta)})$objective

plot(theta, 
     (sqrt(2/pi)*exp(theta^2/2))/theta, 
     type="l", 
     col="red", 
     lwd=2, 
     ylim=c(0,5), 
     xlim=c(0,3),
     ylab=expression(c[theta]),
     xlab=expression(theta))

x <- 1.0001
points(x, y=sqrt(2/(pi*x^2))*exp((-x^2/2)+x^2), col="blue", pch=20)
legend("topright",legend=c(expression(optimal[theta])),
       col="blue",pch=20, bty="n")
ub
opt.theta <- 1.00001
max(sqrt(2/(pi*opt.theta^2))*exp((-opt.theta^2/2)+opt.theta^2))


##########################
####### question 1B ######
##########################
#inversion method
laplace.inv <- function(p){
       if(p < 1/2){
               log(2*p, base=exp(1))
       } else {
               -log(2*(1-p), base=exp(1))
       }
}

p <- runif(1000)
laplace.rv <- sapply(p,laplace.inv) #random variables from laplace

hist(laplace.rv)

laplace.dist <- function(x){ #theta as 1
        if(x<0){
                (1/2)*exp(x)
        }else {
                (1/2)*exp(-x)
        }
}

rvs <- seq(-6,6,length=1000)
laplace.pdf <- sapply(rvs,laplace.dist)
hist(laplace.rv, ylim=c(0,0.5), col="gray", xlab="random variables", prob=T, breaks=50, main="")
lines(density(laplace.rv), col="blue", lwd=2)
lines(rvs, laplace.pdf, col="red", lwd=2, lty=2)
legend("topleft",legend=c("Theoretical","Empirical"),
       col=c("red", "blue"),lwd=c(2,2), bty="n")

#generalized rejection method
goverf <- function(x,theta=1){y = sqrt(pi/2)*theta*exp((x^2/2)-theta*abs(x))}
xc <- laplace.rv
uc <- runif(1000,0,1)
tc <- c*sapply(xc,goverf) #a*g(x)/f(x)
ut <- uc*tc
accept.obs <- xc[ut <= 1]
length(accept.obs)/1000

nseq <- seq(-3,3,length=1000)
norm <- dnorm(nseq)

hist(accept.obs, col="gray", prob=T, breaks=50, ylim=c(0,0.6), xlab = "Accepted Observations", main="")
lines(nseq, norm, col="red", lwd=2)
lines(density(accept.obs), col="blue", lwd=2)
legend("topleft",legend=c("Theoretical","Empirical"),
       col=c("red", "blue"),lwd=c(2,2), bty="n")


####question 1c#####
#ks-test
ks.test(accept.obs, "pnorm", 0,1)
shapiro.test(accept.obs)


##########################
####### question 4A ######
##########################

#install.packages("zoo")
rm(list=ls())
require(zoo)
set.seed(333)
coinflip <- function(seq){
        flip <- c()
        ht <- c()
        match <- FALSE
        while (any(match) == FALSE){
                flip <- sample(c("H", "T"), 1, replace = TRUE, prob=c(0.5,0.5))
                ht <- c(ht,flip)
                if (length(ht) > 3){
                        match <- rollapply(ht,3,identical,seq)
                }
        }
        return(length(match)+1)
}

num.trials <- 1000
trials <- data.frame("hth"=rep(NA,num.trials),"htt"=rep(NA,num.trials))
for (i in 1:num.trials){
        trials[i,] <- c(coinflip(c("H","T","H")), coinflip(c("H","T","T")))
}
summary(trials)

##########################
####### question 4B ######
##########################
rm(list=ls())
set.seed(333)
require(zoo)
num.trials <- 10
trials <- data.frame("hth"=rep(NA,num.trials),"htt"=rep(NA,num.trials))
for (j in 1:num.trials){
        coin <- c('h','t')
        s <- sample(x=coin, size=100000, replace=T, prob=c(0.5,0.5))       
        hth <- length(which(rollapply(s,3,identical,c("h","t","h"))))
        htt <- length(which(rollapply(s,3,identical,c("h","t","t"))))
        trials[j,] <- c(hth,htt)
}
stats <- summary(trials)


##########################
####### question 5D ######
##########################
rm(list=ls())

set.seed(333)
x.i <- runif(10,1,5)
tau.mle <- (5+min(x.i))/2
tau.m <- mean(x.i)
t.mle <- c()
t.mom <- c()
for(i in 1:10000){
        boot <- sample(x.i, 10, replace = T)
        t.mle[i] <- (5+min(boot))/2
        t.mom[i] <- mean(boot)
}
ci.mle <- c(quantile(t.mle,0.025),quantile(t.mle,0.975))
ci.mom <- c(quantile(t.mle,0.025),quantile(t.mom,0.975))

ci.mle
ci.mom
