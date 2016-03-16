
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

