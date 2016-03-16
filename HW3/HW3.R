rm(list=ls())

####HW 3#####

#question 1a - first show f(x) < ag(x)
#x > 0
x <- seq(0,3,length=1000)
theta <- runif(100,0,max(x))

plot(x,2/(pi*(1+x)*sqrt(x^2 + 2*x)), type="l", lwd=2, ylim=c(0,3), xlim=c(0,2.5), 
     main=expression(f(x) <= c[theta]*g[theta]*(x)),
     xlab="Random Variables",
     ylab="Function")
x1 <- subset(x, 0 <= x & x <= theta  , drop=FALSE)
lines(x1,2/(pi*(sqrt(2*x1))), type="l", col="red", lwd=2)
x2 <- subset(x, x > theta, drop=FALSE)     
lines(x2,2/(pi*x2^2), lwd=2, col="blue")
legend("topright", legend=c("f(x)", expression(c[theta][1]*g[theta][1]), 
                            expression(c[theta][2]*g[theta][2])), 
       col=c("black", "red", "blue"), lwd=c(2,2,2), bty="n")

####question 1b -- first part
x3 <- subset(x, 0 <= x & x <= given.theta  , drop=FALSE)
plot(x3, 2/(pi*(sqrt(2*x3))), type="l", col="red", lwd=2, ylim=c(0,3), xlim=c(0,2.5),
     ylab=expression(c[theta]*g[theta]*(x)),
     xlab="random variables")
x4 <- subset(x, x > given.theta, drop=FALSE)
lines(x4,2/(pi*x4^2), col="blue", type="l", lwd=2)
lines(x,2/(pi*(1+x)*sqrt(x^2 + 2*x)),col="black", type="l", lwd=2)
legend("topright", legend=c(expression(c[theta]*g[theta] == paste(frac(2,pi*sqrt(2*x))),
                                       c[theta]*g[theta] == paste(frac(2,pi*x^2))), expression(f(x))),
       lwd = c(2,2,2),col=c('red', 'blue', 'black'), cex=0.7, bty="n")

####question 1b -- second part -- final optimal c.theta
c.theta <- function(theta){
        (2/pi)*(sqrt(2*theta)+(1/theta))
}
x <- seq(0,10,length=1000)[-1]
min(c.theta(x))
c.theta(2^(1/3))

plot(x,c.theta(x), lwd=2, col="black", type="l", xlim=c(0,2), ylim=c(0,5),
     xlab=expression(theta),ylab=expression(c(theta)))
points(x=(2^(1/3)), y=c.theta(2^(1/3)), col='blue', cex=1, pch=19)
text(x=(2^(1/3)), y=c.theta(2^(1/3)), 
     expression(paste("[",c(theta),",", theta,"]") == paste("[",1.515856,", ",2^{1/3},"]")), 
     col="blue", pos=1)
legend("topright", legend=expression(paste(c[theta] == frac(2,pi) * sqrt(2*theta) + frac(1,theta))),
       col='black', lwd=, cex=1, bty="n")


#####question 1c #### perform generalized rejection to obtain 500 obs from f(x).
####for results construct a histogram of the accepted observations with the pdf f superimposed

x <- seq(0.00001,10,length=500)
cg1 <- function(t){2/(pi*sqrt(2*t))}
cg2 <- function(t){2/(pi*t^2)}
cgx <- function(t, theta=2^(1/3)){
                ifelse(t <= theta, cgx <- cg1(t), cgx <- cg2(t))
}

f <- function(x){
        2/(pi*(1+x)*sqrt(x^2 + 2*x))}


cgoverf <- function(x){cgx(x) / f(x)}

goverf <- function(x){xg / f(x)}

xg <- cgx(x) * (1/c.theta(2^(1/3)))
uc <- runif(500,0,1)
tc <- c.theta(2^(1/3))*sapply(xg, cgoverf)
ut <- uc*tc
hist(xg[ut<=1], prob=T, xlim=c(0,10), ylim=c(0,2), breaks=1000, 
     main="Generalized Rejection for f(x)")
lines(x,f(x), col="red", lwd=4)
sum(ut<=1)/500




#######HW 3 -- question 2
rm(list=ls())
###question 2b
x <- c(seq(-4,-2, length=1000),seq(-2,2,length=10000), seq(2, 4, length=1000))
laplace <- function(t, theta=1){(theta/2)*exp(-(theta)*abs(t))}
cauchy.dist <- function(t, mu=3){(mu/(pi*(mu^2 + t^2)))}
phi <- laplace(x)/cauchy.dist(x)
max(phi)

plot(x,phi, type="l", col="black", lwd=2, ylim=c(0,5))
plot(x, laplace(x), type="l", col='black', lwd=2, xlab="Laplace Random Variables", ylab="Function")
lines(x, cauchy.dist(x), col="red", lwd=2)
lines(x, max(phi)*cauchy.dist(x), col="blue", lwd=2)
legend("topright", legend=c(expression(paste("g(x) * cmax")), "f(x)", "g(x)"), col=c("blue", "black", "red"), 
                            lwd=c(2,2,2), bty="n")

#####question 2a



####question 2c

goverf <- function(t,theta=1, mu=3){y = cauchy.dist(t)/laplace(t);return(y)}
xc <- rcauchy(1000,0,1)
uc <- runif(1000,0,1)
tc <- max(phi)*sapply(xc, goverf)
ut <- uc*tc
hist(xc[ut<=1], prob=T, xlim=c(-5,5), ylim=c(0,.5))
x <- seq(-4,4,length=1000)
lines(x,laplace(x), col="red", lwd=4)
sum(ut<=1)/1000

?rbeta




#####question 3a######

x <- seq(0,1,length=1000)
fx <- function(x, a, b){(1/beta(a,b))*x^(a-1)*(1-x)^(b-1)}
alpha <- c(0.5, 1, 0.5, 1, 2, 5)
beta <- c(0.5, 0.5, 1, 1, 2, 10)
plot(x, fx(x, alpha[1], beta[1]), type = "l", lwd=4, col="black", ylim=c(0,5))
v2<-palette(rainbow(6))
for (i in 1:6){
        lines(x, fx(x, alpha[i], beta[i]), type = "l", lwd=4, col=v2[i])
}
legend("top", legend=c(expression(paste(alpha==0.5, ", ", beta==0.5)),
                       expression(paste(alpha==1.0, ", ", beta==0.5)),
                       expression(paste(alpha==0.5, ", ", beta==1.0)),
                       expression(paste(alpha==1.0, ", ", beta==1.0)),
                       expression(paste(alpha==2.0, ", ", beta==2.0)),
                       expression(paste(alpha==5.0, ", ", beta==10.0))),
       col=c(v2[1:6]), 
       lwd=c(rep(2,6)), bty="n", cex=0.75)



#####question 3B######
alpha <- 1.1
beta <- 2
x <- seq(0,1,length=1000)
pw1 <- (alpha-1)/(alpha+beta-2)
plot(x,fx(x,a=2,b=2), lwd=4, 
     type="l", ylab="f(x)", xlab="Random Variables", xlim=c(0,1), ylim=c(0,2))
x1 <- subset(x, x <= pw1, drop=FALSE)
x2 <- subset(x, x >= pw1, drop=FALSE)
lines(x1, fx(x1,a=1.1, b=2), col="red", lwd=4, type="l")
lines(x2, fx(x2,a=1.1, b=2), col="blue", lwd=4, type="l")
legend("topright",legend=c(expression(x <= frac(alpha-1,alpha+beta-2)), 
                           expression(x >= frac(alpha-1,alpha+beta-2))),
       col=c("red", "blue"),lwd=c(2,2), bty="n")



####question 3C######
####What do you expect as a dominating density to use in the rejection sampling scheme?
maxc <- max(fx(x1,a=0.5,b=0.5))
p <- dbeta(x, 1.1,2)*maxc
plot(x,p, type="l", lwd=4, col="green")
lines(x,dbeta(x,1.1,2), type="l", lwd=2, col="green")
lines(x, fx(x,a=1.1,b=2), lwd=4, type="l")




####question 3D####
rejection.beta <- function(n,a,b){
        y <- numeric()
        for (i in 1:n) {
                repeat{
                        x <- runif(1) 
                        if (runif(1)<(x*(a+b-2)/(a-1))^(a-1)*((1-x)*(a+b-2)/(b-1))^(b-1))
                        {y[i] <- x; break}
                }
        }
        y
}

r.beta1 <- rejection.beta(1000,2,2)
r.beta2 <- rbeta(10000,2,2)
r.beta1

sum(r.beta1 <=0) / 1000
sum(r.beta2 <=1) / 1000


x<-seq(0,1,length=1000)

goverf <- function(x){dbeta(x,1.5,1.5) / fx(x,0.5,0.5)}

xc <- rbeta(x,0.5,0.5)
uc <- runif(1000,0,1)
tc <- maxc*sapply(xc, goverf )
ut <- uc*tc
par(mfrow=c(1,3))
hist(xc[ut<=1], prob=T,
     main="rbeta * max(f(x))", breaks=10)

sum(ut<=1)/1000
x <- seq(-4,4,length=1000)
lines(x,fx(x,0.5,0.5), type="l", col="red", lwd=4)


mean(r.beta1); mean(r.beta2)
sd(r.beta1); sd(r.beta2)
par(mfrow=c(2,1))
hist(r.beta1,freq=F)
lines(0:1000/1000,dbeta(0:1000/1000,2,2))
hist(r.beta2,freq=F)
lines(0:1000/1000,dbeta(0:1000/1000,2,2))

