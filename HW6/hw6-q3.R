####question 3####
#part a
rm(list=ls())
set.seed(333)
f.pdf <- function(x){
        if(-1 <= x & x <= 0){
                x + 1
        }else if(0 <= x & x <= 1){
                -x + 1
        }
        else{
                0
        }
}

c.max <- optimize(f.pdf, interval=c(-1,1), maximum=TRUE)$objective

#simple rejection
xc <- runif(100,-1,1) #generate x uniform[a,b]
uc <- runif(100,0,1) #generate uniform [0,1]
tc <- c.max/sapply(xc,f.pdf) #t = c/fx
ut <- uc*tc
accept.obs <- xc[ut <= 1]
length(accept.obs)/100 #accept 50.6%

plot(density(accept.obs))
hist(accept.obs, prob=T, breaks=20, col="gray", main="", xlab="Accepted Observations")

#part b
#inversion method
icdf <- 





