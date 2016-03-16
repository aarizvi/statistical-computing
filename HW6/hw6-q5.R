

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

