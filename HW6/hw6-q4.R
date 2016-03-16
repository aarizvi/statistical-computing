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