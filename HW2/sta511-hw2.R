##############################################
##############################################
##### Implementation of Newton-Raphson Method
##### Beta Distribution
##############################################
##############################################
rm(list=ls())
set.seed(333)
nsims <- 100
iterations <- c()
X.new <- c()
random.dist <- runif(1000)
for(i in 1:nsims){
        U <- random.dist[i]
        alpha <- 3
        beta <- 5
        x.old <- 0.5
        N <- 10 #random number that is bigger than 1 so 'j' in the while loop keepings resetting
        j <- 1
        while(j <= N){
                x.new <- x.old - ((pbeta(x.old, alpha, beta) - U)/dbeta(x.old, alpha, beta))
                j <- j + 1
                if(abs(x.new - x.old) < 0.05) {
                        break
                }
                x.old <- x.new
        }
        X.new[i] <- x.new
        print(X.new)
        iterations[i] <- j
}

mean(iterations)

pdf("hw2-q2.pdf")
hist(X.new, xlim=c(0,0.8), ylim=c(0,4), probability=TRUE, breaks=20, col="red",
     main="Newton-Raphson Method Accepted Observations")
dev.off()



#################################
#################################
######### question 3a ###########
#################################
#################################
rm(list=ls())
set.seed(333)
sample.size <- c(10, 25, 50, 100) #n = 10, 25, 50, 100
nsims <- 500

mat <- matrix(c(1:2*length(sample.size)),ncol=2, nrow=length(sample.size))

for (i in 1:length(sample.size)){
        ks.results <- list()
        chisq.results <- list()
        for (j in 1:nsims){
                r <- runif(sample.size[i])
                ks.results[j] <- ks.test(r, "punif", 0, 1)$p.value #punif because you need to compare cdf
                r.counts <- hist(r, breaks=10, plot=FALSE)$counts #using 10 bins for making counts
                chisq.results[j] <- chisq.test(r.counts)$p.value
        }
        chi.final <- list(sum(chisq.results <= 0.05)/500)
        ks.final <- list(sum(ks.results <= 0.05)/500)
        mat[i,1:2] <- do.call("cbind",c(chi.final,ks.final))
}
colnames(mat) <- c("Chi-square Test", "Kolmogorov-Smirnoff Test")
rownames(mat) <- paste("n", sample.size, sep=" = ")
print(mat)

library(xtable)


xmat <- xtable(mat)
print(xmat)

###############################
##########question 3b #########
###############################
###############################
rm(list=ls())

set.seed(333)
sample.size <- c(10, 25, 50, 100)
shape1 <- 3
shape2 <- 5
nsims <- 500

mat <- matrix(c(1:2*length(sample.size)),ncol=2, nrow=length(sample.size))

for (i in 1:length(sample.size)){
        ks.results <- list()
        chisq.results <- list()
        for (j in 1:nsims){
                r <- rbeta(sample.size[i], shape1, shape2) #rbeta with shape1=3, shape2=5
                ks.results[j] <- ks.test(r, "pbeta", 0, 1)$p.value #pbeta because you need to compare cdf
                r.counts <- hist(r, breaks=10, plot=FALSE)$counts #using 10 bins for making counts
                chisq.results[j] <- chisq.test(r.counts)$p.value
        }
        chi.final <- list(sum(chisq.results <= 0.05)/500)
        ks.final <- list(sum(ks.results <= 0.05)/500)
        mat[i,1:2] <- do.call("cbind",c(chi.final,ks.final))
}
colnames(mat) <- c("Chi-square Test", "Kolmogorov-Smirnoff Test")
rownames(mat) <- paste("n", sample.size, sep=" = ")

xmat <- xtable(mat)
print(xmat)

##########question 4###########
rm(list=ls())
set.seed(333)
sample.size <- c(10,25,50,100,500)
bins <- c(10,25,50,100,500)
mat <- matrix(c(length(sample.size)*length(bins)),ncol=length(sample.size), nrow=length(bins))
nsims <- 500

for (i in 1:length(sample.size)){
        for (p in 1:length(bins)){
                chisq.results <- NULL             
                for (j in 1:nsims){
                        r <- round(runif(sample.size[i]),3)
                        r.counts <- hist(r, breaks=bins[p], plot=FALSE)$counts 
                        chisq.results[j] <- chisq.test(r.counts)$p.value
                }
                chi.final <- sum(chisq.results <= 0.05)/500
                mat[p,i] <- chi.final
        }
}
rownames(mat) <- paste("bin", bins, sep = " = ")
colnames(mat) <- paste("n", sample.size, sep = " = ")
print(mat)

xmat <- xtable(mat)
print(xmat)
