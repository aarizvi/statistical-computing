library(ggplot2)


lambda <- 0.2
#lambda is the rate parameter

nosim <- 1000
n <- 40

set.seed(333)

hist(rexp(40,lambda))



mat <- matrix(rexp(nosim * n, rate=lambda), nosim, n)

sim.mean <- rowMeans(mat)

hist(sim.mean)


sample.mean <- mean(sim.mean)
sample.mean
theoretical.mean <- 1/lambda
theoretical.mean
#pretty close


sim.means <- data.frame(sim.mean)

g <- ggplot(data.frame(sim.mean),aes(x=sim.mean)) + geom_histogram(
        alpha = .20, binwidth=.1, colour = "black", aes(y = ..density..))
g <- g + geom_density(color="red", size = 2)
g <- g + stat_function(fun = dnorm, color="blue", arg=list(mean=5, sd=sqrt(0.625)),size=2)

g

sample.var <- var(sim.mean)
sample.var


theoretical.variance <- (1/lambda)^2/n
theoretical.variance


