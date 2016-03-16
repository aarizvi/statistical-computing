setwd("/Users/aarizvi/Desktop/ROSWELL_PHD/STA511_StatisticalComputing/HW1")

#part 2a-1
set.seed(333)
runif.histogram <- runif(1000,0,1)

#part 2a-2
x <- runif.histogram
y <- pi*x - pi/4

pdf("hw1-q2a-runiform-histogram.pdf")
par(mfrow=c(1,2),bg="gray")
hist(runif.histogram, main="X ~ U(0,1) Simulations", xlab = 'X',breaks=25)
hist(y, main="Y = pi/X - (pi/4) Simulations", xlab = 'Y = pi/X - (pi/4)',breaks=20)
dev.off()
?pdf
?hist
#2c 
z <- runif(1000,0,1)
xpz <- x + z
pdf("hw1-q2c-xpz.pdf")
hist(xpz,breaks=15,col="green",main="hist(X + Z)")
dev.off()

#2d
library(MASS)
pdf("hw1q2d-truehist.pdf")
truehist(xpz,main="truehist(X + Z)",ylab="probability")
dev.off()

#2e
#Guess the distribution of X+Z
?hist
?truehist


#question 3
class.roster <- read.table("classlist.txt",sep="\t",header=TRUE)
Percent.Grade <- runif(35, 60, 100)
class.roster.2 <- cbind(class.roster, Percent.Grade)
class.roster.2 <- data.table(class.roster.2)

#remove weird -\xa0 symbol
class.roster.2$Program.and.Plan <- gsub("[-\xa0]", "",class.roster.2$Program.and.Plan)

library(data.table)
class.roster.2[Percent.Grade > 95, Letter.Grade := "A"]
class.roster.2[Percent.Grade >= 90 & Percent.Grade < 95, Letter.Grade := "A-"]
class.roster.2[Percent.Grade >= 85 & Percent.Grade < 90, Letter.Grade := "B+"]
class.roster.2[Percent.Grade >= 80 & Percent.Grade < 85, Letter.Grade := "B"]
class.roster.2[Percent.Grade >= 75 & Percent.Grade < 80, Letter.Grade := "B-"]
class.roster.2[Percent.Grade < 75, Letter.Grade := "C"]

install.packages('xtable')
library(xtable)

roster.table <- xtable(class.roster.2)
print(roster.table)


