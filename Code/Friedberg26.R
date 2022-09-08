source("Code/initSim.R")
# takes several minutes to run
set.seed(211013)

# data generation from Friedberg et al.:
# equation 26
# this one takes a long time
maxdeg <- 14
minDiff <- 2

N <- 2000 #1000, 5000
sigma <- 0.1# 0.1,  1, 2
D <- 10#20#5
M <- matrix(runif(N*D, min = -1, max=1), nrow=N, ncol=D)
# the true model,  1 variable
y <- 10/(1+exp(-10*(M[,1]-0.5))) + 5/(1+exp(-5*(M[,2]-0.5))) + rnorm(N, 0, sigma)
M <- as.data.frame(cbind(y,M))
D <- ncol(M)-1
colnames(M) <- c("y", paste0("x",1:D))
res <- fitQRloop(M=M, qn = qns, maxdeg = maxdeg, minDiff = minDiff, maxrows = maxrows)

i <- which(qns == 0.5)
plot(M$y, res$qremFit[[3]]$fitted.mod$fitted.values, cex=0.6, pch=19,
     col="grey66", xlab="Observed y", ylab="Predicted")
abline(0,1, col=2, lwd=2)
print(summary(lm(res$qremFit[[3]]$fitted.mod$fitted.values ~ M$y)))
print(res$frmlterms)
