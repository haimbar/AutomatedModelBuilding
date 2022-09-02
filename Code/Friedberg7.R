source("initSim.R")
set.seed(211013)
plotit <- FALSE
maxrows <- 5000
qns <- 1:5/6

# data generation from Friedberg et al.:
# equation 7

N <- 2000 # or 1000, 5000
sigma <- 0.1 # or  1, 2
d <- 20 # or 5, 10
M <- matrix(runif(N*d, min = 0, max=1), nrow=N, ncol=d)
# the true model,  1 variable
y <- 10*sin(pi*M[,1]*M[,2]) + 20*(M[,3]-0.5)^2 + 10*M[,4] + 5*M[,5] + rnorm(N, 0, sigma)
M <- as.data.frame(cbind(y, M))
colnames(M) <- c("y", paste0("x", 1:d))
res <- fitQRloop(M=M, qn = qns, maxdeg = maxdeg, minDiff = minDiff, maxrows = maxrows)

i <- which(qns == 0.5)
plot(M$y, res$qremFit[[3]]$fitted.mod$fitted.values, cex=0.6, pch=19,
     col="grey66", xlab="Observed y", ylab="Predicted", axes=F)
abline(0,1, lwd=2, col="orange"); axis(1); axis(2); grid()
print(summary(lm(res$qremFit[[3]]$fitted.mod$fitted.values ~ M$y)))
print(res$frmlterms)
