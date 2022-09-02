source("initSim.R")
set.seed(211118)
# data generation from Friedberg et al.:
# equation 1
N <- 2000  # 1000, 5000
sigma <- 0.1 # 0.1,  1, 2
D <-   10# 5#, 20
M <- matrix(runif(N*D, min = -1, max=1), nrow=N, ncol=D)
# the true model,  1 variable
y <- log(1+exp(6*M[,1])) + rnorm(N, 0, sigma)
M <- as.data.frame(cbind(y,M))
colnames(M) <- c("y", paste0("x",1:D))

maxdeg <- 10

res <- fitQRloop(M=M, qn = qns, maxdeg = maxdeg, minDiff = minDiff, maxrows = maxrows)
i <- which(qns == 0.5)
plot(M$y, res$qremFit[[3]]$fitted.mod$fitted.values, cex=0.6, pch=19,
     col="grey66", xlab="Observed y", ylab="Predicted")
abline(0,1, col=2, lwd=2)
print(summary(lm(res$qremFit[[3]]$fitted.mod$fitted.values ~ M$y)))
print(res$frmlterms)

plot(M[,2], y, col="grey66", cex=0.5, pch=19)
lines(sort(M[,2]), res$qremFit[[3]]$fitted.mod$fitted.values[order(M[,2])], col=2, lwd=2)
