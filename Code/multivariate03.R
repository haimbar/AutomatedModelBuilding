source("Code/initSim.R")
set.seed(211013)

plotit <- TRUE
N <- 5000
M <- matrix(runif(N*4, min = -1, max=1), nrow=N, ncol=4)
y <- M[,1] - M[,3] + rlnorm(N, 0, 0.1)
M <- as.data.frame(cbind(y, M))
colnames(M) <- c("y", "x1", "x2", "x3", "x4")
res <- fitQRloop(M=M, qn = qns, maxdeg = maxdeg, minDiff = minDiff)

i <- which(qns == 0.5)
plot(M$y, res$qremFit[[i]]$fitted.mod$fitted.values, cex=0.6, pch=19)
abline(0, 1, col=3)
print(summary(lm(res$qremFit[[i]]$fitted.mod$fitted.values ~ M$y)))
