source("Code/initSim.R")
set.seed(211013)

N <- 4000
M <- matrix(runif(N*4, min=0, max=1), nrow=N, ncol=4)
# the true model,  3 variables
y <- 2*M[,1] + M[,1]*M[,2]*M[,2]+ M[,4]+rlnorm(N, 0, 0.05)
M <- as.data.frame(cbind(y,M))
colnames(M) <- c("y", "x1", "x2","x3","x4")

res <- fitQRloop(M=M, qn = qns, maxdeg = maxdeg, minDiff = minDiff, maxrows=maxrows)

i <- which(qns == 0.5)
plot(M$y, res$qremFit[[i]]$fitted.mod$fitted.values, cex=0.6, pch=19)
abline(0, 1, col=3)
print(summary(lm(res$qremFit[[i]]$fitted.mod$fitted.values ~ M$y)))
