source("initSim.R")
set.seed(211013)
qns <- as.numeric(format(qns, digits = 2))
plotit <- TRUE
N <- 2000
M <- matrix(runif(N*4, min = 0, max=3), nrow=N, ncol=4)
y <- exp(M[,1])+(M[,4])^2+M[,2]*M[,3]+rlnorm(N, 0, 0.1)
M <- as.data.frame(cbind(y,M))
colnames(M) <- c("y", "x1", "x2","x3","x4")
res <- fitQRloop(M=M, qn = qns, maxdeg = maxdeg, minDiff = minDiff, maxrows = maxrows)

i <- which(qns == 0.5)
plot(M$y, res$qremFit[[i]]$fitted.mod$fitted.values, cex=0.6, pch=19,
     xlab="Actual y", ylab="Predicted y", col="lightblue", axes=F)
axis(1); axis(2)
abline(0,1, col="purple", lwd=2)

print(summary(lm(res$qremFit[[i]]$fitted.mod$fitted.values ~ M$y)))
print(summary(lm(y ~., data=M)))
