
source("initSim.R")
set.seed(211013)

N <- 8000
maxdeg <- 10
mxm <- 20
alphaQ <- 0.01
M <- matrix(runif(N*3, min = -1, max=1), nrow=N, ncol=3)
M[,3] <- (M[,2])^3 + rnorm(N,0,0.05)
y <- sin(pi*M[,1]/2) + M[,2] + M[,3] + rnorm(N,0,0.05)
M0 <- as.data.frame(cbind(y,M))
colnames(M0) <- c("y", "x1", "x2", "x3")
sset <- 1:6000
M <- M0[sset,]
res <- fitQRloop(M=M, qn = qns, maxdeg = maxdeg, minDiff = minDiff, maxrows = maxrows)

res$qremFit[[3]]$fitted.mod$fitted.values
i <- which(qns == 0.5)
plot(M0$y[-sset], predict(res$qremFit[[i]]$fitted.mod, newdata=M0[-sset,]),
     pch=19,cex=0.3, col=4)
abline(0,1, col=3, lwd=2)
cat(mean((M0$y[-sset] - predict(res$qremFit[[i]]$fitted.mod, newdata=M0[-sset,]))^2),"\n")
print(summary(lm(M0$y[-sset] ~ predict(res$qremFit[[i]]$fitted.mod, newdata=M0[-sset,]))))

