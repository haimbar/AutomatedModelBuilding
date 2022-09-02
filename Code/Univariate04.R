source("initSim.R")
N <- 2000
set.seed(211111)
x <- runif(N, min=0, max=6)
y <- x*(x<2) + (3+0.5*x)*(x>=2) + rnorm(N, 0, 0.2)
x2 <- as.factor(x<2)
M <- data.frame(y,x, x2)
colnames(M) <- c("y", "x1", "x2")
res <- fitQRloop(M=M, qn = qns, maxdeg = maxdeg, minDiff = minDiff)

M2 <- data.frame(y,x)
colnames(M2) <- c("y", "x1")
res2 <- fitQRloop(M=M2, qn = qns, maxdeg = maxdeg, minDiff = minDiff)

plot(x,y, cex=0.4, pch=19, col="orange", axes=F)
axis(1); axis(2); grid()
i <- which(qns == 0.5)
lines(sort(x),res$qremFit[[i]]$fitted.mod$fitted.values[order(x)], col=4, lwd=5)
lines(sort(x),res2$qremFit[[i]]$fitted.mod$fitted.values[order(x)], col="grey33", lty=2, lwd=3)
