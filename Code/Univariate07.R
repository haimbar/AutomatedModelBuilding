source("initSim.R")
N <- 3000
set.seed(211111)
x <- runif(N, min=-1, max=1)
y <- 1/(1+4*x^2) + rnorm(N, 0, 0.02+0.1*abs(x))
M <- data.frame(y,x)
colnames(M) <- c("y", "x1")
D <- 1
minDiff <- -2

res <- fitQRloop(M=M, qn = qns, maxdeg = maxdeg, minDiff = minDiff)
plot(x,y, cex=0.5, pch=19, col="grey66")
for (i in 1:k) {
  lines(sort(x),res$qremFit[[i]]$fitted.mod$fitted.values[order(x)], col=2)
}
