source("initSim.R")
N <- 2000
set.seed(211111)
x <- runif(N, min=0, max=4*pi)
y <- (x-6)^2 + rnorm(N, 0, 0.2*x)
M <- data.frame(y,x)
colnames(M) <- c("y", "x1")
D <- 1

res <- fitQRloop(M=M, qn = qns, maxdeg = maxdeg, minDiff = minDiff)
plot(x,y, cex=0.5, pch=19, col="grey66")
for (i in 1:k) {
  lines(sort(x),res$qremFit[[i]]$fitted.mod$fitted.values[order(x)], col=2)
}
