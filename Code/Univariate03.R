source("Code/initSim.R")
N <- 5000
set.seed(211111)
x <- runif(N, min=0, max=1)
y <- abs(x-0.35) + rlnorm(N, 0, 0.1*(x+0.1)) # EXAMPLE 2
M <- data.frame(y,x)
colnames(M) <- c("y", "x1")
D <- ncol(M)-1
minDiff <- -2

res <- fitQRloop(M=M, qn = qns, maxdeg = maxdeg, minDiff = minDiff)
plot(x,y, cex=0.5, pch=19, col="grey66")
for (i in 1:k) {
  lines(sort(x),res$qremFit[[i]]$fitted.mod$fitted.values[order(x)], col=2)
}
