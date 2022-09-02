source("initSim.R")
qns <- round(qns, digits=3)
N <- 5000
set.seed(211111)
x <- runif(N, min=0, max=4*pi)
y <- exp(-x)*x^5 + rnorm(N, 0, 0.25*(x+0.05)) # EXAMPLE 1
M <- data.frame(y,x)
colnames(M) <- c("y", "x1")
res <- fitQRloop(M=M, qn = qns, maxdeg = maxdeg, minDiff = minDiff)
plot(x,y, cex=0.5, pch=19, col="grey66", axes=F)
axis(1); axis(2); grid()
for (i in 1:k) {
  lines(sort(x),res$qremFit[[i]]$fitted.mod$fitted.values[order(x)], col=2)
}
