source("initSim.R") ##\label{Comment} set up some global parameters \small\dcircle{5} \small\ding{182};
N <- 4000
x <- runif(N,min=0,max=4*pi)
y <- sin(x) + 1.5*x + rnorm(N, 0, 0.5)
M <- data.frame(y,x)
colnames(M) <- c("y", "x1")
D <- ncol(M)-1

res <- fitQRloop(M=M, qn = qns, maxdeg = maxdeg, minDiff = minDiff)
plot(x,y, cex=0.5, pch=19, col="grey66")
for (i in 1:k) {
  lines(sort(x),res$qremFit[[i]]$fitted.mod$fitted.values[order(x)], col=2)
}
