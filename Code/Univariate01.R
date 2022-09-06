source("initSim.R")
N <- 4000
x <- runif(N, min=0, max=4*pi)
y <- sin(x) + 1.5*x + rnorm(N, 0, 0.5)
M <- data.frame(y=y, x1=x)
res <- fitQRloop(M=M, qn = qns, maxdeg = maxdeg, minDiff = minDiff)
pdf("../Figures/Uni01.pdf", width=5, height=5)
plot(x, y, cex=0.5, pch=19, col="grey66")
for (i in 1:k) {
  lines(sort(x), res$qremFit[[i]]$fitted.mod$fitted.values[order(x)], col=2)
}
dev.off()