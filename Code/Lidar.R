source("initSim.R")
mxm <- 20
reps <- 20
qns <- seq(0.4,0.6,by=0.1)
k <- length(qns)
dat <- read.csv("../Data/lidar.txt",header=FALSE)
x <- scale(dat[,1])
y <- dat[,2]
# augment:
x <- c(x, rep(x, reps)+rnorm(reps*length(x),0, 0.01))
y <- c(y, rep(y, reps)+rnorm(reps*length(y),0, 0.1))
N <- length(y)
M <- as.data.frame(cbind(y, x))
colnames(M) <- c("y", "x1")
D <- ncol(M)-1
res <- fitQRloop(M=M, qn = qns, maxdeg = maxdeg, minDiff = minDiff, maxrows = maxrows)
plot(scale(dat[,1]), dat[,2], cex=0.5, pch=19, col="grey66")
for (i in 1:k) {
  lines(sort(x), res$qremFit[[i]]$fitted.mod$fitted.values[order(x)], col=2)
}
