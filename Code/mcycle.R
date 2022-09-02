source("initSim.R")
mxm <- 15
reps <- 30
maxdeg <- 50
minDiff <- 1
qns <- c(0.4,0.5,0.6)
k <- length(qns)
dat <- read.csv("../Data/mcycle", sep="\t", header=TRUE)
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
plot(scale(dat$times), dat$accel, cex=0.5, pch=19, col="grey66")
# for (i in 1:k) {
#   lines(sort(x), res$qremFit[[i]]$fitted.mod$fitted.values[order(x)], col=2)
# }

test.data<-data.frame(x1=seq(min(M$x),max(M$x),0.01))
pred<- predict(res$qremFit[[2]]$fitted.mod,newdata=test.data)
lines(test.data$x1, pred, col=3, lty=2, lwd=2)
