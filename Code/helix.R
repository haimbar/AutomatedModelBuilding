library(mvtnorm)
ts <- seq(0,pi/4, length=5000)
xs <- 5*cos(ts)
ys <- 5*sin(ts)
zs <- 2*ts
M <- as.data.frame(cbind(zs, xs, ys) + rmvnorm(5000,rep(0,3),diag(c(0.01,0.01,0.01))))
plot(M[,2],M[,1])
plot(M[,3],M[,1])
colnames(M) <- c("y","x1","x2")
sset <- sample(1:5000, 4000)
maxdeg <- 6
mxm <- 20
res <- fitQRloop(M=M0[sset,], qn = qns, maxdeg = maxdeg, minDiff = minDiff, maxrows = maxrows)

i <- which(qns == 0.5)
plot(M0$y[-sset], predict(res$qremFit[[i]]$fitted.mod, newdata=M[-sset,]),
     pch=19,cex=0.3, col=4)
abline(0,1, col=3, lwd=2)
print(summary(lm(M$y[-sset] ~ predict(res$qremFit[[i]]$fitted.mod, newdata=M[-sset,]))))

