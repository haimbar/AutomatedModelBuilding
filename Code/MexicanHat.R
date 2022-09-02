#https://en.wikipedia.org/wiki/Ricker_wavelet
source("initSim.R")
set.seed(211013)

N <- 5000
sigma <- 1
M <- matrix(runif(N*2, min = -1, max=1), nrow=N, ncol=2)
#M[,2] <- 2*M[,2]
#Rot <- matrix(c(cos(pi/4), sin(pi/4), -sin(pi/4), cos(pi/4)),2,2)
#M <- as.matrix(M)%*%Rot
y <- ((1-(M[,1]^2+M[,2]^2)/(2*sigma^2)))*
  exp(-(M[,1]^2+M[,2]^2)/(2*sigma^2))/(pi*sigma^4) +
  rnorm(N,0,0.01)
M0 <- as.data.frame(cbind(y,M))
colnames(M0) <- c("y", "x1", "x2")
plot(M0[,2],M0[,3], cex=0.3, pch=19, col=cut(y, breaks=seq(min(y), max(y), length=10)))
sset <- 1:3000
M <- M0[sset,]
D <- 2
minDiff <- -2
res <- fitQRloop(M=M, qn = qns, maxdeg = maxdeg, minDiff = minDiff, maxrows = maxrows)

res$qremFit[[3]]$fitted.mod$fitted.values
i <- which(qns == 0.5)
plot(M0$y[-sset], predict(res$qremFit[[i]]$fitted.mod, newdata=M0[-sset,]),
     pch=19,cex=0.3, col=4)
abline(0,1, col=3, lwd=2)
cat(mean((M0$y[-sset] - predict(res$qremFit[[i]]$fitted.mod, newdata=M0[-sset,]))^2),"\n")
print(summary(lm(M0$y[-sset] ~ predict(res$qremFit[[i]]$fitted.mod, newdata=M0[-sset,]))))

plot(M0[-sset,2], M0[-sset,3], pch=19, cex=0.3,
     col=cut(predict(res$qremFit[[i]]$fitted.mod, newdata=M0[-sset,]),
             breaks=quantile(M0$y, probs = seq(0,1,by=0.1))))
