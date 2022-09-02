# https://archive.ics.uci.edu/ml/datasets/concrete+compressive+strength
rm(list=ls())
source("initSim.R")
concrete <- read.csv("../Data/Concrete_Data.csv")
M <- concrete[,c(9,1:8)]
M[,9] <- log(1+M[,9])
M <- M + rnorm(prod(dim(M)),0,0.01)
M <- as.data.frame(apply(M, 2, scale))
d <- ncol(M)-1
colnames(M) <- c("y", paste0("x",1:d))
qns <- c(0.4, 0.5, 0.6)
reps <- 10
mxm <- 20
plotit <- FALSE

M0 <- M
res <- lmfit <- tstset <- list()
set.seed(212211)
for (j in 1:reps) {
  cat("\n", j, " ")
  #  sset <- sort(sample(nrow(concrete), 700))
  tstset[[j]] <- sort(c(sample(which(concrete$Age..day. == 3),10),
                        sample(which(concrete$Age..day. == 7),10),
                        sample(which(concrete$Age..day. == 14),10),
                        sample(which(concrete$Age..day. == 28),10),
                        sample(which(concrete$Age..day. == 56),10)))
  sset <- setdiff(1:nrow(concrete), tstset[[j]])
  M <- M0[sset,]
  res[[j]] <- fitQRloop(M=M, qn = qns, maxdeg = maxdeg, maxrows = maxrows)
}

#save(concrete, M, res, file="concreteResults.RData")
scaledStrength <- scale(concrete[,9])
scl <- attributes(scaledStrength)$`scaled:scale`
ctr <- attributes(scaledStrength)$`scaled:center`
r2s <- rmse <- rep(0,k)
for (j in 1:reps) {
  trueY <- concrete[-sset,9]
  predY <- predict(res[[j]]$qremFit[[2]]$fitted.mod, newdata=M0[-sset,])*scl+ctr
  plot(trueY, predY, pch=19,cex=0.3, col=4)
  abline(0,1,col=2,lwd=2)
  cat(mean((M0$y[-sset] - predict(res[[j]]$qremFit[[1]]$fitted.mod, newdata=M0[-sset,]))^2),"\n")
  summlm <- summary(lm(predY ~ trueY))
  print(summlm)
  r2s[j] <- summlm$r.squared
  rmse[j] <- sqrt(mean(summlm$residuals^2))
}
#plot(res[[i]]$qremFit$coef$beta, cex=0.5,pch=19, col=(2+(abs(ztests[[1]])>2)))
#sort(names(ztests[[1]])[which(abs(ztests[[1]]) > 2)])
plot(r2s, ylim=c(0,1), pch=19, cex=0.6, col=4)
abline(h=1,col="grey", lty=2)
grid()
summary(r2s)

j <- which.max(r2s)
predY <- predict(res[[j]]$qremFit[[2]]$fitted.mod, newdata=M0[-sset,])*scl+ctr
plot(trueY, predY, pch=19,cex=0.9, col=4, xlab="observed", ylab="predicted", xlim=c(0,80), ylim=c(0,80), axes=F)
axis(1); axis(2); grid(); abline(0,1,lwd=2,col=2)

res[[j]]$qremFit[[3]]$fitted.mod