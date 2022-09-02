#https://www.tensorflow.org/tutorials/keras/regression
#urlname = 'http://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data'
#mpgdat <- download.file(urlname, "~/Downloads/mpg.dat")

# https://www.tensorflow.org/decision_forests/migration#validation_dataset

source("initSim.R")
library(sampling)  

mpgdat <- read.csv("../Data/auto-mpg.data", sep=",", header = F)
colnames(mpgdat) <- c('MPG', 'Cylinders', 'Displacement', 'Horsepower', 'Weight',
                      'Acceleration', 'Model Year', 'Origin')
head(mpgdat)
M <- as.data.frame(mpgdat)
colnames(M) <- c("y", paste0("x",1:7))
M[,3:7] <- apply(M[,3:7],2,scale)
M$x1 <- cut(M$x1,breaks = c(0,4.5, 6.5, 8.5))
M$x7 <- as.factor(M$x7)
M0 <- M
qns <- c(0.35, 0.5, 0.65)
k <- length(qns)
mxm <- 10
reps <- 10
plotit <- FALSE
res <- list()
r2s <- rmse <- mae <- rep(100,reps)
j <- which(qns == 0.5)
ssize <- round(0.7*table(M0$x7))

for (repno in 1:reps) {
  set.seed(repno+350654)
  sset <- sort(strata(M0, colnames(M0)[8], size=ssize, method = "srswor")$ID_unit)
  M <- M0[sset,]
  res[[repno]] <- fitQRloop(M=M, qn = qns, maxdeg = 6, minDiff = minDiff, maxrows = maxrows)
  trueY <- M0$y[-sset]
  predY <- predict(res[[repno]]$qremFit[[j]]$fitted.mod, newdata=M0[-sset,])
  plot(trueY, predY,pch=19,cex=0.7, col=4, main=repno, xlim=c(0,50),
       ylim=c(0,50), axes=F, xlab="Actual MPG", ylab="Predicted MPG")
  abline(0,1,col=2,lwd=4); axis(1); axis(2); grid()
  summlm <- summary(lm(predY ~ trueY))
  print(summlm)
  r2s[repno] <- summlm$r.squared
  rmse[repno] <- sqrt(mean(summlm$residuals^2))
  mae[repno] <- mean(abs(trueY - predY))
}

plot(mae, ylim=c(0,3))
minmae <- which.min(mae)
cat(minmae,mae[minmae],"\n")
print(res[[minmae]]$frmlterms)

trueY <- M0$y[-sset]
predY <- predict(res[[minmae]]$qremFit[[j]]$fitted.mod, newdata=M0[-sset,])
plot(trueY, predY,pch=19,cex=0.7, col=4, main="", xlim=c(0,50),
     ylim=c(0,50), axes=F, xlab="Actual MPG", ylab="Predicted MPG")
abline(0,1,col=2,lwd=4); axis(1); axis(2); grid()


#Looks like year is highly non-linear, but it could be because it is confounded with origin
plot(M0$x3[-sset], M0$y[-sset], cex=0.6, pch=19)
points(M0$x3[-sset],predict(res[[minmae]]$qremFit[[2]]$fitted.mod,newdata=M0[-sset,]), cex=0.7,col=2, pch=17)

print(table(mpgdat$`Model Year`,mpgdat$Origin))
print(table(mpgdat$`Model Year`,mpgdat$Cylinders))
