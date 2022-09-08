source("Code/initSim.R")
dat <- read.table("Data/ozone.dat", header=T)
qns <- c(0.35, 0.5, 0.65)
k <- length(qns)
mxm <- 10
reps <- 1
takelog <- F
M <- dat[,-1]
colnames(M)[1] <- "y"
if(takelog)
  M$y <- log(1+M$y)
M0 <- M
set.seed(114322)
sset <- sort(sample(1:330, 66))
alphaQ <- 0.01
res <- fitQRloop(M=M[-sset,], qn = qns, maxdeg = 6, minDiff = minDiff, maxrows = maxrows)

print(summary(res$qremFit[[2]]$fitted.mod))
if(takelog){
trueY <- exp(M0$y[sset])-1
predY <- exp(predict(res$qremFit[[2]]$fitted.mod, newdata=M0[sset,]))-1
} else{
  trueY <- M0$y[sset]
  predY <- predict(res$qremFit[[2]]$fitted.mod, newdata=M0[sset,])
  
}
axlim <- max(predY, trueY)
plot(trueY, predY,pch=19,cex=0.7, col=4, main="Ozone", xlim=c(0, axlim),
     ylim=c(0, axlim), axes=F, xlab="Actual O3", ylab="Predicted O3")
abline(0,1,col=2,lwd=4); axis(1); axis(2); grid()
#summlm <- summary(lm(predY ~ trueY))
#print(summlm)
