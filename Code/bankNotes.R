# https://archive.ics.uci.edu/ml/datasets/banknote+authentication
source("initSim.R")
library("e1071")
set.seed(211013)
maxdeg <- 7
maxrows <- 10000
minDiff <- 2

dat <- read.csv("../Data/data_banknote_authentication.txt", header=F)
# predictors
M <- dat[,1:4]
# the response (converted to -1 and 1)
y = (dat$V5 - 0.5)*2
qns <- round(sort(c(table(y)/sum(table(y)), qns)), digits=3)
k <- length(qns)
sset <- sort(sample(length(y), 500))
Mtmp <- apply(M[sset,], 2, scale)
ytmp <- y[sset]
Ds <- as.matrix(dist(Mtmp))
diag(Ds) <- 2*max(Ds)
yp <- which(ytmp > 0)
y1p <- apply(apply(Ds[,-yp], 1, sort, decreasing=FALSE)[1:30,], 2, mean)
y2p <- apply(apply(Ds[,yp], 1, sort, decreasing=FALSE)[1:30,], 2, mean)
y1 <- log10(y1p/y2p)
plot(y1, col=1+(ytmp>0)); abline(h=0, col=3, lwd=2)

sset <- setdiff(sset, sset[which(abs(y1) < 0.05)])
M1 <- cbind(y[sset], M[sset,])
M2 <- cbind(y[-sset], M[-sset,])
d <- ncol(M1)-1
colnames(M1) <- colnames(M2) <- c("y", paste0("x", 1:d))
res <- fitQRloop(M=M1, qn = qns, maxdeg = maxdeg, minDiff = minDiff, maxrows = maxrows)

for (i in 1:k) {
  predy <- sign(predict(res$qremFit[[i]]$fitted.mod, newdata = M2))
  realy <- sign(M2$y)
  miscls <- which(predy != realy)
  cat(qns[i],length(miscls)/nrow(M2),"\n")
  plot(predict(res$qremFit[[i]]$fitted.mod, newdata = M2),
       M2$y,cex=0.6,pch=19, main=qns[i])
}

# SVM
mod1 <- svm(V5 ~ ., data=dat, type="C-classification", subset = sset, 
            kernel="radial", cross=10)
pred <- predict(mod1, dat[-sset,])
print(table(pred, dat[-sset,5]))
