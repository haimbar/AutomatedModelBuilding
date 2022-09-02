# https://archive.ics.uci.edu/ml/datasets/HTRU2
source("initSim.R")
set.seed(211013)
maxdeg <- 4
maxrows <- 10000
minDiff <- 2

dat <- read.csv("../Data/HTRU_2.csv", header=F)
M <- apply(dat[,1:8], 2, scale)
y = (dat$V9 - 0.5)*2
qns <- round(sort(c(table(y)/sum(table(y)), qns)), digits=3)
k <- length(qns)
sset <- sort(sample(length(y), 3000))
Mtmp <- M[sset,]
ytmp <- y[sset]
Ds <- as.matrix(dist(Mtmp))
diag(Ds) <- 2*max(Ds)
yp <- which(ytmp > 0)
nbrs <- 1:30
y1p <- apply(apply(Ds[,-yp], 1, sort, decreasing=FALSE)[nbrs,], 2, mean)
y2p <- apply(apply(Ds[,yp], 1, sort, decreasing=FALSE)[nbrs,], 2, mean)
y1 <- log10(y1p/y2p)
plot(y1, col=2+(ytmp>0), cex=0.3, pch=19); abline(h=0, col=3, lwd=2)
sset <- setdiff(sset, sset[which(abs(y1) < 0.05)])
M1 <- as.data.frame(cbind(y[sset], M[sset,]))
M2 <- as.data.frame(cbind(y[-sset], M[-sset,]))
d <- ncol(M1)-1
colnames(M1) <- colnames(M2) <- c("y", paste0("x", 1:d))
res <- fitQRloop(M=M1, qn = qns, maxdeg = maxdeg, minDiff = minDiff, maxrows = maxrows)

for (i in 1:k) {
  predy <- sign(predict(res$qremFit[[i]]$fitted.mod, newdata = M2))
  realy <- sign(M2$y)
  miscls <- which(predy != realy)
  cat(qns[i],length(miscls)/nrow(M2),"\n")
  plot(predict(res$qremFit[[i]]$fitted.mod, newdata = M2), 
       y[-sset],cex=0.6,pch=19, main=qns[i])
}
