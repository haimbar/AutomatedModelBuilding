source("initSim.R")
set.seed(211013)
# Classification using quantile regression
N <- 5000
maxdeg <- 10
plotit <- F

M <- matrix(runif(N*2, min = -1, max=1), nrow=N, ncol=2)
y <- sign(M[,1]*M[,2])
N <- length(y)
Ds <- as.matrix(dist(M))
diag(Ds) <- 2*max(Ds)
yp <- which(y>0)
y1p <- apply(apply(Ds[,-yp], 1, sort, decreasing=FALSE)[1:30,], 2, min)
y2p <- apply(apply(Ds[,yp], 1, sort, decreasing=FALSE)[1:30,], 2, min)
y1 <- log10(y1p/y2p)
hist(y1,breaks=50)
M <- as.data.frame(cbind(y1, M))
trset <- 1:(N/2)
tsset <- setdiff(1:N, trset)
M2 <- as.data.frame(M[tsset,])
y2 <- y[tsset]
M <- M[trset,]
y <- y[trset]
D <- 2
colnames(M) <- c("y", paste0("x", 1:D))
plot(M[,2],M[,3],col=3+sign(M[,1]),cex=0.3,xlab="x1",ylab="x2")
abline(h=0);abline(v=0)

res <- fitQRloop(M=M, qn = qns, maxdeg = maxdeg, minDiff = minDiff, maxrows = maxrows)

i <- 3
colnames(M2) <-c("y","x1","x2")
#pdf("../Figures/xorFit.pdf", width = 5, height = 5)
plot(M2[,2],M2[,3], cex=0.3,pch=19, axes=F, xlab="x1", ylab="x2",
     col=3+sign(predict(res$qremFit[[i]]$fitted.mod, newdata = M2)))
axis(1); axis(2)
abline(v=0, col="grey66", lwd=3)
abline(h=0, col="grey66", lwd=3)
miscls <- which(sign(predict(res$qremFit[[i]]$fitted.mod, newdata = M2)) != sign(y2))
cat(length(miscls)/nrow(M2),"\n")
points(M2[miscls,2], M2[miscls,3],pch=19,cex=0.8, col="black")
#dev.off()


# compare with SVM
library("e1071")
mod <- svm(M, y, kernel="radial", type="C-classification", cross=10)
cat(mean(mod$accuracies),"\n")
pred <- fitted(mod)
table(pred, y)
pred2 <- predict(mod, newdata=M2, decision.values = T)
ressvm <- table(pred2, y2)
cat(1-sum(diag(ressvm))/sum(ressvm), "\n")

misclssvm <- which(pred2 != as.factor(sign(y2)))
plot(M2[,2],M2[,3], cex=0.3,pch=19, axes=F, xlab="x1", ylab="x2",
     col=3+sign(predict(res$qremFit[[i]]$fitted.mod, newdata = M2)))
abline(v=0, col="grey66", lwd=3)
abline(h=0, col="grey66", lwd=3)
miscls <- which(sign(predict(res$qremFit[[i]]$fitted.mod, newdata = M2)) != sign(y2))
points(M2[misclssvm,2], M2[misclssvm,3],pch=19,cex=0.9, col="black")
