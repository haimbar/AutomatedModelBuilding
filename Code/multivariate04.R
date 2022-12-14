source("Code/initSim.R")
set.seed(211013)
N <- 2000
M <- matrix(runif(N*4, min = -1, max=3), nrow=N, ncol=4)
y <- M[,1]*M[,2]*M[,4] + rnorm(N, 0, 0.1)
M <- as.data.frame(cbind(y, M))
colnames(M) <- c("y", "x1", "x2", "x3", "x4")
res <- fitQRloop(M=M, qn = qns, maxdeg = maxdeg, minDiff = minDiff,
                 maxrows = maxrows)
pdf("Figures/multivariate04.pdf", width=5, height=5)
i <- which(qns == 0.5)
plot(M$y, res$qremFit[[i]]$fitted.mod$fitted.values, cex=0.6, pch=19,
     xlab="Observed", ylab="predicted", axes=FALSE, col="grey66")
abline(0, 1, col=3, lwd=2); axis(1); axis(2)
dev.off()
slctd <- res$frmlterms
fittedModel <- paste(slctd, collapse=" + ")
cat("\nFitted model:", fittedModel)
#print(summary(lm(res$qremFit[[i]]$fitted.mod$fitted.values ~ M$y)))