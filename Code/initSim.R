rm(list = ls())
source("Code/runQREM.R")
qns <- 1:5/6
k <- length(qns)
minDiff <- 4
maxdeg <- 15
maxrows <- 5000
mxm <- 30
alphaQ <- 0.01
plotit <- FALSE