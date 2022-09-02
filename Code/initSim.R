rm(list = ls())
source("runQREM.R")
qns <- 1:5/6
k <- length(qns)
alphaQ <- 0.01
maxdeg <- 15
mxm <- 30
plotit <- TRUE
minDiff <- 4
maxrows <- 5000