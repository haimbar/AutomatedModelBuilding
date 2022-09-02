source("initSim.R")

# compare with https://github.com/fnauman/timeseries/blob/master/dynamicalsystems/sindy_cubicmodel.ipynb

qns <- c(0.4,0.45,0.5,0.55,0.6)
plotit <- FALSE
mxdg <- 9
dt <- 0.01
t0 <- -dt
tms <- seq(0, 60, by=dt)
Nt <- length(tms)
x <- y <- z <- rep(0.9, Nt)
trim <- 0
bw <- 0.5
alphaQ <- 0.01
sig <- 0# 0.01 # set to 0 for no error
simtype <- "Lotka-Volterra"
#simtype <- "vanderpol"
#simtype <- "SINDy" # https://fnauman.github.io/ds-blog/differential%20equations/machine%20learning/2019/09/10/sindy-basic.html#Example
#simtype <- "Lorenz" #https://www.johndcook.com/blog/2020/01/26/lorenz-system/
#simtype <- "SCs"

for (i in 2:Nt){
  if(simtype == "Lotka-Volterra") {
    prms <- c(2/3, 4/3, 1, 1)
    x[i] <- x[i-1] + dt*(prms[1]*x[i-1] - prms[2]*x[i-1]*y[i-1])
    y[i] <- y[i-1] + dt*(prms[4]*x[i-1]*y[i-1] - prms[3]*y[i-1])
  }
  if(simtype == "vanderpol") {
    mu <- 5 # or 0.5
    x[i] <- x[i-1] + dt*y[i-1]
    y[i] <- y[i-1] + dt*(-x[i-1] + mu*(1-x[i-1]^2)*y[i-1])
  }
  if(simtype == "SINDy") {
    x[i] <- x[i-1] + dt*(-x[i-1]^3 - y[i-1])
    y[i] <- y[i-1] + dt*(x[i-1] - y[i-1]^3)
  }
  if(simtype == "Lorenz") {
    x[i] <- x[i-1] + dt*10*(y[i-1] - x[i-1])
    y[i] <- y[i-1] + dt*(x[i-1]*(28 - z[i-1]) - y[i-1])
    z[i] <- z[i-1] + dt*(x[i-1]*y[i-1] - (8/3)*z[i-1])
  }
  if(simtype == "SCs") {
    x[i] <- x[i-1] + dt*(0.5*x[i-1] - 0.3*x[i-1]^2)
    y[i] <- y[i-1] + dt*(0.1*x[i-1] - 0.5)
  }
}
if (trim > 0) {
  x <- x[-(1:trim)]
  y <- y[-(1:trim)]
  z <- z[-(1:trim)]
  tms <- tms[-(1:trim)]
}
Nt <- length(tms)
plot(x,y, cex=0.1, xlim=c(min(x,y), max(x,y)), ylim=c(min(x,y), max(x,y)))
plot(tms,x,type='l', ylim=c(min(x,y), max(x,y)))
lines(tms,y, col=2)
xn <- x + rnorm(length(x), 0, sig)
yn <- y + rnorm(length(x), 0, sig)
#plot(xn, type='l', ylim=c(min(xn,yn), max(xn,yn)))
#lines(yn, col=2)

if(simtype == "Lorenz") {
  M <- data.frame(y=diff(y)/dt + rnorm(length(y)-1,0, sig),
                  x=x[-length(x)], x2=y[-length(y)],
                  x3=z[-length(z)])
  sset <- sort(sample(nrow(M), nrow(M)*0.5))
  sset <- 1:floor(nrow(M)/2)
  resY <- fitQRloop(M[sset,], qn = qns, maxdeg = mxdg, minDiff = minDiff, maxrows = maxrows)
  predY <- predict(resY$qremFit[[3]]$fitted.mod, newdata=M[-sset,])
  plot(M$y[-sset], predY, cex=0.4, col="grey", pch=19, 
       ylim=range(M$y))
  print(resY$qremFit[[3]]$coef)
  
  M <- data.frame(y=diff(x)/dt  + rnorm(length(y)-1,0, sig),
                  x=x[-length(x)], x2=y[-length(y)],
                  x3=z[-length(z)])
  resX <- fitQRloop(M[sset,], qn = qns, maxdeg = mxdg, minDiff = minDiff, maxrows = maxrows)
  predX <- predict(resX$qremFit[[3]]$fitted.mod, newdata=M[-sset,])
  plot(M$y[-sset], predX, cex=0.4, col="grey", pch=19, ylim=range(M$y))
  print(resX$qremFit[[3]]$coef)
  
  M <- data.frame(y=diff(z)/dt  + rnorm(length(y)-1,0, sig),
                  x=x[-length(x)], x2=y[-length(y)],
                  x3=z[-length(z)])
  resZ <- fitQRloop(M[sset,], qn = qns, maxdeg = mxdg, minDiff = minDiff, maxrows = maxrows)
  predZ <- predict(resZ$qremFit[[3]]$fitted.mod, newdata=M[-sset,])
  plot(M$y[-sset], predZ, cex=0.4, col="grey", pch=19, ylim=range(M$y))
  print(resZ$qremFit[[3]]$coef)
  
  plot(dt*predX[-length(predX)], 
       dt*predY[-length(predY)],
       cex=0.3)
  points(diff(x),diff(y),cex=0.1, col=3)
  
  plot(dt*predX, dt*predZ, cex=0.3)
  points(diff(x),diff(z),cex=0.1, col=3)
  
  plot(dt*predX, dt*predZ, cex=0.3)
  points(diff(x),diff(z),cex=0.1, col=3)
  
  plot(dt*predY[-length(predY)], 
       dt*predZ[-length(predZ)],
       cex=0.3)
  points(diff(y), diff(z),cex=0.1, col=3)
  
  predXall <- predict(resX$qremFit[[3]]$fitted.mod, newdata=M)
  predYall <- predict(resY$qremFit[[3]]$fitted.mod, newdata=M)
  predZall <- predict(resZ$qremFit[[3]]$fitted.mod, newdata=M)
  
  xpred <- rep(x[1], length(x)-1)
  xpred <- xpred + cumsum(dt*predXall)
  ypred <- rep(y[1], length(y)-1)
  ypred <- ypred + cumsum(dt*predYall)
  zpred <- rep(z[1], length(z)-1)
  zpred <- zpred + cumsum(dt*predZall)
  plot(xpred,  zpred,  type='l')
  points(diff(x), diff(z),cex=0.1, col=3)
  
} else {
  flt <- 3
  xClean <- stats::filter(stats::filter(xn, rep(1/flt, flt)), rep(1/flt, flt))
  yClean <- stats::filter(stats::filter(yn, rep(1/flt, flt)), rep(1/flt, flt))
#  xClean <- zoo::rollmean(zoo::rollmean(xn, k=5), k=5)
#  yClean <- zoo::rollmean(zoo::rollmean(yn, k=5), k=5)
  
  #ksmooth(tms,zoo::rollmedian(diff(yn),3), bandwidth = 1)$y
  plot(ksmooth(tms,zoo::rollmedian(diff(yn),3), bandwidth = bw)$y,
       type='l')
  lines(diff(y), type='l', col=2, lwd=3)
  xClean <- xn
  yClean <- yn
  
  dxClean <- ksmooth(tms,zoo::rollmedian(diff(xn),3), bandwidth = bw)$y
  dyClean <- ksmooth(tms,zoo::rollmedian(diff(yn),3), bandwidth = bw)$y
  #  xClean <- ksmooth(tms, xn, bandwidth = bw)$y
#  yClean <- ksmooth(tms, yn, bandwidth = bw)$y
  exc <- which(is.na(xClean))
  if (length(exc) > 0) {
    xClean <- xClean[-exc]
    yClean <- yClean[-exc]
    x <- x[-exc]
    y <- y[-exc]
  }
  M <- data.frame(y=diff(yClean)/dt,
                  x=xClean[-length(xClean)],
                  x2=yClean[-length(yClean)])
  # M <- data.frame(y=filter(diff(yn), rep(1/flt, flt))/dt,
  #                 x=xn[-length(x)], x2=yn[-length(y)])
  # M <- data.frame(y=diff(y)/dt  + rnorm(length(y)-1,0, sig),
  #                 x=x[-length(x)], x2=y[-length(y)])
  sset <- 1:floor(nrow(M)/2)
  resY <- fitQRloop(M[sset,], qn = qns, maxdeg = mxdg, minDiff = minDiff, maxrows = maxrows)
  predY <- predict(resY$qremFit[[3]]$fitted.mod, newdata=M[-sset,])
  plot(M$y[-sset], predY, cex=0.4, col="grey", pch=19, 
       ylim=range(M$y[-sset]))
  print(resY$qremFit[[3]]$coef)
  
  M <- data.frame(y=dxClean[-1]/dt,
                  x=xClean[-length(xClean)], 
                  x2=yClean[-length(yClean)])
  # M <- data.frame(y=diff(x)/dt  + rnorm(length(x)-1,0, sig),
  #                 x=x[-length(x)], x2=y[-length(y)])
  resX <- fitQRloop(M[sset,], qn = qns, maxdeg = mxdg, minDiff = minDiff, maxrows = maxrows)
  predX <- predict(resX$qremFit[[3]]$fitted.mod, newdata=M[-sset,])
  plot(M$y[-sset], predX, cex=0.4, col="grey", pch=19, 
       ylim=range(M$y[-sset]))
  print(resX$qremFit[[3]]$coef)
  
  #plot(predX, predY, cex=0.1)
  #points(x[-1][-exc], y[-1][-exc], cex=0.1, col=2)
  
  # plot(predX[-length(predX)] + predX[-1], type="l",
  #      ylim=range(c(predX + predX[-1], predY + predY[-1])))
  # lines(predY[-length(predY)] + predY[-1], col=2)
  
  # plot(dt*predX[-length(predX)], 
  #      dt*predY[-length(predY)],
  #      cex=0.3)
  # points(diff(x)[-exc],diff(y)[-exc],cex=0.1, col=3)
  
  predXall <- predict(resX$qremFit[[3]]$fitted.mod, newdata=M)
  predYall <- predict(resY$qremFit[[3]]$fitted.mod, newdata=M)
  xpred <- rep(x[1], length(predXall))
  xpred <- xpred + cumsum(dt*predXall)
  ypred <- rep(y[1], length(predYall))
  ypred <- ypred + cumsum(dt*predYall)
  plot(xpred,  ypred,  type='l', lwd=3)
  points(x[-1], y[-1], cex=0.1, col=3)
}

#import pandas as pd
#df = pd.read_csv("/Users/haim/Downloads/tvregdiff-master/y.csv")
#X=df["t"]
#dX=X[1]-X[0]
#Y=df["y"]
# dYdX = TVRegDiff(Y, itern=10, alph=5e-2, dx=dX)