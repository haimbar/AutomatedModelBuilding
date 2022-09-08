source("Code/initSim.R")
load("Data/uscrime.rda")
qns <- 1:19/20
k <- length(qns)
mxm <- 30
minDiff <- 0
maxrows <- 2000

pops <- uscrime$Total[which(uscrime$Data == "Population")]
states <- factor(uscrime$State[which(uscrime$Data == "Population")])
popstate <- data.frame(states, pops)
print(sort(by(popstate[,2], popstate[,1], min)))

keepstates <- names(which(by(popstate[,2], popstate[,1], min) > 1500000))

byPopData <- uscrime[which(uscrime$Data == "Population"),]
byPopData$State <- factor(byPopData$State)
plot(log10(byPopData$Total) ~ byPopData$State)

byRateData <- uscrime[which(uscrime$Data == "Rate"),-2]
byRateData$State <- factor(byRateData$State)
byRateData <- byRateData[which(byRateData$State %in% keepstates),]

lm1 <- lm(log(Rape)  ~ 0+Year+State, data=byRateData)
anova(lm1)
plot(byRateData$Year, byRateData$Rape)

lm2 <- lm(log(Rape)  ~ 0+Year+State + (Year<2013), data=byRateData)
anova(lm2)
plot(byRateData$Year, byRateData$Rape)

lm3 <- lm(log(Rape)  ~ 0+Year*(Year<2013), data=byRateData)
anova(lm3)
summary(lm3)
plot(lm3$fitted.values, log(byRateData$Rape))


M <- data.frame(log(byRateData$Rape), byRateData$Year, factor(byRateData$Year<2013))
M <- data.frame(byRateData$Rape, byRateData$Year, factor(byRateData$Year<2013))
colnames(M) <- c("y", "x1", "x2")
M[,2] <- M[,2]-1995

res <- fitQRloop(M=M, qn = qns, maxdeg = maxdeg, minDiff = minDiff)
plot(M$x1,M$y, cex=0.5, pch=19, col="orange", xlab="Year", 
     ylab="Rape (rate)", axes=F)
axis(1); axis(2)
for (i in 1:k) {
  lines(sort(M$x1), res$qremFit[[i]]$fitted.mod$fitted.values[order(M$x1)],
        col=4)
}


#lm4 <- lmer(log(Rape)  ~ 0+Year+ (Year<2013) + (1|State), byRateData)
#anova(lm4)
#plot(fitted(lm4))
