library("QREM")

# the QR penalty function
rho <- function(u, q) {
  sum(u*(q-(u<0)))
}


# given variables and their degrees, return the polynomial for the regression
createTerm <- function(degs, varnames) {
  ret <- varnames
  for (i in 1:length(degs)) {
    if (degs[i] > 1)
      ret[i] <- sprintf("I(%s^%d)",varnames[i], degs[i])
  }
  idx <- which(degs > 0)
  paste0(ret[idx], collapse="*")
}

# precompute all possible terms up to degree maxDegree.
# stop with the degree which gives a number of terms which exceed maxRows. 
createAllTermsHash <- function(varnames, varclass, maxDegree, maxRows=2000) {
  nVar <- length(varnames)
  degrees <- diag(1, nVar, nVar)
  rownames(degrees) <- varnames
  rng <- 1:nVar
  ht <- new.env(hash=TRUE)
  if (maxDegree > 1) {
    for (deg in 1:(maxDegree-1)) {
      for (rnum in rng) {
        for (cnum in 1:nVar) {
          newTerm <- degrees[rnum,]
          if((varclass[cnum] == "factor") & (newTerm[cnum] > 0))
            next
          newTerm[cnum] <- newTerm[cnum]+1
          fterm <- createTerm(newTerm, varnames)
          if (fterm %in% names(ht))
            next
          ht[[fterm]] <- 1
          degrees <- rbind(degrees, newTerm, deparse.level = 0)
          rownames(degrees)[nrow(degrees)] <- fterm
        }
      }
      rng <- (1+max(rng)):nrow(degrees)
      if(nrow(degrees) > maxRows) {
        cat("Exceeded ", maxRows," possible terms. Maximum degree will be ", deg,".\n")
        break
      }
    }
  }
  degrees
}

updateSelection <- function(deg, inModel, goodFit) {
  for (i in which(inModel == 0)) {
    # if all the variables in the current term have a good fit, do not 
    # use augmented versions of it:
    if(all(goodFit[which(deg[i,] > 0)])) {
      inModel[i] <- -1
    }
  }
  for (i in which(!goodFit)) {
    rs <- intersect(which(deg[,i] > 0), which(inModel == -1))
    inModel[rs] <- 0 
  }
  return(inModel)
}


selectNext <- function(inModel, degrees, pvals, goodFit) {
  candidates <- which(inModel == 0)
  degs <- apply(degrees, 1, sum)
  minDeg <- min(degs[candidates]) # randomize if more than 1?
  candidates <- intersect(candidates, which(degs == minDeg))
  if (length(candidates) == 1)
    return(candidates[1])
  priority <- rep(0, length(candidates))
  for (i in 1:length(candidates)) {
    priority[i] <- max(-log10(1e-16 + pvals[,which(degrees[candidates[i],] > 0)]))
  }
  candidates[which.max(priority)]
}

selectNextNew <- function(inModel, degrees, pvals, goodFit) {
  Mtmp <- (inModel == 0) * (degrees) %*% (1-goodFit)
  rsum <- rowSums(degrees)
  Mtmp[which(rsum > min(rsum)),] <- 0
  Mtmp[Mtmp == 0] <- 2*max(Mtmp)+1
  return(which.min(apply(Mtmp, 1, sum)))
}


fitQRloop <- function(M, qns, minDiff=0, maxdeg=15, maxrows=1000) {
  varnames <- colnames(M)[-1] 
  d <- length(varnames)
  nq <- length(qns)
  chisqstat <- chisqpval <- chisqdf <- matrix(1000, ncol=d, nrow=nq)
  # the null model, the residuals ui, and the GoF statistic for the null model
  empQuantile <- quantile(M$y, qns)
  qremFit <- list()
  G0 <- rep(0, nq)
  G <- 100*G0
  for (i in 1:nq) {
    qremFit[[i]] <- list(ui=M$y-empQuantile[i])
    G0[i] <- 2*rho(qremFit[[i]]$ui, qns[i])
  }
  for (colnum in 1:d) {
    pvals <- flatQQplot(dat=M, cnum = colnum+1, qrfits=qremFit,
                        qns=qns, maxm = mxm, plot.it = plotit)
    chisqstat[,colnum] <- sapply(pvals, `[[`, 1)
    chisqdf[,colnum] <- sapply(pvals, `[[`, 2)-1
    chisqpval[,colnum] <- 1-pchisq(chisqstat[,colnum],
                                   chisqdf[,colnum])
  }
  goodFit <- as.logical(apply(chisqpval > alphaQ, 2, min))
  M <- M[, c(1, 1+which(goodFit == FALSE))]
  varnames <- colnames(M)[-1] 
  d <- length(varnames)
  degrees <- createAllTermsHash(varnames, unlist(lapply(M,class))[-1], maxdeg, maxRows = maxrows)
  inModel <- rep(0, nrow(degrees)) # 0=candidate, 1=lock-in, -1=lock-out
  goodFit <- rep(FALSE, d)
  chisqstat <- chisqpval <- chisqdf <- matrix(1000, ncol=d, nrow=nq)
  frmlterms <- c()
  while(!all(goodFit)) {
    inModel <- updateSelection(degrees, inModel, goodFit)
    if(all(inModel != 0)){
      cat("\nran out of terms\n")
      break
    }
    if(length(which(inModel == 0)) == 0){
      cat("ran out of terms\n")
      break
    }
    newProposal <- selectNext(inModel, degrees, chisqpval, goodFit)
    proposedTerm <- createTerm(degrees[newProposal,], varnames)
    cat("\n",proposedTerm,"\t")
    frmlterms <- union(proposedTerm, frmlterms)
    frml <- as.formula(paste("y ~ ", paste(frmlterms, collapse= "+")))
    for (i in 1:nq) {
      qremFit[[i]] <- QREM(lm, linmod=frml, df=M, qn=qns[i])
      G[i] <- 2*rho(qremFit[[i]]$ui, qns[i]) + 2*length(frmlterms) #### factors
    }
    if (all(G > G0 - minDiff)) { # model is no better with the new proposal in all quantiles
      inModel[newProposal] <- -2 # lock it out of future models
      frmlterms <- setdiff(frmlterms, proposedTerm)
      if (all(inModel != 0)) {
        cat("\nNo more possible terms with max. degree <= ", maxdeg,".\n")
        break
      }
      next
    } 
    # Otherwise, we got a better with with the most recent proposal
    inModel[newProposal] <- 1 # put in the model
    cat("accepted")
    G0 <- G
    # and compute the chi-squared GoF statistics
    for (colnum in 1:d) {
      pvals <- flatQQplot(dat=M, cnum = colnum+1, qrfits=qremFit, qns=qns, maxm = mxm, plot.it = plotit)
      chisqstat[,colnum] <- sapply(pvals, `[[`, 1)
      chisqdf[,colnum] <- sapply(pvals, `[[`, 2)-1
      chisqpval[,colnum] <- 1-pchisq(chisqstat[,colnum],
                                     chisqdf[,colnum])
    }
    goodFit <- as.logical(apply(chisqpval > alphaQ, 2, min))
  }
  frml <- as.formula(paste("y ~ ", paste(frmlterms, collapse= "+")))
  for (i in 1:nq) {
    qremFit[[i]] <- QREM(lm, linmod=frml, df=M, qn=qns[i])
  }
  list(frmlterms=frmlterms, qremFit=qremFit, goodFit=goodFit, chisqstat=chisqstat,
       chisqpval=chisqpval, chisqdf=chisqdf, inModel=inModel)
}
