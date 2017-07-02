parameterChoice <- parametry[1]

reli <- NULL
reli1 <- numeric(nReliSamples)
for (j in c(1:4)) {
  parameterChoice <- parametry[j]
  for (i in 1:nReliSamples) {
    randomTwoSamples <- sample(1:nSamples, 2)
    
    par1 <- mergedSamples[[parameterChoice]][randomTwoSamples[1], ]
    par2 <- mergedSamples[[parameterChoice]][randomTwoSamples[2], ]
    
    reli1[i] <- cor(par1, par2)
  }
  reli <- cbind(reli, reli1)
}
colnames(reli) <- parametry
reli <- as.data.frame(reli)

describe(reli, quant = c(.025, .975))
hist(reli$A_ind)

plot(density(reli[,1]), xlim=c(0,1), ylim=c(0,20), main="", xlab="Reliability", col=rainbow(4)[1], lwd=2)
lines(density(reli[,2]), col=rainbow(4)[2], lwd=2)
lines(density(reli[,3]), col=rainbow(4)[3], lwd=2)
lines(density(reli[,4]), col=rainbow(4)[4], lwd=2)
legend("topleft", inset=.02, parametry, col=rainbow(4), lwd=2)

ks.test(reli[,2], pnorm, exact = )


# ** Variance method -------------------------------------------------------------

relvar <- vector()
for (j in 1:4) {
  parameterChoice <- parametry[j]
  x <- nSubj
  x2 <- vector()
  for (i in 1:nSubj) {
    x <- var(mergedSamples[[parameterChoice]][, i])
    x2 <- c(x2, x)
  }
  relvar[j] <- 1 - mean(x2)/var(colMeans(mergedSamples[[parameterChoice]]))
}

names(relvar) <- parametry
relvar