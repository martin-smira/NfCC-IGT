library(shinystan)
library(rstan)
# library(bayesplot)


setwd("D:/Dropbox/Dokumenty/Projekty/IOWA GAMBLING TASK/NfCC-IGT")

load("samples/IND_samples_1st_run.Rdata")

print(samples)

# Running time for each chain (hours)
print(get_elapsed_time(samples) / 3600)

## ShinyStan
shinyStanSamples <- as.shinystan(samples)
launch_shinystan(shinyStanSamples)


samplesSum <- summary(samples)$summary

# Some options and constants
RhatCut <- 1.02  # strict
nSubj <- 164
nSamples <- 50000
nReliSamples <- 10000

## Rhat pass
rHatPassed <- logical(nSubj)
for (i in 1:nSubj) {
  parNames <- paste0(c("A", "w", "a", "c"), "_ind[", i, "]")
  
  rHats <- samplesSum[parNames, "Rhat"]
  
  rHatPassed[i] <- all(rHats < RhatCut)
}

rHatNOTPassedNum <- which(!rHatPassed)
rHatPassedNum <- which(rHatPassed)


## Extract samples and reliability computation
samplesX <- extract(samples)

parameterChoice <- "c_ind"

reli <- numeric(nReliSamples)
for (i in 1:nReliSamples) {
  randomTwoSamples <- sample(1:nSamples, 2)
  
  par1 <- samplesX[[parameterChoice]][randomTwoSamples[1], rHatPassedNum]
  par2 <- samplesX[[parameterChoice]][randomTwoSamples[2], rHatPassedNum]
  
  reli[i] = cor.test(par1, par2)$estimate  
}

summary(reli)
hist(reli)
