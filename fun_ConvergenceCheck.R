ConvergenceCheck <- function(samples, nEffCut, seSdCut, rHatCut) {
  library(rstan)
  
  samplesSum <- rstan::summary(samples)$summary
  
  nSamples <- nrow(rstan::extract(samples)[[1]])  # pocet vzorku
  
  SDs <- samplesSum[, "sd"]
  SEs <- samplesSum[, "se_mean"]
  rHats <- samplesSum[, "Rhat"]
  nEffs <- samplesSum[, "n_eff"]
  
  test1 <- which((nEffs / nSamples) < nEffCut)
  test2 <- which((SEs / SDs) > seSdCut)
  test3 <- which(rHats > rHatCut)
  
  badParameters <- unique(names(c(test1, test2, test3)))
  
  # Extract subject numbers
  tmp1 <- stringr::str_match(badParameters, "\\w+\\[(\\d+)\\]")[,2]
  tmp2 <- as.numeric(unique(tmp1))
  
  badSubjs <- sort(tmp2[-length(tmp2)])
  
  return(list(badParameters = badParameters, 
              badSubjs = badSubjs))
}