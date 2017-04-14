rm(list = ls())
library(rstan)
library(dplyr)

setwd("D:/Dropbox/Dokumenty/Projekty/IOWA GAMBLING TASK/NfCC-IGT")

## Identify "bad" subjects from previous run
load("samples/IND_samples_1st_run.Rdata")

samplesSum <- summary(samples)$summary

nSamples <- nrow(extract(samples)[[1]])  # pocet vzorku

SDs <- samplesSum[, "sd"]
SEs <- samplesSum[, "se_mean"]
rHats <- samplesSum[, "Rhat"]
nEffs <- samplesSum[, "n_eff"]


test1 <- which((nEffs / nSamples) < 0.1)
test2 <- which((SEs / SDs) > 0.1)
test3 <- which(rHats > 1.1)

badParameters <- unique(names(c(test1, test2, test3)))

# Extract subject numbers
tmp1 <- stringr::str_match(badParameters, "\\w+\\[(\\d+)\\]")[,2]
tmp2 <- as.numeric(unique(tmp1))

badSubjs <- sort(tmp2[-length(tmp2)])

## Read data
iow5 <- read.csv2("data/IOWA2015trials.csv")
iow6 <- read.csv2("data/IOWA2016trials.csv")

# Merge and prepare data
data <- tbl_df(bind_rows(iow5, iow6) %>%
  select(ID, trial, deck, difference) %>%
  mutate(ID2 = rep(1:length(unique(ID)), each = max(trial)),
         deck = as.numeric(deck),
         difference = difference / 1000) %>%
  arrange(ID, trial))

# Subset bad subjects
subData <- data %>%
  filter(ID2 %in% badSubjs)

nSubjs <- length(unique(subData$ID))
nTrials <- max(subData$trial)

choice <- matrix(subData$deck, nSubjs, nTrials, byrow = TRUE)  # Deck selections
net <- matrix(subData$difference, nSubjs, nTrials, byrow = TRUE)  # Net outcomes

# Input data for the model
mydata <- list(choice = choice, n_s = nSubjs, n_t = nTrials, net = net)

# Model set-up
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores() - 2) ## Hynek: minus 2 instead minus 1

# Parameters to be monitored
mypars <- c("A_ind", "w_ind", "a_ind", "c_ind")

#### Model run ####

# Compile the model (if not already complied)
stan_mod <- stan_model('models/pvl_d_IND_1g.stan')

start = Sys.time()
# Run iterations; output data is saved in object "samples"
samples <- sampling(stan_mod, data = mydata, init = "random", pars = mypars,
                warmup = 20000, iter = 40000, thin = 4, chains = 10)

end = Sys.time()
end - start   # run time

print(samples, digits=3)
# windows(20,1); traceplot(samples, ask=TRUE)
save(samples, file="Samples/IND_samples_2nd_run-badSubjs.Rdata")
