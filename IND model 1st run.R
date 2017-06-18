rm(list = ls())
library(rstan)
library(dplyr)


## Read  data
setwd("D:/Dropbox/Dokumenty/Projekty/IOWA GAMBLING TASK/NfCC-IGT")
# setwd("c:/Users/Hynek/Dropbox/MU/NFC/Analýzy/IOWA kognitivní modely")

iow5 <- read.csv2("data/IOWA2015trials.csv")
iow6 <- read.csv2("data/IOWA2016trials.csv")

# Merge and prepare data
data <- bind_rows(iow5, iow6)
data <- tbl_df(data)

data <- data %>%
  select(ID, trial, deck, difference) %>%
  # filter(ID == 222) %>%
  mutate(deck = as.numeric(deck),
         difference = difference / 1000) %>%
  arrange(ID, trial)

nSubjs <- length(unique(data$ID))
nTrials <- max(data$trial)

choice <- matrix(data$deck, nSubjs, nTrials, byrow = TRUE)  # Deck selections
net <- matrix(data$difference, nSubjs, nTrials, byrow = TRUE)  # Net outcomes


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

# Run iterations; output data is saved in object "samples"
samples <- sampling(stan_mod, data = mydata, init = "random", pars = mypars,
                warmup = 2000, iter = 12000, thin = 2, chains = 10)

print(samples, digits=3)
# windows(20,1); traceplot(samples, ask=TRUE)
save(samples, file="samples/IND_1st_run.Rdata")
