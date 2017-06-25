rm(list = ls())
library(rstan)
library(dplyr)

setwd("D:/Dropbox/Dokumenty/Projekty/IOWA GAMBLING TASK/NfCC-IGT")

## Identify "bad" subjects from previous run
load("samples/IND_samples_1st_run.Rdata")

source("fun_ConvergenceCheck.R")
badSubjs <- ConvergenceCheck(samples, .1, .1, 1.1)$badSubjs

## Read data
iow5 <- read_csv2("data/IOWA2015trials.csv")
iow6 <- read_csv2("data/IOWA2016trials.csv")
tbl_id_mapping <- read_csv("data/id_mapping.csv")

# Merge and prepare data
data <- bind_rows(iow5, iow6) %>%
  left_join(tbl_id_mapping, by = c("ID" = "ID_original")) %>% 
  select(ID_new, trial, deck, difference) %>%
  arrange(ID_new, trial) %>%
  mutate(deck = match(deck, LETTERS),
         difference = difference / 1000) 

# Subset bad subjects
subData <- data %>%
  filter(ID_new %in% badSubjs)

nSubjs <- length(unique(subData$ID_new))
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

# Run iterations; output data is saved in object "samples"
samples <- sampling(stan_mod, data = mydata, init = "random", pars = mypars,
                warmup = 20000, iter = 40000, thin = 4, chains = 10)

print(samples, digits=3)
# windows(20,1); traceplot(samples, ask=TRUE)
save(samples, file="Samples/IND_samples_2nd_run-badSubjs.Rdata")
