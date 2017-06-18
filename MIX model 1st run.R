
rm(list = ls())
library(rstan) 
library(dplyr)

# SPECIFY prior mixing proportion (higher value favours different group dist.)
mix <- c(.5, .5, .5, .5) 

iow5 <- read.csv2("data/IOWA2015trials.csv")
iow6 <- read.csv2("data/IOWA2016trials.csv")

# Merge and prepare data
data <- bind_rows(iow5, iow6)
data <- tbl_df(data)

data <- data %>%
  select(ID, trial, deck, difference, nfcc) %>%
  filter(!is.na(nfcc)) %>%
  mutate(deck = as.numeric(deck),
         difference = difference / 1000) %>%
  # top_n(200 * 10, ID) %>% 
  arrange(ID, trial)

dataC <- data %>% 
  filter(nfcc == 0)

dataE <- data %>% 
  filter(nfcc == 1)

nSubjsC <- length(unique(dataC$ID))
nSubjsE <- length(unique(dataE$ID))
nSubjs <- nSubjsC + nSubjsE

nTrials <- max(data$trial)

choiceC <- matrix(dataC$deck, nSubjsC, nTrials, byrow = TRUE)  # Deck selections
choiceE <- matrix(dataE$deck, nSubjsE, nTrials, byrow = TRUE)  # Deck selections

netC <- matrix(dataC$difference, nSubjsC, nTrials, byrow = TRUE)  # Net outcomes
netE <- matrix(dataE$difference, nSubjsE, nTrials, byrow = TRUE)  # Net outcomes

## Merge data from both group 
net <- rbind(netC, netE)
choice <- rbind(choiceC, choiceE)

# Input data for the model
mydata <- list(choice = choice, n_s = nSubjs, n_s_1 = nSubjsC, n_s_2 = nSubjsE,
               n_t = nTrials, net = net, mix = mix)  

# Parameters to be monitored
mypars <- c("z", "mu_A", "mu_w", "mu_a", "mu_c", "std")

#### Model run ####
# Model set-up
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores() - 1)

# Compile the model
stan_mod <- stan_model('models/pvl_d_mix_all_1-std.stan')

# Run iterations; output data is saved in object "sampels"
samples <- sampling(stan_mod, data = mydata, init = "random", pars = mypars, 
                warmup = 200, iter = 1200, thin = 1, chains = 6)

print(samples, digits = 3)      
# windows(20,1); traceplot(samples, ask=TRUE)
# save(samples, file="Samples/4-z_samples_1st_run.Rdata")

############# Testing Z proportions ##################################
extractedSamples <- extract(samples)

z1 <- extractedSamples$z[, 1]
z2 <- extractedSamples$z[, 2]
z3 <- extractedSamples$z[, 3]
z4 <- extractedSamples$z[, 4]

z <- paste0(z1, z2, z3, z4)

table(z)

# Testing if the results match the "one z" solution
# proportion of z[1]
sum(z == "1000") / (sum(z == "1000") + sum(z == "0000"))
# proportion of z[2] 
sum(z == "0100") / (sum(z == "0100") + sum(z == "0000"))
# proportion of z[3] 
sum(z == "0010") / (sum(z == "0010") + sum(z == "0000"))
# proportion of z[4] 
sum(z == "0001") / (sum(z == "0001") + sum(z == "0000"))

# BF for parameter A
sum(z == "1000") / sum(z == "0000")
# BF for parameter w
sum(z == "0100") / sum(z == "0000")
# BF for parameter a
sum(z == "0010") / sum(z == "0000")
# BF for parameter c
sum(z == "0001") / sum(z == "0000")


# Alternative BF computing counts of all cases where z[2] was 1
#  compated to cases where z[2] was 0
zz2 = substr(z, 2, 2)
table(zz2)
