# Script of hierarchical PVL-Delta model implemented in Stan 2.5
# Date:
# Author: Martin Smira
# How to use this script...

################################################################################
rm(list = ls())
library(rstan) 

#### Chooose : ####
# TRUE if you want to exclude trials with depleted decks 
limitTrials <- FALSE  
# SPECIFY prior mixing proportion (higher value favours different group dist.)
mix <- c(.5, .5, .5, .5) 
###################

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores() - 1) 

# Load data 
data = read.csv2('Iowa_smira.csv')
data$Deck <- as.numeric(data$Deck)

# Using only cca one third of oh the all subjects - most extreme cases 
data$NfCC_cut = ifelse(data$NfCC <= 34, 1, ifelse(data$NfCC < 57, 2, 3))   # Only 16+16 most extreme subjs (w eff.)
# data$NfCC_cut = ifelse(data$NfCC <= 37, 1, ifelse(data$NfCC < 51, 2, 3))   %  30+32 extreme subjs (no eff.)

data$Net <- (data$Win - data$Lost) / 1000


controlData <- subset(data, NfCC_cut == 1)
patientData <- subset(data, NfCC_cut == 3)


#### Data preparation - first for control group, then Parkinson's group ####
n_sC <- length(unique(controlData$ID))  # Number of subjects in Group 1
n_sP <- length(unique(patientData$ID))  # Number of subjects in Group 2
n_s <- n_sC + n_sP  # Total number of subjects
n_t <- max(data$Trial)  # Number of trials

choiceC <- matrix(controlData$Deck, n_sC, n_t, byrow = TRUE)  # Deck selections
netC <- matrix(controlData$Net, n_sC, n_t, byrow = TRUE)  # Net outcomes
# netC <- netC / 1000  # Scale the net gains

choiceP <- matrix(patientData$Deck, n_sP, n_t, byrow = TRUE)
netP <- matrix(patientData$Net, n_sP, n_t, byrow = TRUE)
# netP <- netP / 1000

## Merge data from both group 
net <- rbind(netC, netP)
choice <- rbind(choiceC, choiceP)

# Identify trials, where the subject made the actuall deck choice, as opposed to 
# trials where the compture made a random choice for him
human_choice <- matrix(1, n_s, n_t)
# human_choice <- read.csv('Data/humanMadeChoices.csv')

# In case of limited number of cards in a deck, specify the number 
# Otherwise, set to 999
n_cards <- 999

# Input model data
mydata <- list(choice = choice, n_s = n_s, n_s_1 = n_sC, n_s_2 = n_sP,
               n_t = n_t, net = net, mix = mix, human_choice = human_choice,
               n_cards = n_cards)  

# Parameters to be monitored
mypars <- c("z", "mu_A", "mu_w", "mu_a", "mu_c", "std")

#### Model run ####

# Compile the model
samples <- stan(file = "pvl_d_mix_all_1-std.stan",
                data = mydata, iter = 1, chains = 1)

start = Sys.time()
# Run iterations; output data is saved in object "sampels"
samples <- stan(fit = samples, data = mydata, init = "random", pars = mypars, 
                warmup = 500, iter = 5500, thin = 1, chains = 6)
end = Sys.time()                  
end - start        
print(samples, digits=3)      
# windows(20,1); traceplot(samples, ask=TRUE)
# save(samples, file="Samples/4-z_samples_1st_run.Rdata")

############# Testing Z proportions ##################################
z1 <- extract(samples)$z[, 1]
z2 <- extract(samples)$z[, 2]
z3 <- extract(samples)$z[, 3]
z4 <- extract(samples)$z[, 4]

z <- paste(z1, z2, z3, z4, sep="")

table(z)

# Testing if the results match the "one z" solution
# proportion of z[1]
sum(z=="1000")/(sum(z=="1000")+sum(z=="0000"))
# proportion of z[2] 
sum(z=="0100")/(sum(z=="0100")+sum(z=="0000"))
# proportion of z[3] 
sum(z=="0010")/(sum(z=="0010")+sum(z=="0000"))
# proportion of z[4] 
sum(z=="0001")/(sum(z=="0001")+sum(z=="0000"))

# BF for parameter A
sum(z=="1000")/sum(z=="0000")
# BF for parameter w
sum(z=="0100")/sum(z=="0000")
# BF for parameter a
sum(z=="0010")/sum(z=="0000")
# BF for parameter c
sum(z=="0001")/sum(z=="0000")


# Alternative BF computing counts of all cases where z[2] was 1
#  compated to cases where z[2] was 0
zz2 = substr(z, 2, 2)
table(zz2)
