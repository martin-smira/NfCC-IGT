
library(rstan)

source("fun_ConvergenceCheck.R")

setwd("D:/Dropbox/Dokumenty/Projekty/IOWA GAMBLING TASK/NfCC-IGT")


load("samples/IND_samples_1st_run.Rdata")
samples1 <- samples
load("samples/IND_samples_2nd_run-badSubjs.Rdata")
samples2 <- samples

# print(samples1, digits_summary = 3)
# print(samples2, digits_summary = 3)

badSubjs1 <- ConvergenceCheck(samples1, .1, .1, 1.1)$badSubjs


x1 <- paste0("A", "_ind[", badSubjs1, "]")
x2 <- paste0("w", "_ind[", badSubjs1, "]")
x3 <- paste0("a", "_ind[", badSubjs1, "]")
x4 <- paste0("c", "_ind[", badSubjs1, "]")
x <- c(x1, x2, x3, x4)

s1 <- round(summary(samples1)$summary[x, ], 3)
s2 <- round(summary(samples2)$summary, 3)[-93,]

# s1
# s2
# s1 - s2

# Merge samples from the two runs
eSamples1 <- rstan::extract(samples1)
eSamples2 <- rstan::extract(samples2)

nSubj1 <- ncol(eSamples1[[1]])
nSubj2 <- ncol(eSamples2[[1]])

badSubjs2 <- ConvergenceCheck(samples2, .1, .1, 1.1)$badSubjs
fixedSubjs2 <- c(1:nSubj2)[-badSubjs2]

badSubjs2orig <- badSubjs1[badSubjs2]
fixedSubjs2orig <- badSubjs1[-badSubjs2]

subjIdxs <- c(c(1:nSubj1)[-badSubjs1], fixedSubjs2orig)
subjIdxsOrder <- order(subjIdxs)
subjIdxsOrdered <- subjIdxs[subjIdxsOrder]

par_A_mat <- cbind(eSamples1$A_ind[, -badSubjs1], eSamples2$A_ind[, fixedSubjs2])
par_w_mat <- cbind(eSamples1$w_ind[, -badSubjs1], eSamples2$w_ind[, fixedSubjs2])
par_a_mat <- cbind(eSamples1$a_ind[, -badSubjs1], eSamples2$a_ind[, fixedSubjs2])
par_c_mat <- cbind(eSamples1$c_ind[, -badSubjs1], eSamples2$c_ind[, fixedSubjs2])

mergedSamples <- list(A_ind = par_A_mat[, subjIdxsOrder],
                      w_ind = par_w_mat[, subjIdxsOrder],
                      a_ind = par_a_mat[, subjIdxsOrder],
                      c_ind = par_c_mat[, subjIdxsOrder])

# save(mergedSamples, file = "samples/mergedSamples.Rdata")

iow5 <- read_csv2("data/IOWA2015trials.csv")
iow6 <- read_csv2("data/IOWA2016trials.csv")
tbl_id_mapping <- read_csv("data/id_mapping.csv")

final_data <- data_frame(ID_new = as.integer(subjIdxsOrdered), 
           mean_A = apply(mergedSamples$A_ind, 2, mean),
           mean_w = apply(mergedSamples$w_ind, 2, mean),
           mean_a = apply(mergedSamples$a_ind, 2, mean),
           mean_c = apply(mergedSamples$c_ind, 2, mean)) %>% 
  left_join(tbl_id_mapping, by = "ID_new") %>% 
  select(ID_original, ID_new:mean_c) %>% 
  left_join(bind_rows(iow5, iow6) %>% 
              group_by(ID) %>% 
              summarise(nfcc = unique(nfcc),
                        finalScore = sum(difference)),
                        meanRT = mean(RT),
                        meanRT2 = mean(RT2),
            by = c("ID_original" = "ID"))
  

write_csv(final_data, "data/df_for_anal.csv")


