library(mlbench)
library(kernlab)
library(dplyr)
library(Metrics)

source("gen_functions.R")

##GERAÇÃO DAS SÉRIES
n = 500
N = 100
split = .8

sim1 <- gen_struct1(n = n, N = N, split = split)
sim2 <- gen_struct2(n = n, N = N, split = split)
saveRDS(sim1, paste0("data/struct1_", n))
saveRDS(sim2, paste0("data/struct2_", n))

##GERAÇÃO DAS SÉRIES
n = 100
sim1 <- gen_struct1(n = n, N = N, split = split)
sim2 <- gen_struct2(n = n, N = N, split = split)
saveRDS(sim1, paste0("data/struct1_", n))
saveRDS(sim2, paste0("data/struct2_", n))