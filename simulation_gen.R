library(mlbench)
library(kernlab)
library(dplyr)
library(Metrics)

source("gen_functions.R")

##GERAÇÃO DAS SÉRIES
N = 50
split = .8
####################

n = 300
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

##GERAÇÃO DAS SÉRIES
n = 50
sim1 <- gen_struct1(n = n, N = N, split = split)
sim2 <- gen_struct2(n = n, N = N, split = split)
saveRDS(sim1, paste0("data/struct1_", n))
saveRDS(sim2, paste0("data/struct2_", n))
