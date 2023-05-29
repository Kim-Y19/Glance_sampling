rm(list = ls())
library("tictoc")
# load("Data/glance_dec_data_test.R")
load("../OSD/Data/VirtSim.R")
# load("Data/test_prediction_100.RData")
source("RScript/active_sampling.R")
set.seed(123)
par(mfrow = c(1, 3))
cat("\14")

library(tidyverse)

df <- df %>% 
  mutate(caseID = as.character(caseID))
data <- df
sampling_method <- "importance sampling"
proposal_dist <- "density sampling"
target <- "NA"
use_logic <- TRUE
n_per_case <- 1
niter <- 20
verbose <- TRUE
plot <- FALSE
i <- 1
nboot <- 200
df$eoff_acc_prob <- 1

for (i in 1:1) {
  tic()
  print(sprintf("%d", i))
  res <- active_sampling(df, 
                         sampling_method = "active", 
                         proposal_dist = "NA",
                         target = "crash", 
                         use_logic = TRUE,
                         n_per_case = 1, 
                         niter = 10, 
                         verbose = TRUE, 
                         plot = FALSE)
  toc()
}