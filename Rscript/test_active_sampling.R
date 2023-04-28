rm(list = ls())
library("tictoc")
load("Data/glance_dec_data_test.R")
# load("Data/test_prediction_100.RData")
source("RScript/active_sampling.R")
set.seed(123)
par(mfrow = c(1, 3))
cat("\14")

df %<>% 
  mutate(caseID = as.character(caseID))
# data <- df
# sampling_method <- "active sampling"
# proposal_dist <- "NA"
# target <- "impact speed reduction"
# use_logic <- TRUE
# n_per_case <- 1
# niter <- 20
# verbose <- TRUE
# plot <- FALSE
# i <- 1
# nboot <- 200
# df$eoff_acc_prob <- 1

for (i in 1:1) {
  tic()
  print(sprintf("%d", i))
  res <- active_sampling(df, 
                         sampling_method = "active", 
                         proposal_dist = "NA",
                         target = "impact speed", 
                         use_logic = TRUE,
                         n_per_case = 1, 
                         niter = 15, 
                         verbose = TRUE, 
                         plot = FALSE)
  toc()
}