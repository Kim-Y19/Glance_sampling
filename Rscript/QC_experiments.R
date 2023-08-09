################################################################################
#
# QC_experiments.R
#
################################################################################

# Clean-up.
rm(list = ls())


# Load packages.
library("doParallel")
library("foreach")
library("magrittr")
library("parallel")
library("tictoc")
library("tidyverse")


# Load data.
load("Data/Testdata.R")


# Load functions.
source("Rscript/active_sampling.R")


# Experiment parameters.
params <- crossing(sampling_method = c("simple random sampling", 
                                       "importance sampling", 
                                       "active sampling"),
                   proposal_dist = c("density sampling", 
                                     "severity sampling"),
                   target = c("impact speed reduction",
                              "injury risk reduction", 
                              "crash avoidance"),
                   use_logic = c(TRUE, FALSE)) %>% 
  mutate(proposal_dist = ifelse(sampling_method != "importance sampling", "NA", proposal_dist), 
         target = ifelse(sampling_method != "active sampling", "NA", target)) %>% 
  filter(!duplicated(.))


# Run.
nsim <- 10
niter <- 10
# for ( i in 1:nrow(params) ) {
i <- 1
set.seed(1234)

# Setup parallel backend to use many processors
cores <- detectCores(logical = TRUE)
cl <- makeCluster(cores[1] - 1) # Not to overload computer
registerDoParallel(cl)

tic()
finalRes <- foreach(j = 1:nsim, .combine = rbind, .verbose = TRUE,
                    .export = "df",
                    .packages = c("boot", "caret", "magrittr", "ranger", "stringr", "tidyverse")) %dopar% {
                      # for (j in 1:nsim) {
                      
                      cat(sprintf("Processor %d, Sampling method = %s\nProposal dist = %s\nTarget = %s\nuse logic = %s\n",
                                    i, 
                                    params$sampling_method[i],
                                    params$proposal_dist[i],
                                    params$target[i],
                                    params$use_logic[i]))
                      cat("\n")
                      
                      # Active sampling.
                      res <- active_sampling(df, 
                                             sampling_method = params$sampling_method[i],
                                             proposal_dist = params$proposal_dist[i],
                                             target = params$target[i],
                                             use_logic = params$use_logic[i],
                                             niter = niter,
                                             nboot = 0, 
                                             verbose = FALSE)  
                      
                      # Results dataset.
                      res$results %>% 
                        mutate(rep = j)
                    }

# }
stopCluster(cl) # Stop cluster
toc()

