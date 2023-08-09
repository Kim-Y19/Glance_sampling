################################################################################
#
# check_parallel.R
#
################################################################################

# Clean-up.
rm(list = ls())
gc()

# Load packages.
library("bigstatsr")
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
nsim <- 20
niter <- 50
# for ( i in 1:nrow(params) ) {
i <- 1
set.seed(1234)

# Setup parallel backend to use many processors
cl <- makeCluster(10) # Not to overload computer
registerDoParallel(cl)

tic()
for (j in 1:nsim) {
  
  cat(sprintf("Process %d, Sampling method = %s\nProposal dist = %s\nTarget = %s\nuse logic = %s\n",
              j,
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
toc()

tic()
finalRes <- foreach(j = 1:nsim, .combine = rbind, .verbose = TRUE,
                    .packages = c("boot", "caret", "magrittr", "ranger", "stringr", "tidyverse")) %dopar% {
                      
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
toc()

tic()
finalRes <- foreach(j = 1:nsim, .combine = rbind, .verbose = TRUE,
                    .packages = c("boot", "caret", "magrittr", "ranger", "stringr", "tidyverse")) %dopar% {
                      
                      # Active sampling.
                      res <- active_sampling(df, 
                                             sampling_method = params$sampling_method[i],
                                             proposal_dist = params$proposal_dist[i],
                                             target = params$target[i],
                                             use_logic = params$use_logic[i],
                                             niter = niter,
                                             nboot = 0, 
                                             verbose = FALSE)  
                      
                      j
                    }
toc()


# To store results. 
tic()
SE_MISR <- FBM(nrow = niter, ncol = nsim, init = NA) # Squared error, mean impact speed reduction
SE_MIJR <- FBM(nrow = niter, ncol = nsim, init = NA) # Squared error, mean injury risk reduction
SE_CAR <- FBM(nrow = niter, ncol = nsim, init = NA) # Squared error, crash avoidance rate
finalRes <- foreach(j = 1:nsim, .combine = rbind, .verbose = TRUE,
                    .packages = c("boot", "caret", "magrittr", "ranger", "stringr", "tidyverse")) %dopar% {
                      
                      # Active sampling.
                      res <- active_sampling(df, 
                                             sampling_method = params$sampling_method[i],
                                             proposal_dist = params$proposal_dist[i],
                                             target = params$target[i],
                                             use_logic = params$use_logic[i],
                                             niter = niter,
                                             nboot = 0, 
                                             verbose = FALSE)  
                      
                      SE_MISR[, j] <- res$results$mean_impact_speed_reduction_sqerr
                      SE_MIJR[, j] <- res$results$mean_injury_risk_reduction_sqerr
                      SE_CAR[, j] <- res$results$crash_avoidance_rate_sqerr
                      
                      j
                    }

# }
toc()
stopCluster(cl) # Stop cluster
