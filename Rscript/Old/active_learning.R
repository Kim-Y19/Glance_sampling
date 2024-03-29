################################################################################
#
# active_learning.R
#
# INPUT:
#
# data: input dataset with variables
#   - 'caseID': ID for original crash event. 
#   - 'eoff': glance duration off road after tauinv = 0.2 s (overshot).
#   - 'acc': acceleration (negative value means positive deceleration).
#   - 'eoff_acc_prob': probability of (eoff, acc) pair according to baseline distribution.
#   - 'impact_speed0': impact speed in baseline scenario.
#   - 'impact_speed1': impact speed  in counter factual scenario (i.e., with counter measure such as AEB).                           
#   - 'injury_risk0': injury risk in baseline scenario.
#   - 'injury_risk1': injury risk in counter factual scenario (i.e. with counter measure such as AEB).                           
#
# sampling_method: SRS (simple random sampling), importance sampling (probability proportional to 'size'), or optimised.
#
# target: target of optimisation, only used when sampling_method = "optimised".
#
# use_logic:  Use logical constraints (TRUE or FALSE) to infer regions with certainty outcomes 
#             (no crash or maximal impact speed collision) and avoid sampling in those regions.
#
# n_cases_per_iter: number of cases to sample from per iteration. 
#
# niter: number of iterations.
#
# nboot: number of bootstrap replicates used to calculate confidence intervals.
#
# verbose: should iteration progress be printed to console? (TRUE/FALSE).
#
# plot: should plots of predicted outcomes and sampling probabilities be produced? (TRUE/FALSE).
#
#
# OUTPUT: 
#
# List of three datasets:
#   - results: meta information of simulation, iteration history, estimates with standard errors and squared errors.
#   - labelled: all labelled data points. 
#   - crashes: all generated crashes. 
#
################################################################################


active_learning <- function(data, 
                            sampling_method = c("SRS", 
                                                "importance sampling", 
                                                "optimised"), 
                            proposal_dist = c("NA", # Only used when sampling_method = "importance sampling", "NA" otherwise.
                                              "pps, size = prior weight", 
                                              "pps, size = prior weight * severity"), 
                            target = c("NA", # Only used when sampling_method = "optimised", "NA" otherwise.
                                       "baseline impact speed distribution", 
                                       "impact speed reduction",
                                       "injury risk reduction", 
                                       "crash avoidance",
                                       "all"),
                            use_logic = TRUE, # TRUE or FALSE. 
                            n_cases_per_iter = 1,
                            niter = 500, 
                            nboot = 100, 
                            verbose = FALSE, # TRUE or FALSE.
                            plot = FALSE) { # TRUE or FALSE.
  
  
  # Make sure packages are loaded. ----
  require("boot")
  require("caret")
  require("magrittr")
  require("ranger")
  require("sampling")
  require("tidyverse")
  
  
  # Calculate variables. ----
  data %<>% 
    mutate(impact_speed_reduction = impact_speed0 - impact_speed1,
           injury_risk_reduction = injury_risk0 - injury_risk1)

  
  # Check input parameters. ----
  sampling_method <- match.arg(sampling_method)
  proposal_dist <- match.arg(proposal_dist)
  target <- match.arg(target)

  # proposal_dist should be "NA" when sampling_method not equal to "importance sampling".
  if ( sampling_method != "importance sampling" & proposal_dist != "NA") { 
    stop(sprintf("Error in active_learning. sampling_method = %s and proposal_dist = %s not allowed.", 
                 sampling_method, proposal_dist))
  } 
  
  # target should be "NA" when sampling_method not equal to "optimised".
  if ( sampling_method != "optimised" & target != "NA") { 
    stop(sprintf("Error in active_learning. sampling_method = %s and target = %s not allowed.", 
                 sampling_method, target))
  } 
  
  # proposal_dist must be specified if sampling_method = "importance sampling".
  if ( sampling_method == "importance sampling" & proposal_dist == "NA" ) {
    stop("Error in active_learning. sampling_method = importance sampling and proposal_dist = NA not allowed.")
  }

  # target must be specified if sampling_method = "optimised".
  if ( sampling_method == "optimised" & target == "NA" ) {
    stop("Error in active_learning. sampling_method = optimised and target = NA not allowed.")
  }
  
  # n_cases_per_iter should be integer between 1 and number of cases in input dataset.
  n_cases_per_iter <- round(n_cases_per_iter)
  if ( n_cases_per_iter < 1 ) {
    stop("Error in active_learning. n_cases_per_iter must be greater than or equal to 1.")
  }
  # if ( n_cases_per_iter > length(unique(df$caseID)) ) {
  #   stop(sprintf("Error in active_learning. n_cases_per_iter must be smaller than or equal to %d (number of cases in input dataset).", 
  #                length(unique(df$caseID))))
  # }
  
  
  # Load helper functions. ----
  source("Rscript/calculate_sampling_scheme.R")
  source("Rscript/estimate_targets.R")
  source("Rscript/find_crashes.R")
  source("Rscript/find_max_impact_crashes.R")
  source("Rscript/find_non_crashes.R")
  source("Rscript/initialise_grid.R")
  source("Rscript/KL.R")
  source("Rscript/safe_caret_train.R")
  source("Rscript/safe_UPmaxentropy.R")
  source("Rscript/update_predictions.R")
  
  
  # Set some parameters. ----
  
  res <- NULL # To store results.
  n_cases <- length(unique(df$caseID)) # Number of cases in input dataset.
  ground_truth <- estimate_targets(data, weightvar = "eoff_acc_prob") # Calculate target quantities on full data.
  n_seq <- cumsum(rep(n_cases_per_iter, niter)) # Cumulative number of baseline scenario simulations. 


  # For optimised sampling:
  # Prediction models will be updated every when n_update observations have been collected.
  # Find corresponding iteration indices model_update_iterations.
  if ( sampling_method == "optimised" ) {

    n_update <- c(seq(10, 100, 10), seq(125, 500, 25), seq(550, 1000, 50), seq(1100, 2000, 100), seq(2200, 5000, 200), seq(5500, 10000, 500))
    model_update_iterations <- vapply(1:length(n_update), function(ix) which(c(n_seq, 0) > n_update[ix] & c(0, n_seq) > n_update[ix])[1] - 1, FUN.VALUE = numeric(1))
    model_update_iterations <- as.numeric(na.omit(model_update_iterations))
    model_update_iterations <- unique(model_update_iterations[model_update_iterations > 1])

    if ( verbose ) {
      print(sprintf("Predictions updated at iterations %s", paste(model_update_iterations, collapse = ", ")))
      print(sprintf("after %s observations", paste(n_seq[model_update_iterations - 1], collapse = ", ")))
    }
    
  }
  
  
  # If bootstrap is used.
  if ( nboot > 0 & niter * n_cases_per_iter >= 10) {
    
    n_update <- seq(10, niter * n_cases_per_iter, 10)
    boot_update_iterations <- vapply(1:length(n_update), function(ix) which(c(n_seq, max(n_seq) + 1) >= n_update[ix] & c(0, n_seq) >= n_update[ix])[1] - 1, FUN.VALUE = numeric(1))
    boot_update_iterations <- unique(as.numeric(na.omit(boot_update_iterations)))
    
    if ( verbose ) {
      print(sprintf("Bootstrap standard error updated at iterations %s", paste(boot_update_iterations, collapse = ", ")))
      print(sprintf("after %s observations", paste(n_seq[boot_update_iterations], collapse = ", ")))
    }
    
  }
  
 
  # Initialise labelled and unlabelled datasets. ----
  grid <- tibble(eoff = max(data$eoff), acc = max(data$acc)) 
  init <- initialise_grid(data, grid)
  labelled <- init$labelled 
  unlabelled <- init$unlabelled 
  
  if ( use_logic | sampling_method == "optimised" ) {
    labelled$sim_count0 <- 1
    labelled$sim_count1 <- 1
  }
  if ( sampling_method == "importance sampling" & proposal_dist == "pps, size = prior weight * severity" ) {
    labelled$sim_count0 <- 1
  }
  n_seq <- n_seq + sum(labelled$sim_count0) + sum(labelled$sim_count1)

  
  # Iterate. ----
  new_sample <- labelled 
  for ( i in 1:niter ) {
    
    # Print iteration number if verbose = TRUE.
    if ( verbose ) { print(sprintf("Iteration %d", i)) }
    
    
    # Logic. ---
    
    # Find all known crashes in unlabelled dataset.
    ix <- find_crashes(new_sample, unlabelled)
    
    unlabelled %<>%
      mutate(crash0 = ifelse(row_number() %in% ix$crashes0, 1, crash0),
             crash1 = ifelse(row_number() %in% ix$crashes0, 1, crash1)) 
    
    # Find all known maximal impact speed crashes in unlabelled dataset.
    ix <- find_max_impact_crashes(new_sample, labelled, unlabelled)
    
    unlabelled %<>%
      mutate(max_impact0 = ifelse(row_number() %in% ix$max_impact_crashes0, 1, max_impact0),
             max_impact1 = ifelse(row_number() %in% ix$max_impact_crashes1, 1, max_impact1),
             sim_count0 = ifelse(row_number() %in% ix$max_impact_crashes0, 0, sim_count0),
             sim_count1 = ifelse(row_number() %in% ix$max_impact_crashes1, 0, sim_count1)) 
    
    # Find all known non-crashes in unlabelled dataset.
    ix <- find_non_crashes(new_sample, unlabelled)
    
    unlabelled %<>% 
      mutate(non_crash0 = ifelse(row_number() %in% ix$non_crashes0, 1, non_crash0),
             non_crash1 = ifelse(row_number() %in% ix$non_crashes1, 1, non_crash1),
             sim_count0 = ifelse(row_number() %in% ix$non_crashes0, 0, sim_count0),
             sim_count1 = ifelse(row_number() %in% ix$non_crashes1, 0, sim_count1)) 
    
    # If use_logic (elimination) = TRUE. ----
    if ( use_logic ) {
      
      # Remove certainty non-crashes from unlabelled set.
      unlabelled %<>% 
        filter(!(row_number() %in% ix$non_crashes0)) 
      
    } 
    
    
    # Update predictions. ----
    if ( sampling_method == "optimised" && i %in% model_update_iterations ) {
      
      if ( verbose ) { print("Update predictions.") }
      
      # Calculated predictions.
      pred <- update_predictions(labelled, unlabelled, target = target) 
      
      # Prediction R-squared and RMSE.
      r2 <- list(impact_speed0 = pred$r2_impact_speed0,
                 impact_speed_reduction = pred$r2_impact_speed_reduction,
                 injury_risk_reduction = pred$r2_injury_risk_reduction,
                 accuracy_crash0 = pred$accuracy_crash0,
                 accuracy_crash1 = pred$accuracy_crash1)
      
      rmse <- list(log_impact_speed0 = pred$rmse_log_impact_speed0,
                   impact_speed_reduction = pred$rmse_impact_speed_reduction,
                   injury_risk_reduction = pred$rmse_injury_risk_reduction)

      # Add to unlabelled dataset.
      unlabelled %<>% 
        mutate(collision_prob0_pred = pred$collision_prob0,
               collision_prob1_pred = pred$collision_prob1,
               impact_speed0_pred = pred$impact_speed0_pred, 
               impact_speed_reduction_pred = pred$impact_speed_reduction_pred,
               injury_risk_reduction_pred = pred$injury_risk_reduction_pred)
      
    }  # End update predictions.
    
    
    # Calculate sampling probabilities. ----
 
    # Set R-squares and RMSEs to NA if sampling method is not equal to "optimised" 
    # or if prediction models for optimised sampling has not (yet) been fitted.
    if ( sampling_method != "optimised" | !exists("pred") ) {
      r2 <- list(impact_speed0 = NA_real_,
                 impact_speed_reduction = NA_real_,
                 injury_risk_reduction = NA_real_,
                 accuracy_crash0 = NA_real_,
                 accuracy_crash1 = NA_real_)
      rmse <- list(log_impact_speed0 = NA,
                   impact_speed_reduction = NA,
                   injury_risk_reduction = NA)
    } 
    
    # Sets estimates to NA if target quantities have not (yet) been estimated.
    if ( !exists("est") ) {
      est <- estimate_targets(labelled, weightvar = "eoff_acc_prob")
    }
    
    # Calculate sampling scheme.
    prob <- calculate_sampling_scheme(unlabelled, 
                                      labelled, 
                                      sampling_method, 
                                      proposal_dist, 
                                      target, 
                                      n_cases_per_iter,
                                      est = as.list(est),
                                      r2 = r2,
                                      rmse = rmse)
    
    # If predictions have been updated: update previous estimate of 'size'.
    # Only use with optimised sampling.
    if ( sampling_method == "optimised" && i %in% (model_update_iterations[-1] - 1) ) {
      unlabelled$size <- prob$size
    }
    

    if ( plot ) {
      plot(unlabelled$eoff, prob$sampling_probability, 
           col = unlabelled$caseID, 
           pch = match(unlabelled$acc, sort(unique(unlabelled$acc))), 
           main = sprintf("Iteration %d", i), 
           bty = "l")
    }
    
 
    # Sample new instances
    new_wt <- as.numeric(rmultinom(n = 1, size = n_cases_per_iter, prob = prob$sampling_probability)) / 
      prob$sampling_probability

    
    # Get data for sampled observations.
    new_sample <- unlabelled %>% 
      mutate(old_weight = 0, 
             new_weight = new_wt) %>% 
      filter(new_weight > 0) %>% 
      dplyr::select(caseID, eoff, acc, eoff_acc_prob, sim_count0, sim_count1, old_weight, new_weight, iter) %>% 
      mutate(iter = i)%>%
      left_join(data, by = c("caseID", "eoff", "acc", "eoff_acc_prob"))
    
    
    # Update labelled set.
    bwt <- 1 / i # Batch weight is 1/i since all n_t are equal. 
    labelled <- labelled %>%
      mutate(old_weight = sampling_weight,
             new_weight = 0) %>% 
      add_row(new_sample) %>%
      mutate(sampling_weight = old_weight + bwt * (new_weight - old_weight)) %>% # Update sampling weights. 
      dplyr::select(-old_weight, -new_weight) %>% 
      group_by(caseID, eoff, acc, eoff_acc_prob, iter, impact_speed0, impact_speed1, injury_risk0, injury_risk1, impact_speed_reduction, injury_risk_reduction) %>% 
      summarise_all(sum) %>% 
      mutate(final_weight = eoff_acc_prob * sampling_weight) %>% 
      ungroup()

    
    # Estimate target quantities.
    crashes <- labelled %>% filter(impact_speed0 > 0 & final_weight > 0)
    effective_number_simulations0 <- effective_number_simulations1 <- n_seq[i]
    actual_number_simulations0 <- sum(labelled$sim_count0)
    actual_number_simulations1 <- sum(labelled$sim_count1)
    
    if ( nrow(crashes) > 0 ) { # If any crashes have been generated.
      
      boot <- boot(crashes, 
                   statistic = function(data, ix) estimate_targets(data[ix, ], weightvar = "final_weight"), 
                   R = ifelse(nboot > 0 && i %in% boot_update_iterations, nboot, 0) ) # Run bootstrap every 10th sample.
      
      est <- boot$t0 # Estimates.
      se <- apply(boot$t, 2 , sd) # Standard error of estimates.
      lower <- est - qnorm(0.975) * se # Confidence limits
      upper <- est + qnorm(0.975) * se
      
    } else {
      
      est <- estimate_targets(crashes) # Returns NaN if crashes is empty set.
      se <- lower <- upper <- rep(NA, length(est))
      
    }
    sqerr <- (est - ground_truth)^2 # Squared error with respect to ground truth.
    cov <- as.numeric(lower < ground_truth & ground_truth < upper)
    names(se) <- paste0(names(est), "_se")
    names(sqerr) <- paste0(names(est), "_sqerr")
    names(cov) <- paste0(names(est), "_ci_cover")
    
    # Prediction R-squares.
    r2_tbl <- as_tibble(r2)
    names(r2_tbl) <- c("r2_impact_speed0", "r2_impact_speed_reduction", "r2_injury_risk_reduction", "accuracy_crash0", "accuracy_crash1")
    # print(r2_tbl)
    
    # Append results.
    newres <- tibble(sampling_method = sampling_method,
                     proposal_dist = proposal_dist,
                     target = target,
                     use_logic = use_logic,
                     n_cases_per_iter = n_cases_per_iter,
                     labelled_mean_impact_speed0 = sum(labelled$impact_speed0*labelled$eoff_acc_prob)/sum(labelled$eoff_acc_prob),
                     labelled_mean_impact_speed1 = sum(labelled$impact_speed1*labelled$eoff_acc_prob)/sum(labelled$eoff_acc_prob),
                     labelled_mean_injury_risk0 = sum(labelled$injury_risk0*labelled$eoff_acc_prob)/sum(labelled$eoff_acc_prob),
                     labelled_mean_injury_risk1 = sum(labelled$injury_risk1*labelled$eoff_acc_prob)/sum(labelled$eoff_acc_prob),
                     labelled_mean_crash_avoidance = sum(crashes$impact_speed1 == 0)*sum(crashes[crashes$impact_speed1 == 0,]$eoff_acc_prob)/
                       sum(crashes$impact_speed0 > 0)/sum(crashes[crashes$impact_speed0 > 0,]$eoff_acc_prob)) %>% # Meta-information.
      add_column(iter = i, # Iteration history.
                 neff0 = effective_number_simulations0, 
                 neff1 = effective_number_simulations1, 
                 neff_tot = effective_number_simulations0 + effective_number_simulations1,
                 nsim0 = actual_number_simulations0, 
                 nsim1 = actual_number_simulations1, 
                 nsim_tot = actual_number_simulations0 + actual_number_simulations1) %>% 
      add_column(as_tibble(as.list(est))) %>% # Estimates.
      add_column(as_tibble(as.list(se)))  %>% # Standard errors.
      add_column(as_tibble(as.list(sqerr))) %>% # Squared errors.
      add_column(as_tibble(as.list(cov))) %>% # Confidence interval coverage.
      add_column(impact_speed0_KLdiv = KL(ground_truth["impact_speed0_logmean"], 
                                          ground_truth["impact_speed0_logSD"],
                                          est["impact_speed0_logmean"], 
                                          est["impact_speed0_logSD"])) %>% # Kullback-Leibler divergence.
      add_column(r2_tbl) # Prediction R-squared and accuracy.

    
    if ( is.null(res) ) {
      res <- newres
    } else {
      res %<>% 
        add_row(newres)
    }
    
  } # End active learning.
  
  return(list(results = res, 
              labelled = labelled, 
              crashes = labelled %>% filter(impact_speed0 > 0)))
  
}
