################################################################################
#
# active_sampling.R
#
# INPUT:
#
# data: input dataset with variables
#   - 'caseID': ID for original crash event. 
#   - 'eoff': off-road glance after tauinv = 0.2 s (overshot).
#   - 'acc': acceleration (negative value means positive deceleration).
#   - 'eoff_acc_prob': probability of (eoff, acc) pair according to baseline distribution.
#   - 'impact_speed0': impact speed in baseline scenario.
#   - 'impact_speed1': impact speed  in counter factual scenario (i.e., with counter measure such as AEB).                           
#   - 'injury_risk0': injury risk in baseline scenario.
#   - 'injury_risk1': injury risk in counter factual scenario (i.e. with counter measure such as AEB).                           
#
# sampling_method: simple random sampling, importance sampling, or active sampling.
#
# target: target of optimisation, only used when sampling_method = "active sampling".
#
# use_logic:  Use logical constraints (TRUE or FALSE) to infer regions with certainty outcomes 
#             (no crash or maximal impact speed collision) and avoid sampling in those regions.
#
# n_per_case: number of observations to sample per case and iteration. 
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


active_sampling <- function(data, 
                            sampling_method = c("simple random sampling", 
                                                "importance sampling", 
                                                "active sampling"), 
                            proposal_dist = c("NA", # Only used when sampling_method = "importance sampling", "NA" otherwise.
                                              "density sampling", 
                                              "severity sampling"), 
                            target = c("NA", # Only used when sampling_method = "active sampling", "NA" otherwise.
                                       "impact speed reduction",
                                       "injury risk reduction", 
                                       "crash avoidance"),
                            use_logic = FALSE, # TRUE or FALSE. 
                            n_per_case = 1,
                            niter = 500, 
                            verbose = FALSE, # TRUE or FALSE.
                            plot = FALSE, # TRUE or FALSE.
                            nboot = 200) { 
  
  
  # Make sure packages are loaded. ----
  library("boot")
  library("caret")
  library("magrittr")
  library("ranger")
  library("stringr")
  library("tidyverse")
  
  
  # Calculate some variables. ----
  data %<>%
    dplyr::select(caseID, eoff, acc, eoff_acc_prob, impact_speed0, impact_speed1, injury_risk0, injury_risk1)

  maximpact <- data %>% 
    group_by(caseID) %>% 
    summarise(impact_speed_max0 = max(impact_speed0, na.rm = TRUE), .groups = "keep") %>% 
    ungroup() %>% 
    dplyr::select(caseID, impact_speed_max0)
  
  data %<>% 
    mutate(impact_speed_reduction = impact_speed0 - impact_speed1,
           injury_risk_reduction = injury_risk0 - injury_risk1,
           crash_avoidance = as.numeric( (impact_speed0 > 0) * (impact_speed1 == 0)) ) %>%  
    left_join(maximpact, by = "caseID")
  

  # Check input parameters. ----
  sampling_method <- match.arg(sampling_method)
  proposal_dist <- match.arg(proposal_dist)
  target <- match.arg(target)

  # proposal_dist should be "NA" when sampling_method not equal to "importance sampling".
  if ( sampling_method != "importance sampling" & proposal_dist != "NA") { 
    stop(sprintf('Sampling_method = "%s" and proposal_dist = "%s" not allowed.', 
                 sampling_method, proposal_dist))
  } 
  
  # target should be "NA" when sampling_method not equal to "active sampling".
  if ( sampling_method != "active sampling" & target != "NA") { 
    stop(sprintf('Sampling_method = "%s" and target = "%s" not allowed.', 
                 sampling_method, target))
  } 
  
  # proposal_dist must be specified if sampling_method = "importance sampling".
  if ( sampling_method == "importance sampling" & proposal_dist == "NA" ) {
    stop('Sampling_method = "importance sampling" and proposal_dist = "NA" not allowed.')
  }
  
  # target must be specified if sampling_method = "active sampling".
  if ( sampling_method == "active sampling" & target == "NA" ) {
    stop('Sampling_method = "optimised"" and target = "NA" not allowed.')
  }
  
  # n_per_case should be integer greater than or equal to 1.
  n_per_case <- round(n_per_case)
  if ( n_per_case < 1 ) {
    stop("n_per_case must be greater than or equal to 1.")
  }
  
  # Load helper functions. ----
  source("Rscript/calculate_sampling_scheme.R")
  source("Rscript/estimate_targets.R")
  source("Rscript/estimate_targets_by_case.R")
  source("Rscript/find_crashes.R")
  source("Rscript/find_max_impact_crashes.R")
  source("Rscript/find_non_crashes.R")
  source("Rscript/initialise_grid.R")
  source("Rscript/safe_caret_train.R")
  source("Rscript/update_predictions.R")
  
  
  # Set some parameters. ----
  
  res <- NULL # To store results.
  case_ID_sum <- unique(data$caseID)
  ground_truth <- estimate_targets(data, weightvar = "eoff_acc_prob") # Calculate target quantities on full data.
  n_cases <- length(unique(data$caseID)) # Number of cases.
  batch_size <- n_per_case * n_cases # Batch size per iteration.
  nseq <- cumsum(rep(batch_size, niter)) # Cumulative number of baseline scenario simulations. 
# browser()
  # For optimised sampling:
  # Prediction models will be updated n_update observations have been collected.
  # Find corresponding iteration indices model_update_iterations.
  # For optimised sampling:
  # Prediction models will be updated n_update observations have been collected.
  # Find corresponding iteration indices model_update_iterations.
  if ( sampling_method == "active sampling" & niter > 1 ) {
    
    model_update_iterations <- seq(1, niter)
    
    if ( verbose ) {
      cat(sprintf("Predictions updated at iterations %s", paste(model_update_iterations, collapse = ", ")))
      cat(sprintf("\nafter %s observations.", paste(nseq[model_update_iterations - 1], collapse = ", ")))
      cat("\n")
    }
    
  } else {
    model_update_iterations <- NA
  }
  
  # If plots should be produced.
  plot_iter <- NA
  if ( plot ) {
    
    n_update <- seq(0, niter * batch_size, 100)[-1]
    plot_iter <- vapply(1:length(n_update), function(ix) which(c(nseq, 0) > n_update[ix] & c(0, nseq) > n_update[ix])[1] - 1, FUN.VALUE = numeric(1))
    plot_iter <- as.numeric(na.omit(plot_iter))
    plot_iter <- unique(plot_iter[plot_iter > 1])
    
    if ( verbose & length(n_update) > 0 ) {
      cat(sprintf("Plotting predictions and sampling schemes at iteration %s", paste(plot_iter, collapse = ", ")))
      cat(sprintf("after %s observations", paste(nseq[plot_iter - 1], collapse = ", ")))
      cat("\n")
    }  
  } 
  plot_iter <- ifelse(length(plot_iter) == 0, NA, plot_iter) # Make sure not empty.
  
  
  # Plot baseline impact speed distribution. ----
  
  # 1D.
  if (plot) {
    ggplot(data %>% filter(caseID <= 42)) + # Plot 42 cases on 7x6 grid.
      geom_point(aes(x = eoff, y = impact_speed0, colour = -acc)) +
      scale_colour_continuous(type = "viridis") +
      labs(x = "OEOFF (s)",
           y = "Baseline impact speed (km/h)",
           colour = bquote('Maximal deceleration '(km/s^2))) +
      facet_wrap(~caseID, ncol = 7, nrow = 6, labeller = labeller(caseID = function(x) "")) + 
      theme_classic() + 
      theme(panel.spacing = unit(0.1, "cm"), # Increase/decrease to increase/reduce horizontal white space between cases
            strip.background = element_blank(),
            strip.placement = "outside",
            strip.text = element_text(size = 0), # Change size to increase/reduce vertical white space between cases. Set to element_blank() for no white space.
            legend.direction = "horizontal",
            legend.position = "top",
            legend.key.height = unit(0.3, "cm"), # Change width and height as necessary.
            legend.key.width = unit(2, "cm")) +
      guides(colour = guide_colourbar(title.position = "top", title.hjust = 0.5))
    
    filename <- sprintf("Output/BaselinImpactSpeed_1D.png")
    ggsave(filename, width = 160, height = 100, unit = "mm", dpi = 1000)
    
    # 2D.
    ggplot(data %>% filter(caseID <= 42)) + # Plot 42 cases on 7x6 grid.
      geom_rect(aes(xmin = eoff - 0.05, xmax = eoff + 0.05, ymin = -acc - 0.25, ymax = -acc + 0.25, fill = impact_speed0)) +
      scale_fill_continuous(type = "viridis") +
      labs(x = "OEOFF (s)",
           y = bquote('Maximal deceleration '(km/s^2)),
           fill = "Baseline impact speed (km/h)") +
      facet_wrap(~caseID, ncol = 7, nrow = 6, labeller = labeller(caseID = function(x) "")) + 
      theme_classic() + 
      theme(panel.spacing = unit(0.1, "cm"), # Increase/decrease to increase/reduce horizontal white space between cases
            strip.background = element_blank(),
            strip.placement = "outside",
            strip.text = element_text(size = 0), # Change size to increase/reduce vertical white space between cases. Set to element_blank() for no white space.
            legend.direction = "horizontal",
            legend.position = "top",
            legend.key.height = unit(0.3, "cm"), # Change width and height as necessary.
            legend.key.width = unit(2, "cm")) +
      guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
    
    filename <- sprintf("Output/BaselinImpactSpeed_2D.png")
    ggsave(filename, width = 160, height = 100, unit = "mm", dpi = 1000)
  }
  
  
  # Initialise labelled and unlabelled datasets. ----
  
  grid <- tibble(eoff = max(data$eoff), acc = max(data$acc)) 
  init <- initialise_grid(data, grid)
  labelled <- init$labelled 
  unlabelled <- init$unlabelled 
 
  # Update simulation counts for initialisation.
  if ( use_logic | sampling_method == "active sampling" | (sampling_method == "importance sampling" & proposal_dist == "severity sampling") ) {
    n_init <- nrow(labelled)
  } else {
    n_init <- 0
  } 

  # If use_logic = TRUE: reduce simulation counts using logic.
  if ( use_logic ) {
    unlabelled %<>% 
      mutate(sim_count1 = ifelse(impact_speed0 > 0, 1, 0))
    
    labelled$sim_count0 <- 1
    labelled$sim_count1 <- 1
  }
  
  if ( proposal_dist == "severity sampling" ) {
    labelled$sim_count0 <- 1
  }
  
  # If no initialisation: start with empty sample.
  # if ( sampling_method != "active sampling"  ) {
  #   labelled %<>% filter(1 == 0)
  # }

  nseq0 <- nseq + n_init
  nseq1 <- nseq + n_init * (sampling_method == "active sampling" | use_logic)

  
  # Iterate. ----
  new_sample <- labelled 
  for ( i in 1:niter ) {
    
    # Print iteration number if verbose = TRUE.
    if ( verbose ) { cat(sprintf("\nIteration %d.", i)) }
    
    
    # If use_logic = TRUE. ---
    if ( use_logic ) {
      
      # Find all known crashes in unlabelled dataset.
      ix <- find_crashes(new_sample, unlabelled)
      
      unlabelled %<>%
        mutate(crash0 = ifelse(row_number() %in% ix$crashes0, 1, crash0),
               crash1 = ifelse(row_number() %in% ix$crashes0, 1, crash1)) 
      
      # Find all known maximal impact speed crashes in unlabelled dataset.
      ix <- find_max_impact_crashes(new_sample, maximpact, unlabelled)
      
      unlabelled %<>%
        mutate(max_impact0 = ifelse(row_number() %in% ix$max_impact_crashes0, 1, max_impact0),
               sim_count0 = ifelse(row_number() %in% ix$max_impact_crashes0, 0, sim_count0)) 
      
      if ( i > 1 ) {
        max_impact_certainty_selections <- unlabelled %>%
          filter(row_number() %in% ix$max_impact_crashes0) %>%
          mutate(max_impact0 = 1,
                 sim_count0 = 0,
                 sim_count1 = 0,
                 batch_size = batch_size,
                 nhits = 1,
                 pi = 1,
                 mu = n_per_case * pi,
                 sampling_weight = 1 / mu,
                 batch_weight = batch_size / nseq[i],
                 final_weight = eoff_acc_prob * nhits * sampling_weight) %>%
          dplyr::select(caseID, eoff, acc, eoff_acc_prob, sim_count0, sim_count1, iter, max_impact0, batch_size, nhits, pi, mu, sampling_weight, batch_weight, final_weight) %>%
          mutate(iter = i - 1) %>%
          left_join(data, by = c("caseID", "eoff", "acc", "eoff_acc_prob"))
        
        # Add to labelled set.
        labelled %<>%
          add_row(max_impact_certainty_selections)
        
        # Remove from unlabelled set.
        unlabelled %<>%
          filter(!(row_number() %in% ix$max_impact_crashes0))
      #   
      }
      
      # Find all known non-crashes in unlabelled dataset.
      ix <- find_non_crashes(new_sample, unlabelled)
      
      unlabelled %<>% 
        mutate(non_crash0 = ifelse(row_number() %in% ix$non_crashes0, 1, non_crash0),
               non_crash1 = ifelse(row_number() %in% ix$non_crashes1, 1, non_crash1),
               sim_count0 = ifelse(row_number() %in% ix$non_crashes0, 0, sim_count0),
               sim_count1 = ifelse(row_number() %in% ix$non_crashes1, 0, sim_count1)) 
      
      # Remove certainty non-crashes from unlabelled set.
      unlabelled %<>%
        filter(!(row_number() %in% ix$non_crashes0))
    #   
    }


    # Update predictions. ----
    if ( sampling_method == "active sampling" && i %in% model_update_iterations ) {
      
      if ( verbose ) { cat("\nUpdate predictions.") }
      
        # Update predictions.
        pred <- update_predictions(labelled, 
                                   unlabelled, 
                                   target, 
                                   use_logic, 
                                   verbose = verbose,
                                   plot = plot & i %in% plot_iter, iter = i) 
        
        unlabelled[, names(pred)] <- pred
        
    }  # End update predictions.
    

    # Update estimates, per case ----
    if ( sampling_method == "active sampling" && i > min(model_update_iterations) ) {
      est_by_case <- estimate_targets_by_case(labelled %>% 
                                                filter(is.na(max_impact0) | max_impact0 != 1) %>% 
                                                add_row(certainty_selections) %>% 
                                                filter(impact_speed0 > 0 & final_weight > 0), "final_weight",
                                              case_ID_sum)
      
      # BLUP estimate.
      sigma_e <- sqrt(apply(se_by_case^2 * est_by_case$n, 2, mean, na.rm = TRUE))
      sigma_u <- apply(est_by_case[, c("mean_impact_speed_reduction", "mean_injury_risk_reduction", "mean_crash_avoidance")], 2, sd, na.rm = TRUE)
      icc <- vapply(1:n_cases, function(ix) sigma_u^2 / (sigma_u^2 + sigma_e^2 / est_by_case$n[ix]), numeric(3))
      est_by_case[, 3:5] <- t(icc) * est_by_case[, 3:5] + t((1 - icc) * est)
      
      # Fix NAs.
      est_by_case$mean_impact_speed_reduction[is.na(est_by_case$mean_impact_speed_reduction)] <- est["mean_impact_speed_reduction"]
      est_by_case$mean_injury_risk_reduction[is.na(est_by_case$mean_injury_risk_reduction)] <- est["mean_injury_risk_reduction"]
      est_by_case$mean_crash_avoidance[is.na(est_by_case$mean_crash_avoidance)] <- est["crash_avoidance_rate"]
      
    
      # BLUP estimate of standard error.
      se_by_case_blup <- t(vapply(1:n_cases, 
                                  function(ix) sqrt( (est_by_case$n[ix] * se_by_case[ix, ]^2 + sigma_e^2) / (est_by_case$n[ix] + 1) ), 
                                  numeric(3)))
      
      # Fix NAs.
      se_by_case_blup <- as.data.frame(se_by_case_blup)
      se_by_case_blup[which(is.na(se_by_case_blup[, 1])), 1] <- sigma_e[1]
      se_by_case_blup[which(is.na(se_by_case_blup[, 2])), 2] <- sigma_e[2]
      se_by_case_blup[which(is.na(se_by_case_blup[, 3])), 3] <- sigma_e[3]
      names(se_by_case_blup) <- c("se_mean_impact_speed_reduction", "se_mean_injury_risk_reduction", "se_mean_crash_avoidance")
      
      est_by_case <- cbind(est_by_case, se_by_case_blup) %>% 
        dplyr::select(-n)

      # Add to unlabelled set.
      unlabelled %<>%
        dplyr::select(-contains("mean")) %>%
        left_join(est_by_case, by = "caseID") %>%
        mutate(mean_impact_speed_reduction = ifelse(is.na(mean_impact_speed_reduction), 0, mean_impact_speed_reduction),
               mean_injury_risk_reduction = ifelse(is.na(mean_injury_risk_reduction), 0, mean_injury_risk_reduction),
               mean_crash_avoidance = ifelse(is.na(mean_crash_avoidance), 0, mean_crash_avoidance))  
      }
    
    # Calculate sampling probabilities. ----
    prob <- calculate_sampling_scheme(unlabelled, 
                                      labelled, 
                                      sampling_method, 
                                      proposal_dist, 
                                      target,
                                      verbose)
    
    # Plot. ----
    if ( sampling_method == "active sampling" & plot & i %in% c(1, plot_iter) ) {
      
      unlabelled %>% 
        mutate(sampling_probability = prob$sampling_probability)%>%
        filter(caseID <= 42) %>% # Plot 42 cases on 7x6 grid. 
        ggplot() +
        geom_rect(aes(xmin = eoff - 0.05, xmax = eoff + 0.05, ymin = -acc - 0.25, ymax = -acc + 0.25, fill = sampling_probability)) +        
        scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) +
        labs(x = "OEOFF (s)",
             y = bquote('Maximal deceleration '(km/s^2)),
             fill = "Sampling probability") +
        facet_wrap(~caseID, ncol = 7, nrow = 6, labeller = labeller(caseID = function(x) "")) + 
        theme_classic() + 
        theme(panel.spacing = unit(0.1, "cm"), # Increase/decrease to increase/reduce horizontal white space between cases
              strip.background = element_blank(),
              strip.placement = "outside",
              strip.text = element_text(size = 0), # Change size to increase/reduce vertical white space between cases. Set to element_blank() for no white space.
              legend.direction = "horizontal",
              legend.position = "top",
              legend.key.height = unit(0.3, "cm"), # Change width and height as necessary.
              legend.key.width = unit(2, "cm")) +
        guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
      
      filename <- sprintf("Output/ActiveSamplingScheme_%s_2D_Iter%d.png", 
                          target %>% str_to_title() %>% str_remove_all(" "), 
                          i)
      ggsave(filename, width = 160, height = 100, unit = "mm", dpi = 1000)
      
      unlabelled %>% 
        mutate(sampling_probability = prob$sampling_probability)%>%
        filter(caseID <= 42) %>% # Plot 42 cases on 7x6 grid. 
        ggplot() +
        geom_point(aes(x = eoff, y = sampling_probability, colour = -acc)) +
        scale_colour_continuous(type = "viridis") +
        labs(x = "OEOFF (s)",
             y = "Sampling probability",
             colour = bquote('Maximal deceleration '(km/s^2))) +
        facet_wrap(~caseID, ncol = 7, nrow = 6, labeller = labeller(caseID = function(x) "")) + 
        theme_classic() + 
        theme(panel.spacing = unit(0.1, "cm"), # Increase/decrease to increase/reduce horizontal white space between cases
              strip.background = element_blank(),
              strip.placement = "outside",
              strip.text = element_text(size = 0), # Change size to increase/reduce vertical white space between cases. Set to element_blank() for no white space.
              legend.direction = "horizontal",
              legend.position = "top",
              legend.key.height = unit(0.3, "cm"), # Change width and height as necessary.
              legend.key.width = unit(2, "cm")) +
        guides(colour = guide_colourbar(title.position = "top", title.hjust = 0.5))
      
      filename <- sprintf("Output/ActiveSamplingScheme_%s_1D_Iter%d.png", 
                          target %>% str_to_title() %>% str_remove_all(" "), 
                          i)
      ggsave(filename, width = 160, height = 100, unit = "mm", dpi = 1000)
      
    }
    
    
    # Sample new instances according to multinomial distribution, stratified by case. ----
    nhits <- rep(0, nrow(unlabelled))
    for ( id in unique(unlabelled$caseID) ) {
      ix <- which(unlabelled$caseID == id)  
      nhits[ix] <- as.numeric(rmultinom(n = 1, size = n_per_case, prob = prob$sampling_probability[ix]))
    }
    
    # browser()
    # Get data for sampled observations.
    new_sample <- unlabelled %>% 
      mutate(batch_size = batch_size, 
             nhits = nhits,
             pi = prob$sampling_probability,
             mu = n_per_case * pi,
             sampling_weight = 1 / mu, 
             batch_weight = batch_size / nseq[i],
             final_weight = eoff_acc_prob * nhits * sampling_weight) %>% 
      filter(nhits > 0) %>% 
      dplyr::select(caseID, eoff, acc, eoff_acc_prob, sim_count0, sim_count1, iter, batch_size, nhits, pi, mu, sampling_weight, batch_weight, final_weight) %>% 
      mutate(iter = i) %>%
      left_join(data, by = c("caseID", "eoff", "acc", "eoff_acc_prob"))
    
 
    # Certainty selections.(Select already labelled instances with probability 1).
    # Only after first iteration, otherwise set to empty set.
    if ( i >  1 ) {
      certainty_selections <- labelled %>% 
        dplyr::select(-sim_count0, -sim_count1, -batch_size, -nhits, -pi, -mu, -sampling_weight, -batch_weight, -final_weight) %>% 
        filter(iter > 0) %>% 
        mutate(nhits = 1,
               pi = 1,
               mu = 1,
               sampling_weight = 1, 
               batch_weight = batch_size * (i - iter) / nseq[i],
               final_weight = eoff_acc_prob * batch_weight * nhits * sampling_weight)      
    } else {
      certainty_selections <- labelled %>% 
        filter(0 == 1)
    }

    
    # Update labelled set.
    labelled %<>% 
      mutate(batch_weight = batch_size / nseq[i]) %>% # Update batch-weights.
      add_row(new_sample) %>% # Add new sample.
      mutate(final_weight = eoff_acc_prob * batch_weight * nhits * sampling_weight) 

    
    # Remove labelled observations from unlabelled dataset.
    unlabelled %<>%
      filter(nhits == 0)

    
    # Estimate target quantities. ----
    
    # Estimate mean impact speed reduction, mean injury risk reduction, crash avoidance rate. 
    est <- estimate_targets(labelled %>% 
                              filter(is.na(max_impact0) | max_impact0 != 1) %>% 
                              add_row(certainty_selections) %>% 
                              filter(impact_speed0 > 0 & final_weight > 0), "final_weight")
    
    
    # Variance estimation using bootstrap method. 
    # If an element is selected multiple times: split into multiple observations/rows.
    ix <- rep(1:nrow(labelled), labelled$nhits) # To repeat rows.
    crashes <- labelled[ix, ] %>%
      mutate(final_weight = eoff_acc_prob * batch_weight * sampling_weight) %>%
      filter(is.na(max_impact0) | max_impact0 != 1) %>% 
      add_row(certainty_selections) %>% 
      filter(impact_speed0 > 0 & final_weight > 0)
    
    if ( nrow(crashes) > 0 ) { 
      boot <- boot(crashes, 
                   statistic = function(data, ix) c(unlist(estimate_targets(data[ix, ], weightvar = "final_weight")), unlist(estimate_targets_by_case(data[ix, ], weightvar = "final_weight", case_ID_sum)[, -c(1, 2)])),
                   R = nboot) 
      se <- apply(boot$t, 2 , sd, na.rm = TRUE) # Standard error of estimates.
    }  
    
    se_by_case <- matrix(nrow = n_cases, ncol = 3, se[-c(1:3)])
    se_by_case[se_by_case < 1e-12] <- NA # Too small values set to NA.
    se <- se[1:3]

    # Confidence intervals.
    lower <- est - qnorm(0.975) * se 
    upper <- est + qnorm(0.975) * se
    
    # Confidence intervals cover true value?
    CI_cover <- as.numeric(lower < ground_truth & ground_truth < upper)
    CI_cover[is.na(CI_cover)] <- 0

  
    # Append results. ----
    
    # Squared error from ground truth. 
    sqerr <- (est - ground_truth)^2 
 
    # Add names.
    names(se) <- paste0(names(est), "_se")
    names(CI_cover) <- paste0(names(est), "_ci_cover")
    names(sqerr) <- paste0(names(est), "_sqerr")

    newres <- tibble(sampling_method = sampling_method, # Meta-information.
                     proposal_dist = proposal_dist,
                     target = target,
                     use_logic = use_logic,
                     batch_size = batch_size) %>% 
      add_column(iter = i, # Iteration history.
                 neff0 = nseq0[i], 
                 neff1 = nseq1[i], 
                 neff_tot = nseq0[i] + nseq1[i],
                 nsim0 = ifelse(use_logic, sum(labelled$sim_count0), sum(labelled$sim_count0 * labelled$nhits)), 
                 nsim1 = ifelse(use_logic, sum(labelled$sim_count1), sum(labelled$sim_count1 * labelled$nhits))) %>%
      mutate(nsim_tot = nsim0 + nsim1) %>% 
      add_column(as_tibble(as.list(est))) %>% # Estimates.
      add_column(as_tibble(as.list(sqerr))) %>% # Squared errors.
      add_column(as_tibble(as.list(se)))  %>% # Standard errors.
      add_column(as_tibble(as.list(CI_cover)))# Confidence interval coverage.

    
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
  