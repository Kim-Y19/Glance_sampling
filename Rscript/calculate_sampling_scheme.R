# Calculate sampling scheme.
calculate_sampling_scheme <- function(unlabelled,
                                      labelled,
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
                                      est = NULL,
                                      r2 = NULL) {


  # Calculate 'size' of pps (probability proportional to size) sampling. ----
  if ( sampling_method == "simple random sampling" ) { 
    
    size <- rep(1, nrow(unlabelled))
    
  } else if ( sampling_method == "importance sampling" ) {
    
    if ( proposal_dist == "density sampling" ) { # Density sampling.
      
      size <- unlabelled$eoff_acc_prob
      
    } else if ( proposal_dist == "severity sampling" ) { # Severity sampling.
      
      # Severity is assumed to increase with increase in EOFF, deceleration and 
      # maximal impact speed under baseline scenario. 
      eoff_std <- with(unlabelled, eoff - min(eoff))
      eoff_std <- 0.1 + 0.9 * eoff_std / max(eoff_std)
      acc_std <- with(unlabelled, acc - min(acc))
      acc_std <- 0.1 + 0.9 * acc_std / max(acc_std)
      impact_speed_max0_std <- with(unlabelled, impact_speed_max0 - min(impact_speed_max0))
      impact_speed_max0_std <- 0.1 + 0.9 * impact_speed_max0_std / max(impact_speed_max0_std)
      size <- unlabelled$eoff_acc_prob * eoff_std * acc_std * impact_speed_max0_std

    } 
    
  } else if ( sampling_method == "active sampling" ) {
    
    # Baseline crash probability.
    # If prediction accuracy is missing or negative: set all equal.
    if ( is.na(r2$accuracy_crash0) | r2$accuracy_crash0 < 0 ) {  
      collision_prob0_pred <- 1
    } else {
      collision_prob0_pred <- unlabelled$collision_prob0_pred
    }

    if ( target == "impact speed reduction" ) {
      
      r2 <- r2$impact_speed_reduction
      mu <- est$mean_impact_speed_reduction
      pred <- unlabelled$impact_speed_reduction_pred
      sigma <- unlabelled$sigma_impact_speed_reduction

    } else if ( target == "injury risk reduction" ) {
      
      r2 <- r2$injury_risk_reduction
      mu <- est$mean_injury_risk_reduction
      pred <- unlabelled$injury_risk_reduction_pred
      sigma <- unlabelled$sigma_injury_risk_reduction
      
    } else if ( target == "crash avoidance" ) {
      
      r2 <- r2$accuracy_crash1
      mu <- 1 - est$crash_avoidance_rate
      pred <- unlabelled$collision_prob1_pred
      sigma <- unlabelled$sigma_collision1
      
    } 
  
    # If prediction R-squared/accuracy is missing or negative: set all equal.
    if ( is.na(r2) | r2 < 0 ) {
      mu <- 0
      pred <- 1
      sigma <- 0
    }

    size <- unlabelled$eoff_acc_prob * sqrt(collision_prob0_pred * ((pred - mu)^2 + sigma^2))

  } 
  
  # If there are any NAs: use density importance sampling.
  if ( any(is.na(size)) ) {
    size <- unlabelled$eoff_acc_prob
  }
  
  # Probability proportional to size.
  sampling_probability <- size / sum(size)
  
  
  # Return.
  return(list(size = size, sampling_probability = sampling_probability))
  
}