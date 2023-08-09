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
                                      verbose = FALSE) {


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

    if ( target == "impact speed reduction" ) {
      
      mu <- unlabelled$mean_impact_speed_reduction
      pred <- unlabelled$pred_impact_speed_reduction
      sigma <- unlabelled$sigma_impact_speed_reduction
      se_est <- unlabelled$se_mean_impact_speed_reduction

    } else if ( target == "injury risk reduction" ) {
      
      mu <- unlabelled$mean_injury_risk_reduction
      pred <- unlabelled$pred_injury_risk_reduction
      sigma <- unlabelled$sigma_injury_risk_reduction
      se_est <- unlabelled$se_mean_injury_risk_reduction
      
    } else if ( target == "crash avoidance" ) {
      
      mu <- 1 - unlabelled$mean_crash_avoidance
      pred <- unlabelled$pred_collision1
      sigma <- unlabelled$sigma_collision1
      se_est <- unlabelled$se_mean_crash_avoidance
      
    } 
     
    p <- unlabelled$eoff_acc_prob
    q <- unlabelled$pred_collision0
    size <- p * sqrt(q * ((pred - mu)^2 + sigma^2 + se_est^2))

  } 

  # Within each case: sampling probability proportional to size.
  sampling_probability <- rep(NA, nrow(unlabelled))
  for ( id in unique(unlabelled$caseID) ) {
    ix <- which(unlabelled$caseID == id)  
    
    # If there are any NAs: use density importance sampling.
    if ( any(is.na(size[ix])) ) {
      size[ix] <- unlabelled$eoff_acc_prob[ix]
      # if ( verbose ) {
      #   print("NAs in size vector found. Using density importance sampling.")
      # }
    }
    
    sampling_probability[ix] <- size[ix] / sum(size[ix])
  }

  # Return.
  return(list(size = size, sampling_probability = sampling_probability))
  
}