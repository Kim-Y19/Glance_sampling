estimate_targets <- function(data, weightvar = NULL) {
  
  if ( !is.null(weightvar) ) {

    data$w <- unlist(data[, weightvar])
    
    # denom <- with(data, tapply(w * (impact_speed0 > 0), caseID, sum))
    # mean_impact_speed_reduction <- mean(with(data, tapply(w * (impact_speed0 > 0) * (impact_speed_reduction), caseID, sum)) / denom)
    # mean_injury_risk_reduction <- mean(with(data, tapply(w * (impact_speed0 > 0) * (injury_risk_reduction), caseID, sum)) / denom)
    # crash_avoidance_rate <- mean(1 - with(data, tapply(w * (impact_speed0 > 0) * (impact_speed1 > 0), caseID, sum)) / denom)

    res <- data %>%
      group_by(caseID) %>%
      summarise(mean_impact_speed_reduction = sum( w * (impact_speed0 > 0) * (impact_speed_reduction) ) / sum (w * (impact_speed0 > 0)),
                mean_injury_risk_reduction = sum( w * (impact_speed0 > 0) * (injury_risk_reduction) ) / sum(w * (impact_speed0 > 0)),
                crash_avoidance_rate = 1 - sum( w * (impact_speed0 > 0) * (impact_speed1 > 0) ) / sum(w * (impact_speed0 > 0)),
                .groups = "drop") %>%
      ungroup() %>%
      dplyr::select(-caseID) %>%
      summarise_all(mean)

    mean_impact_speed_reduction <- res$mean_impact_speed_reduction
    mean_injury_risk_reduction <- res$mean_injury_risk_reduction
    crash_avoidance_rate <- res$crash_avoidance_rate
    
  } else {
    
    mean_impact_speed_reduction <- mean_injury_risk_reduction <- crash_avoidance_rate <- NA
    
  }
  

  return(c("mean_impact_speed_reduction" = mean_impact_speed_reduction, 
           "mean_injury_risk_reduction" = mean_injury_risk_reduction, 
           "crash_avoidance_rate" = crash_avoidance_rate))
}
