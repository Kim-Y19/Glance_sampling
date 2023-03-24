estimate_targets_by_case <- function(data, weightvar = NULL) {
  
  if ( !is.null(weightvar) ) {
    
    data$w <- unlist(data[, weightvar])
    
    denom <- with(data, tapply(w * (impact_speed0 > 0), caseID, sum))
    mean_impact_speed_reduction <- with(data, tapply(w * (impact_speed0 > 0) * (impact_speed_reduction), caseID, sum)) / denom
    mean_injury_risk_reduction <- with(data, tapply(w * (impact_speed0 > 0) * (injury_risk_reduction), caseID, sum)) / denom
    mean_crash_avoidance <- 1 - with(data, tapply(w * (impact_speed0 > 0) * (impact_speed1 > 0), caseID, sum)) / denom
    
  } else {
    
    mean_impact_speed_reduction <- mean_injury_risk_reduction <- mean_crash_avoidance <- NA
    
  }
  
  return(tibble(caseID = as.numeric(names(denom)),
                mean_impact_speed_reduction = mean_impact_speed_reduction,
                mean_injury_risk_reduction = mean_injury_risk_reduction, 
                mean_crash_avoidance = mean_crash_avoidance))
}