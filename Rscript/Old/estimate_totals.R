estimate_totals <- function(data, weightvar = NULL) {
  
  data$w <- unlist(data[, weightvar])
 
  total_impact_speed_reduction <- with(data, sum( w * impact_speed_reduction) ) 
  total_injury_risk_reduction <- with(data, sum( w * injury_risk_reduction) ) 
  total_crash_avoidance <- with(data, sum( w * crash_avoidance ) ) 
  total_baseline_crashes <- with(data, sum( w * (impact_speed0 > 0)) ) 
  
  return(c("total_impact_speed_reduction" = total_impact_speed_reduction, 
           "total_injury_risk_reduction" = total_injury_risk_reduction, 
           "total_crash_avoidance" = total_crash_avoidance, 
           "total_baseline_crashes" = total_baseline_crashes))
}