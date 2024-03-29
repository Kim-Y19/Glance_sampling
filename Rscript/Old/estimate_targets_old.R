estimate_targets <- function(data, weightvar = NULL) {
  
  data$w <- data[, weightvar]
  
  if( nrow(data) > 0 ){
    
    crashes <- data %>% filter(impact_speed0 > 0 & w > 0)
    
    mean_impact_speed0 <- with(crashes, sum(w * (impact_speed0)) / sum(w))
    mean_impact_speed1 <- with(crashes, sum(w * (impact_speed1)) / sum(w))
    mean_impact_speed_reduction <- mean_impact_speed0 - mean_impact_speed1
    percent_impact_speed_reduction <- 100 * mean_impact_speed1 / mean_impact_speed0
    mean_injury_risk0 <- with(crashes, sum(w * (injury_risk0)) / sum(w))
    mean_injury_risk1 <- with(crashes, sum(w * (injury_risk1)) / sum(w))
    mean_injury_risk_reduction <- mean_injury_risk0 - mean_injury_risk1
    percent_injury_risk_reduction <- 100 * mean_injury_risk1 / mean_injury_risk0
    percent_crashes0 <- 100 * with(data %>% filter(w > 0), sum(w * (impact_speed0 > 0)) / sum(w))
    percent_crashes1 <- 100 * with(data %>% filter(w > 0), sum(w * (impact_speed1 > 0)) / sum(w))
    crash_avoidance_rate <- 1 - with(crashes, sum(w * (impact_speed1 > 0)) / sum(w))
    
  } else {
    
    mean_impact_speed0 <- NA
    mean_impact_speed1 <- NA
    mean_impact_speed_reduction <- mean_impact_speed0 - mean_impact_speed1
    percent_impact_speed_reduction <- 100 * mean_impact_speed1 / mean_impact_speed0
    mean_injury_risk0 <- NA
    mean_injury_risk1 <- NA
    mean_injury_risk_reduction <- mean_injury_risk0 - mean_injury_risk1
    percent_injury_risk_reduction <- 100 * mean_injury_risk1 / mean_injury_risk0
    percent_crashes0 <- NA
    percent_crashes1 <- NA
    crash_avoidance_rate <- NA
    
  }
  
  return(c("mean_impact_speed0" = mean_impact_speed0, 
           "mean_impact_speed1" = mean_impact_speed1, 
           "mean_impact_speed_reduction" = mean_impact_speed_reduction, 
           "percent_impact_speed_reduction" = percent_impact_speed_reduction, 
           "mean_injury_risk0" = mean_injury_risk0, 
           "mean_injury_risk1" = mean_injury_risk1, 
           "mean_injury_risk_reduction" = mean_injury_risk_reduction, 
           "percent_injury_risk_reduction" = percent_injury_risk_reduction, 
           "percent_crashes0" = percent_crashes0,
           "percent_crashes1" = percent_crashes1,
           "crash_avoidance_rate" = crash_avoidance_rate))
}