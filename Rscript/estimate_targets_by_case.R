estimate_targets_by_case <- function(data, weightvar = NULL, ncases = NULL) {
  
  if ( !is.null(weightvar) ) {
    
    data$w <- unlist(data[, weightvar])
    
    denom <- with(data, tapply(w * (impact_speed0 > 0), caseID, sum))
    n <- with(data, tapply(impact_speed0 > 0, caseID, sum))
    mean_impact_speed_reduction <- with(data, tapply(w * (impact_speed0 > 0) * (impact_speed_reduction), caseID, sum)) / denom
    mean_injury_risk_reduction <- with(data, tapply(w * (impact_speed0 > 0) * (injury_risk_reduction), caseID, sum)) / denom
    mean_crash_avoidance <- 1 - with(data, tapply(w * (impact_speed0 > 0) * (impact_speed1 > 0), caseID, sum)) / denom
    
  } else {
    
    mean_impact_speed_reduction <- mean_injury_risk_reduction <- mean_crash_avoidance <- NA
    
  }
  
  n_final <- rep(0, ncases)
  denom_final <- misr_final <- mirr_final <- mca_final <- rep(NA, ncases)
  names(n_final) <- names(denom_final) <- names(misr_final) <- names(mirr_final) <- names(mca_final) <- 1:ncases
  
  n_final[intersect(1:ncases, names(n))] <- n
  denom_final[intersect(1:ncases, names(n))] <- denom
  misr_final[intersect(1:ncases, names(n))] <- mean_impact_speed_reduction
  mirr_final[intersect(1:ncases, names(n))] <- mean_injury_risk_reduction
  mca_final[intersect(1:ncases, names(n))] <- mean_crash_avoidance
  
  
  return(tibble(caseID = as.character(names(n_final)),
                n = n_final,
                mean_impact_speed_reduction = misr_final,
                mean_injury_risk_reduction = mirr_final, 
                mean_crash_avoidance = mca_final))
}