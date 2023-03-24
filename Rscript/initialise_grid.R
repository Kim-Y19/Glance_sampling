initialise_grid <- function(data, grid) {
  
  # Labelled dataset.
  labelled <- data %>% 
    right_join(grid, by = c("eoff", "acc")) %>%
    mutate(sim_count0 = 1, 
           sim_count1 = 1, 
           iter = 0,
           batch_size = 0,
           batch_weight = 1,
           pi = 1,
           mu = 1,
           nhits = 1,
           sampling_weight = 1,
           final_weight = eoff_acc_prob * nhits * sampling_weight)
  
  
  # Unlabelled dataset.
  n <- nrow(data)
  unlabelled <- data %>% 
    mutate(pred_collision0 = 1, 
           pred_collision1 = 0, 
           pred_impact_speed_reduction = 0,
           pred_injury_risk_reduction = 0,
           sigma_impact_speed_reduction = 1,
           sigma_injury_risk_reduction = 1,
           sigma_collision1 = 1,
           mean_impact_speed_reduction = 0,
           mean_injury_risk_reduction = 0,
           mean_crash_avoidance = 0,
           crash0 = NA_integer_,
           crash1 = NA_integer_,
           non_crash0 = NA_integer_,
           non_crash1 = NA_integer_,
           max_impact0 = NA_integer_,
           max_impact1 = NA_integer_,
           sim_count0 = 1,
           sim_count1 = 1,
           iter = NA) 
  
  return(list(labelled = labelled, unlabelled = unlabelled))
}
