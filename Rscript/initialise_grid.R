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
    mutate(collision_prob0_pred = NA_real_, 
           collision_prob1_pred = NA_real_, 
           impact_speed0_pred = NA_real_, 
           impact_speed_reduction_pred = NA_real_,
           injury_risk_reduction_pred = NA_real_,
           sigma_impact_speed_reduction = NA_real_,
           sigma_injury_risk_reduction = NA_real_,
           sigma_collision1 = NA_real_,
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
