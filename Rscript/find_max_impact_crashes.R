find_max_impact_crashes <- function(new_sample, maximpact, unlabelled) {
  
  unlabelled$max_impact0 <- 0

  for ( i in unique(new_sample$caseID) ) { # Iterate over all cases.
    
    # Get max impact speed for current case.
    max <- maximpact %>% 
      filter(caseID == i) 

    # New data for current case.
    labelled_i <- new_sample %>% 
      filter(caseID == i)
    
    for ( j in 1:nrow(labelled_i) ) { # Iterative over all labelled data points.
      
      newx <- labelled_i[j, ]
      
      if ( newx$impact_speed0 == max$impact_speed_max0 ) { # Find baseline max impact speed scenarios.
        ix <- with(unlabelled, which(caseID == i & eoff >= newx$eoff & acc >= newx$acc))
        
        unlabelled %<>% 
          mutate(max_impact0 = ifelse(row_number() %in% ix, 1, max_impact0 ))
        
      }
      
    }
  }
  
  ix0 <- which(unlabelled$max_impact0 == 1)

  return(list(max_impact_crashes0 = ix0))
  
}