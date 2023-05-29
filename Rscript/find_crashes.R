find_crashes <- function(new_sample, unlabelled) {
  
  unlabelled$crash0 <- 0
  unlabelled$crash1 <- 0
  
  for ( i in unique(new_sample$caseID) ) { # Iterate over all cases.
   
    # New data for current case.
    labelled_i <- new_sample %>% 
      filter(caseID == i)
    
    for ( j in 1:nrow(labelled_i) ) { # Iterative over all labelled data points.
      
      newx <- labelled_i[j, ]
      
      if ( newx$impact_speed0 > 0 ) { # Find certainty crashes, baseline scenario.
        ix <- with(unlabelled, which(caseID == i & eoff >= newx$eoff & acc >= newx$acc))
        
        unlabelled %<>% 
          mutate(crash0 = ifelse(row_number() %in% ix, 1, crash0 ))
        
      }
      
      if ( newx$impact_speed1 > 0 ) { # Find certainty crashes, with counter measure.
        ix <- with(unlabelled, which(caseID == i & eoff >= newx$eoff & acc >= newx$acc))
        
        unlabelled %<>% 
          mutate(crash1 = ifelse(row_number() %in% ix, 1, crash1 ))
        
      }
    }
  }
  
  ix0 <- which(unlabelled$crash0 == 1)
  ix1 <- which(unlabelled$crash1 == 1)
  
  return(list(crashes0 = ix0, crashes1 = ix1))
  
}