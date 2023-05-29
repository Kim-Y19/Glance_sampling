find_non_crashes <- function(new_sample, unlabelled) {
  
  unlabelled$non_crash0 <- 0
  unlabelled$non_crash1 <- 0

  for ( i in unique(new_sample$caseID) ) { # Iterate over all cases.
    
     # New data for current case.
    labelled_i <- new_sample %>% 
      filter(caseID == i)
    
    for ( j in 1:nrow(labelled_i) ) { # Iterative over all labelled data points.
      
      newx <- labelled_i[j, ]
      
      if ( newx$impact_speed0 == 0 ) { # Find certainty non-crashes, baseline scenario.
        ix <- with(unlabelled, which(caseID == i & eoff <= newx$eoff & acc <= newx$acc))
        
        unlabelled %<>% 
          mutate(non_crash0 = ifelse(row_number() %in% ix, 1, non_crash0 ))
        
      }
      
      if ( newx$impact_speed1 == 0 ) { # Find certainty non-crashes, with counter measure.
        ix <- with(unlabelled, which(caseID == i & eoff <= newx$eoff & acc <= newx$acc))
        
        unlabelled %<>% 
          mutate(non_crash1 = ifelse(row_number() %in% ix, 1, non_crash1 ))
        
      }
    }
  }
  
  ix0 <- which(unlabelled$non_crash0 == 1)
  ix1 <- which(unlabelled$non_crash1 == 1)
  
  return(list(non_crashes0 = ix0, non_crashes1 = ix1))
  
}
