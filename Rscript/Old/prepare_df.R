prepare_df <- function(loadfile, glance, dec, plotit = TRUE){
  df_with_y =  read_pickle_file(loadfile)
  drops <- c("t")
  df_with_y = df_with_y[ , !(names(df_with_y) %in% drops)]
  df_with_y = df_with_y[!duplicated(df_with_y),]
  transform(df_with_y,acc = as.numeric(acc), eoff = as.numeric(eoff),
            eoff_acc_prob = as.numeric(eoff_acc_prob),impact_speed = as.numeric(impact_speed))
  df_with_y$impact_speed = as.numeric(df_with_y$impact_speed)
  for ( i in unique(df_with_y$caseID) ){ # Iterate over all cases.
    labelled = df_with_y[df_with_y$caseID == i,]
    for ( j in unique(labelled$acc) ) {
      labelled_dec = labelled[labelled$acc == j,]
      labelled_dec = labelled_dec[order(labelled_dec$eoff),]
      dec_weight = dec$dec_weight[which(dec$dec == j)] 
      if (length(labelled_dec[labelled_dec$impact_speed == 0,]$eoff)>0)
      {
        min_index = max(labelled_dec[labelled_dec$impact_speed == 0,]$eoff)
        min_index = which(glance$glance_duration == min_index)
      }
      else
      {
        min_index = 0
      }
      if (length(labelled_dec[labelled_dec$impact_speed == labelled_dec[nrow(labelled_dec)-1,]$impact_speed,]$eoff)>0 &
          labelled_dec[nrow(labelled_dec),]$impact_speed != 0)
      {
        max_index = min(labelled_dec[labelled_dec$impact_speed == labelled_dec[nrow(labelled_dec),]$impact_speed,]$eoff)
        max_index = which(glance$glance_duration == max_index)
      }
      else{
        max_index = 0
      }
      max_v = tail(labelled_dec$impact_speed, n =1)
      if (min_index > 2){
        for (k in 2:(min_index-1)){
          if(length(which(labelled_dec$eoff == glance$glance_duration[k]))==0){
            df_with_y[nrow(df_with_y) + 1,] = c(i,
                                                j,
                                                glance$glance_duration[k],
                                                glance$glance_weight[k]*dec_weight,
                                                
                                                0)
          }
          
        }
      }
      if (max_index < length(glance$glance_duration) & max_index > 0) {
        for (k in (max_index +1):length(glance$glance_duration)){
          if(length(which(labelled_dec$eoff == glance$glance_duration[k])) == 0){
            df_with_y[nrow(df_with_y) + 1,] = c(i,j,
                                                glance$glance_duration[k],
                                                glance$glance_weight[k]*dec_weight,
                                                
                                                max_v)
          }
        }
      }
      if(min(df_with_y[df_with_y$caseID == i & df_with_y$acc == j & df_with_y$eoff == tail(glance$glance_duration,1),]$impact_speed) == 0){
        df_with_y[df_with_y$caseID == i & df_with_y$acc == j,]$impact_speed = 0
      }
      if(plotit){
        plot(as.numeric(df_with_y[df_with_y$caseID == i,]$eoff),df_with_y[df_with_y$caseID == i,]$acc,col = 'red',
             main = c('CasecaseID:',i),
             xlab = 'Glance duration',
             ylab = 'Deceleration',
             xlim = c(0,6.5))
      }
    }
  }
  df_with_y = df_with_y[!duplicated(df_with_y),]
  df_with_y = df_with_y[order(df_with_y$caseID,df_with_y$acc,df_with_y$eoff),]
  for ( i in unique(df_with_y$caseID) ) { 
    labelled = df_with_y[df_with_y$caseID == i,]
    for ( j in unique(labelled$acc) ) { 
      labelled_dec = labelled[labelled$acc == j,]
      labelled_dec = labelled_dec[order(labelled_dec$eoff),]
      glance_sum = sum(glance$glance_weight[-1])
      df_with_y[(df_with_y$caseID == i & df_with_y$acc == j),]$eoff_acc_prob = as.numeric(df_with_y[df_with_y$caseID == i & df_with_y$acc == j,]$eoff_acc_prob)/(1-glance$glance_weight[1])
      sum(as.numeric(df_with_y[(df_with_y$caseID == i & df_with_y$acc == j),]$eoff_acc_prob))
    }
    sum(as.numeric(df_with_y[df_with_y$caseID == i,]$eoff_acc_prob))
  }
  transform(df_with_y,acc = as.numeric(acc), eoff = as.numeric(eoff),
            eoff_acc_prob = as.numeric(eoff_acc_prob),impact_speed = as.numeric(impact_speed))
  df_with_y$acc = as.numeric(df_with_y$acc)
  df_with_y$eoff = as.numeric(df_with_y$eoff)
  df_with_y$impact_speed = as.numeric(df_with_y$impact_speed)
  df_with_y$eoff_acc_prob = as.numeric(df_with_y$eoff_acc_prob)
  return(df_with_y)
}