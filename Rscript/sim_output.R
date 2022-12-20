sim_output <- function(df,input,inputparameter){
  niter = inputparameter$niter
  batch_size = inputparameter$batch_size
  Sim_n = inputparameter$Sim_n
  target = input$target
  proposal_dist = input$proposal_dist
  sampling_method = input$sampling_method
  group = input$group
  use_logic = input$use_logic
  opt_method = input$opt_method
  res_list <- replicate(Sim_n, data.frame())
  labelled_list <- replicate(Sim_n, data.frame())
  crashes_list<- replicate(Sim_n, data.frame())
  verbose = inputparameter$verbose
  nboot = inputparameter$nboot
  
  for (k in 1:Sim_n){
    print(paste("simulation",k,"start,","reduced logic is",input$use_logic,", number per iteration:",batch_size,",total iteration:",niter))
    set.seed(k)
    out <- active_sampling (df, sampling_method, 
                            proposal_dist,
                            target,
                            use_logic, # TRUE or FALSE 
                            batch_size,
                            niter,
                            nboot,
                            verbose,
                            plot = FALSE)
    #print(out$results$neff0)
    res_list[[k]] <- out$results
    labelled_list[[k]] <- out$labelled
    crashes_list[[k]] <- out$crashes
  }
  res = do.call(rbind, res_list)
  labelled = do.call(rbind, labelled_list)
  crashes = do.call(rbind, crashes_list)
  
  aver = as.data.frame(matrix(nrow=niter,ncol=0))
  biggest = as.data.frame(matrix(nrow=niter,ncol=0))
  smallest = as.data.frame(matrix(nrow=niter,ncol=0))
  index = matrix(nrow = niter,ncol = Sim_n)
  for(p in 1:niter){
    aver$group[p] = group
    aver$opt_method[p] = opt_method
    aver$n_crashes[p] = (mean(res$n_crashes[index[p,]]))
    aver$use_logic[p] = use_logic
    index[p,] = seq(p,niter*Sim_n,niter)
    aver$neff0[p] = mean(res$neff0[index[p,]])
    aver$neff_tot[p] = mean(res$neff_tot[index[p,]])
    aver$nsim_tot[p] = mean(res$nsim_tot[index[p,]])
    aver$nsim0[p] = mean(res$nsim0[index[p,]])
    aver$nsim1[p] = mean(res$nsim1[index[p,]])
    aver$iter[p] = mean(res$iter[p])
    aver$mean_impact_speed0_sqerr[p] = sqrt(mean(res$mean_impact_speed0_sqerr[index[p,]]))
    aver$mean_impact_speed_reduction_sqerr[p] = sqrt(mean(res$mean_impact_speed_reduction_sqerr[index[p,]]))
    aver$crash_avoidance_rate_sqerr[p] = sqrt(mean(res$crash_avoidance_rate_sqerr[index[p,]]))
    aver$mean_injury_risk_reduction_sqerr[p] = sqrt(mean(res$mean_injury_risk_reduction_sqerr[index[p,]]))
    aver$impact_speed0_KLdiv_sqerr[p] = sqrt(mean(res$impact_speed0_KLdiv[index[p,]]))
    aver$impact_speed0_KLdiv[p] = mean(res$impact_speed0_KLdiv[index[p,]])
    
    aver$mean_impact_speed0_se_boot[p] = mean(res$mean_impact_speed0_se_boot[index[p,]])
    aver$mean_impact_speed_reduction_se_boot[p] = mean(res$mean_impact_speed_reduction_se_boot[index[p,]])
    aver$crash_avoidance_rate_se_boot[p] = mean(res$crash_avoidance_rate_se_boot[index[p,]])
    aver$mean_injury_risk_reduction_se_boot[p] = mean(res$mean_injury_risk_reduction_se_boot[index[p,]])
    
    aver$mean_impact_speed0_se_classic[p] = mean(res$mean_impact_speed0_se_classic[index[p,]])
    aver$mean_impact_speed_reduction_se_classic[p] = mean(res$mean_impact_speed_reduction_se_classic[index[p,]])
    aver$crash_avoidance_rate_se_classic[p] = mean(res$crash_avoidance_rate_se_classic[index[p,]])
    aver$mean_injury_risk_reduction_se_classic[p] = mean(res$mean_injury_risk_reduction_se_classic[index[p,]])
    
    aver$mean_impact_speed0_se_mart[p] = mean(res$mean_impact_speed0_se_mart[index[p,]])
    aver$mean_impact_speed_reduction_se_mart[p] = mean(res$mean_impact_speed_reduction_se_mart[index[p,]])
    aver$crash_avoidance_rate_se_mart[p] = mean(res$crash_avoidance_rate_se_mart[index[p,]])
    aver$mean_injury_risk_reduction_se_mart[p] = mean(res$mean_injury_risk_reduction_se_mart[index[p,]])
    
    
    aver$mean_impact_speed_reduction_ci_cover_boot[p] = sum(res$mean_impact_speed_reduction_ci_cover_boot[index[p,]])/Sim_n
    aver$crash_avoidance_rate_ci_cover_boot[p] = sum(res$crash_avoidance_rate_ci_cover_boot[index[p,]])/Sim_n
    aver$mean_injury_risk_reduction_ci_cover_boot[p] = sum(res$mean_injury_risk_reduction_ci_cover_boot[index[p,]])/Sim_n
    
    aver$mean_impact_speed_reduction_ci_cover_classic[p] = sum(res$mean_impact_speed_reduction_ci_cover_classic[index[p,]])/Sim_n
    aver$crash_avoidance_rate_ci_cover_classic[p] = sum(res$crash_avoidance_rate_ci_cover_classic[index[p,]])/Sim_n
    aver$mean_injury_risk_reduction_ci_cover_classic[p] = sum(res$mean_injury_risk_reduction_ci_cover_classic[index[p,]])/Sim_n
    
    aver$mean_impact_speed_reduction_ci_cover_mart[p] = sum(res$mean_impact_speed_reduction_ci_cover_mart[index[p,]])/Sim_n
    aver$crash_avoidance_rate_ci_cover_mart[p] = sum(res$crash_avoidance_rate_ci_cover_mart[index[p,]])/Sim_n
    aver$mean_injury_risk_reduction_ci_cover_mart[p] = sum(res$mean_injury_risk_reduction_ci_cover_mart[index[p,]])/Sim_n
    
    
    
    aver$accuracy_crash0[p] = mean(res$accuracy_crash0[index[p,]])
    aver$accuracy_crash1[p] = mean(res$accuracy_crash1[index[p,]])
    
    aver$r2_impact_speed0[p] = mean(res$r2_impact_speed0[index[p,]])
    aver$r2_impact_speed_reduction[p] = mean(res$r2_impact_speed_reduction[index[p,]])
    aver$r2_crash_avoidance_rate[p] = mean(res$r2_crash_avoidance_rate[index[p,]])
    aver$r2_injury_risk_reduction[p] = mean(res$r2_injury_risk_reduction[index[p,]])
    
    #######
    biggest$group[p] = group
    biggest$use_logic[p] = use_logic
    biggest$iter[p] = mean(res$iter[p])
    biggest$neff0[p] = max(res$neff0[index[p,]])
    biggest$mean_impact_speed0_sqerr[p] = sqrt(max(res$mean_impact_speed0_sqerr[index[p,]]))
    biggest$mean_impact_speed_reduction_sqerr[p] = sqrt(max(res$mean_impact_speed_reduction_sqerr[index[p,]]))
    biggest$crash_avoidance_rate_sqerr[p] = sqrt(max(res$crash_avoidance_rate_sqerr[index[p,]]))
    biggest$mean_injury_risk_reduction_sqerr[p] = sqrt(max(res$mean_injury_risk_reduction_sqerr[index[p,]]))
    
    
    smallest$group[p] = group
    smallest$use_logic[p] = use_logic
    smallest$iter[p] = mean(res$iter[p])
    smallest$neff0[p] = min(res$neff0[index[p,]])
    smallest$mean_impact_speed0_sqerr[p] = sqrt(min(res$mean_impact_speed0_sqerr[index[p,]]))
    smallest$mean_impact_speed_reduction_sqerr[p] = sqrt(min(res$mean_impact_speed_reduction_sqerr[index[p,]]))
    smallest$crash_avoidance_rate_sqerr[p] = sqrt(min(res$crash_avoidance_rate_sqerr[index[p,]]))
    smallest$mean_injury_risk_reduction_sqerr[p] = sqrt(min(res$mean_injury_risk_reduction_sqerr[index[p,]]))
    
  }
  return(list(aver = aver, 
              biggest = biggest, 
              smallest = smallest,
              res = res,
              labelled = labelled,
              crashes = crashes))
  
}