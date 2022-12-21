# short explaination for the code 
# batch_size is limited by the real case number
library("readxl")
source("Rscript/active_sampling.R")
source("Rscript/sim_output.R")
load("Data/glance_dec_data_ttc_AEB_1_25_ttc.R")
sampling_input <- read_excel("Data/Sampling_input_application1.xlsx")
param_input <- read_excel("Data/param_input.xlsx")
add_effn = 2000
nburnin = 0
verbose = FALSE

niter = ceiling(add_effn/param_input$batch_size)
res_total= replicate(length(param_input$Sim_n), data.frame())

for(j in 1:length(param_input$Sim_n)){
  inputparameter <- data.frame(param_input$batch_size[j],niter[j], nburnin,param_input$nboot[j],verbose,param_input$Sim_n[j])
  colnames(inputparameter) <- c("batch_size","niter", "nburnin","nboot","verbose","Sim_n")
  res_top_loop = replicate(tail(sampling_input$sim_order, 1), data.frame())
  
  for(i in 1:tail(sampling_input$sim_order, 1)){
    res_in_loop <- sim_output(df,sampling_input[i,],inputparameter)
    print(paste("sampling method",i,"is finished"))
    res_top_loop[[i]] <- res_in_loop
  }
  print(paste("loop",j,"is finished"))
  res_total[[j]] <- res_top_loop
}  

save(res_total, file = "Data/New_sim_results210.R")

