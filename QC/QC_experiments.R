################################################################################
#
# QC_experiments.R
#
################################################################################

# Clean-up.
rm(list = ls())
gc()

# Load packages.
library("bigstatsr")
library("doParallel")
library("foreach")
library("magrittr")
library("parallel")
library("tictoc")
library("tidyverse")


# ggplot settings.
ptsize <- 10
theme_set(theme_bw()) 
theme_update(axis.text = element_text(size = ptsize, colour = "black", family = "serif"),
             axis.line = element_line(colour = "black", linewidth = 0.25), 
             axis.ticks = element_line(colour = "black", linewidth = 0.25), 
             axis.title.y = element_text(margin = margin(t = 0, r = 0.0, b = 0, l = 0.2, unit = 'cm')),
             legend.key.width = unit(1, "cm"),
             legend.key.height = unit(0.1, "cm"),
             legend.margin = margin(0, 0, 0, 0),
             legend.spacing.y = unit(0.15, 'cm'),
             legend.position = "right",
             legend.text = element_text(size = ptsize, colour = "black", family = "serif"),
             legend.title = element_text(size = ptsize, colour = "black", family = "serif"),
             strip.background.x = element_blank(),
             panel.border = element_blank(),
             panel.grid = element_blank(),  
             plot.subtitle = element_text(size = ptsize, colour = "black", family = "serif", face = "plain", hjust = 0),
             plot.title = element_text(size = ptsize, colour = "black", family = "serif", face = "plain", hjust = 0),
             text = element_text(size = ptsize, colour = "black", family = "serif"))

update_geom_defaults("line", list(linewidth = 0.25))
update_geom_defaults("text", list(size = ptsize / .pt, family = "serif"))


# Load data.
load("QC/Testdata.R")


# Load functions.
source("Rscript/active_sampling.R")


# Experiment parameters.
# 10 reps and 10 iterations in 1000 sec (17 min).
# ~14 hours to run 100 reps with 50 iterations.
# ~3 days to run 500 reps with 50 iterations.
tic()
nreps <- 100
niter <- 50
params <- crossing(sampling_method = c("simple random sampling", 
                                       "importance sampling", 
                                       "active sampling"),
                   proposal_dist = c("density sampling", 
                                     "severity sampling"),
                   target = c("impact speed reduction",
                              "injury risk reduction", 
                              "crash avoidance"),
                   use_logic = c(TRUE, FALSE)) %>% 
  mutate(proposal_dist = ifelse(sampling_method != "importance sampling", "NA", proposal_dist), 
         target = ifelse(sampling_method != "active sampling", "NA", target)) %>% 
  filter(!duplicated(.))


# Setup parallel backend.
cl <- makeCluster(10) 
registerDoParallel(cl)


# Run.
for (i in 1:nrow(params) ) {
  
  set.seed(1234) # Same seed for reproducibility.
  
  # To store results. 
  SE_MISR <- FBM(nrow = niter, ncol = nreps, init = NA) # Squared error, mean impact speed reduction
  SE_MIJR <- FBM(nrow = niter, ncol = nreps, init = NA) # Squared error, mean injury risk reduction
  SE_CAR <- FBM(nrow = niter, ncol = nreps, init = NA) # Squared error, crash avoidance rate
  
  
  val <- foreach(j = 1:nreps, .combine = rbind,
                 .packages = c("boot", "caret", "magrittr", "ranger", "stringr", "tidyverse")) %dopar% {
                   
                   # Active sampling.
                   res <- active_sampling(df, 
                                          sampling_method = params$sampling_method[i],
                                          proposal_dist = params$proposal_dist[i],
                                          target = params$target[i],
                                          use_logic = params$use_logic[i],
                                          niter = niter,
                                          nboot = 0, 
                                          verbose = FALSE)  
                   
                   SE_MISR[, j] <- res$results$mean_impact_speed_reduction_sqerr
                   SE_MIJR[, j] <- res$results$mean_injury_risk_reduction_sqerr
                   SE_CAR[, j] <- res$results$crash_avoidance_rate_sqerr
                   
                   1 # foreach return value
                 }
  
  # Save.
  logic <- ifelse(params$use_logic[i], " with logic", ", no logic")
  if ( params$sampling_method[i] == "simple random sampling" ) {
    label <- "SRS"
  } else if ( params$sampling_method[i] == "importance sampling") {
    label <- params$proposal_dist[i]
  } else if ( params$sampling_method[i] == "active sampling" ) {
    label <- params$target[i]
  }
  label <- paste0(label, logic)
  
  res <- tibble(rep = rep(1:nreps, each = niter), 
                iter = rep(1:niter, nreps),
                label = label,
                sampling_method = params$sampling_method[i],
                proposal_dist = params$proposal_dist[i],
                target = params$target[i],
                use_logic = params$use_logic[i],
                SE_MISR = as.numeric(SE_MISR[]),
                SE_MIJR = as.numeric(SE_MIJR[]),
                SE_CAR = as.numeric(SE_CAR[])) 

  save(res, file = sprintf("QC/%s_%s_%s_%s.RData", 
                           params$sampling_method[i],
                           params$proposal_dist[i],
                           params$target[i],
                           params$use_logic[i]))
  
  rm(SE_MIJR, SE_MISR, SE_CAR)
  
}
stopCluster(cl)
rm(cl, val, i, res, df, niter, nreps, active_sampling)
toc()



# Post-processing ---------------------------------------------------------

allres <- NULL
for (i in 1:nrow(params) ) {
  load(file = sprintf("QC/%s_%s_%s_%s.RData", 
                      params$sampling_method[i],
                      params$proposal_dist[i],
                      params$target[i],
                      params$use_logic[i])) 
  
  if ( i == 1) {
    allres <- res
  } else {
    allres %<>% 
      add_row(res)
  }
}

allres %<>% 
  dplyr::select(-rep) %>% 
  group_by(label, sampling_method, proposal_dist, target, use_logic, iter) %>% 
  summarise(RMSE_MISR = sqrt(mean(SE_MISR, na.rm = TRUE)),
            RMSE_MIJR = sqrt(mean(SE_MIJR, na.rm = TRUE)),
            RMSE_CAR = sqrt(mean(SE_CAR, na.rm = TRUE)), .groups = "keep") %>% 
  mutate()

plt <- allres %>% 
  filter(sampling_method != "simple random sampling") 

fig1 <- ggplot(plt, aes(x = iter, y = RMSE_MISR, linetype = sampling_method, colour = label, shape = use_logic)) + 
  geom_line() + 
  geom_point() + 
  scale_colour_brewer(palette = "Paired") + 
  scale_shape_discrete(breaks = c(TRUE, FALSE), labels = c("With logic", "Without logic")) + 
  scale_linetype_discrete(breaks = c("active sampling", "importance sampling"), labels =  c("Active sampling", "Importance sampling")) + 
  labs(x = "Iteration",
       y = "RMSE mean impact speed reduction", 
       colour = "",
       shape = "",
       linetype = "") 

fig2 <- ggplot(plt, aes(x = iter, y = RMSE_MIJR, linetype = sampling_method, colour = label, shape = use_logic)) + 
  geom_line() + 
  geom_point() + 
  scale_colour_brewer(palette = "Paired") + 
  scale_shape_discrete(breaks = c(TRUE, FALSE), labels = c("With logic", "Without logic")) + 
  scale_linetype_discrete(breaks = c("active sampling", "importance sampling"), labels =  c("Active sampling", "Importance sampling")) + 
  labs(x = "Iteration",
       y = "RMSE Mean injury risk reduction", 
       colour = "",
       shape = "",
       linetype = "") 

fig3 <- ggplot(plt, aes(x = iter, y = RMSE_CAR, linetype = sampling_method, colour = label, shape = use_logic)) + 
  geom_line() + 
  geom_point() + 
  scale_colour_brewer(palette = "Paired") + 
  scale_shape_discrete(breaks = c(TRUE, FALSE), labels = c("With logic", "Without logic")) + 
  scale_linetype_discrete(breaks = c("active sampling", "importance sampling"), labels =  c("Active sampling", "Importance sampling")) + 
  labs(x = "Iteration",
       y = "RMSE crash avoidance rate", 
       colour = "",
       shape = "",
       linetype = "") 

ggsave("QC/RMSE MISR.png", fig1, width = 120, height = 60, unit = "mm", dpi = 1000)
ggsave("QC/RMSE MIJR.png", fig2, width = 120, height = 60, unit = "mm", dpi = 1000)
ggsave("QC/RMSE CAR.png", fig3, width = 120, height = 60, unit = "mm", dpi = 1000)