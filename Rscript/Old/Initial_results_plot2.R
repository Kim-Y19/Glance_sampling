library("readxl")
library("tidyverse")
library("ggplot2")
library("ggpubr")
library("scales")
load("Data/New_sim_results25.R")
fcount <- 1
param_input <- read_excel("Data/param_input.xlsx")
sampling_input <- read_excel("Data/Sampling_input2.xlsx")
n = 44
averlist1 = list()
simlist1 = list()
for (i in 1:length(res_total[[1]])) {
  aver1 <- res_total[[1]][[i]]$aver
  sim1 <- res_total[[1]][[i]]$res
  averlist1[[i]] <- aver1
  simlist1[[i]] <- sim1
}
aver_sum1 = do.call(rbind, averlist1)
sim1 = do.call(rbind, simlist1)
groups = sampling_input$group[1:5]
optimised_group1 = aver_sum1[aver_sum1$group == "baseline impact speed distribution" |
                               aver_sum1$group == "impact speed reduction"|
                               aver_sum1$group == "crash avoidance" |
                               aver_sum1$group == "injury risk reduction",]
sub_sim1 = sim1[(sim1$proposal_dist == "propto eoff_acc_prob" | sim1$target == "baseline impact speed distribution") &
                  sim1$reduce_simulations_by_logic == TRUE,]
is_n = unique(sub_sim1[sub_sim1$sampling_method == "importance sampling",]$neff0)
os_n = unique(sub_sim1[sub_sim1$sampling_method != "importance sampling",]$neff0)
plot_n = intersect(is_n,os_n)

is_iter = unique(sub_sim1[sub_sim1$sampling_method == "importance sampling",]$iter)
os_iter = unique(sub_sim1[sub_sim1$sampling_method != "importance sampling",]$iter)
plot_iter = intersect(is_iter,os_iter)
groups = sampling_input$group[1:5]
optimised_group1 = aver_sum1[aver_sum1$group == "baseline impact speed distribution" |
                               aver_sum1$group == "impact speed reduction"|
                               aver_sum1$group == "crash avoidance" |
                               aver_sum1$group == "injury risk reduction",]

# fix the label 
# try to install the package :install.packages("ggpubr")
g1 <- ggplot(aver_sum1, aes(x = neff0,y=impact_speed0_KLdiv_sqerr,
                            colour = group, linetype = reduce_simulations_by_logic,
                            group = interaction(group, reduce_simulations_by_logic))) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  #xlim(100, 250) +
  #ylim(0,5) + 
  labs(x = "Needed simulation number", y = "RMSE of impact_speed0_KLdiv",
       colour = NULL, linetype= NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of impact_speed0_KLdiv for different sampling methods")

g2 <- ggplot(aver_sum1, aes(x = neff0,y=absolute_impact_speed_reduction_sqerr,
                            colour = group, linetype = reduce_simulations_by_logic,
                            group = interaction(group, reduce_simulations_by_logic))) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  # xlim(100, 250) +
  # ylim(0,2.5) + 
  labs(x = "Needed simulation number", y = "RMSE of impact speed reduction",
       colour = NULL, linetype = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of Impact speed reduction for different sampling methods")

g3 <- ggplot(aver_sum1, aes(x = neff0,y = proportion_crashes_avoided_sqerr,
                            colour = group, linetype = reduce_simulations_by_logic,
                            group = interaction(group, reduce_simulations_by_logic))) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  # xlim(100, 250) +
  # ylim(0,0.25) + 
  labs(x = "Needed simulation number", y = "RMSE of mean crash avoidance rate",
       colour = NULL, linetype = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of mean crash avoidance rate for different sampling methods")

g4 <- ggplot(aver_sum1, aes(x = neff0,y = absolute_injury_risk_reduction_sqerr,
                            colour = group, linetype = reduce_simulations_by_logic,
                            group = interaction(group, reduce_simulations_by_logic))) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  # xlim(100, 250) +
  # ylim(0,0.0025) + 
  labs(x = "Needed simulation number", y = "RMSE of Mean injury risk reduction",
       colour = NULL, linetype = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of mean injury risk reduction for different sampling methods")

ggsave(sprintf("Output/fig%d.png", fcount), g1, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g2, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g3, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g4, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1
###############
g1 <- ggplot(aver_sum1, aes(x = neff0,y=impact_speed0_KLdiv_sqerr,
                            colour = group, linetype = reduce_simulations_by_logic,
                            group = interaction(group, reduce_simulations_by_logic))) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(100, 500) +
  ylim(0,3) + 
  labs(x = "Needed simulation number", y = "RMSE of impact_speed0_KLdiv",
       colour = NULL, linetype = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of impact_speed0_KLdiv for different sampling methods")

g2 <- ggplot(aver_sum1, aes(x = neff0,y=absolute_impact_speed_reduction_sqerr,
                            colour = group, linetype = reduce_simulations_by_logic,
                            group = interaction(group, reduce_simulations_by_logic))) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(100, 500) +
  ylim(0,3) + 
  labs(x = "Needed simulation number", y = "RMSE of impact speed reduction",
       colour = NULL, linetype = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of Impact speed reduction for different sampling methods")

g3 <- ggplot(aver_sum1, aes(x = neff0,y = proportion_crashes_avoided_sqerr,
                            colour = group, linetype = reduce_simulations_by_logic,
                            group = interaction(group, reduce_simulations_by_logic))) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(100,500) +
  ylim(0,0.1) + 
  labs(x = "Needed simulation number", y = "RMSE of mean crash avoidance rate",
       colour = NULL, linetype = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of mean crash avoidance rate for different sampling methods")

g4 <- ggplot(aver_sum1, aes(x = neff0,y = absolute_injury_risk_reduction_sqerr,
                            colour = group, linetype = reduce_simulations_by_logic,
                            group = interaction(group, reduce_simulations_by_logic))) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(100,500) +
  ylim(0,0.005) + 
  labs(x = "Needed simulation number", y = "RMSE of Mean injury risk reduction",
       colour = NULL, linetype = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of mean injury risk reduction for different sampling methods")

ggsave(sprintf("Output/fig%d.png", fcount), g1, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g2, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g3, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g4, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1
##############
g1 <- ggplot(aver_sum1, aes(x = neff0,y=impact_speed0_KLdiv_sqerr,
                            colour = group, linetype = reduce_simulations_by_logic,
                            group = interaction(group, reduce_simulations_by_logic))) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  #scale_y_continuous(trans='log2') +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  labs(x = "Needed simulation number", y = "RMSE of impact_speed0_KLdiv",
       colour = NULL, linetype = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of impact_speed0_KLdiv for different sampling methods")

g2 <- ggplot(aver_sum1, aes(x = neff0,y=absolute_impact_speed_reduction_sqerr,
                            colour = group, linetype = reduce_simulations_by_logic,
                            group = interaction(group, reduce_simulations_by_logic))) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  labs(x = "Needed simulation number", y = "RMSE of impact speed reduction",
       colour = NULL, linetype = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of Impact speed reduction for different sampling methods")

g3 <- ggplot(aver_sum1, aes(x = neff0,y = proportion_crashes_avoided_sqerr,
                            colour = group, linetype = reduce_simulations_by_logic,
                            group = interaction(group, reduce_simulations_by_logic))) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  labs(x = "Needed simulation number", y = "RMSE of mean crash avoidance rate",
       colour = NULL, linetype = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of mean crash avoidance rate for different sampling methods")

g4 <- ggplot(aver_sum1, aes(x = neff0,y = absolute_injury_risk_reduction_sqerr,
                            colour = group, linetype = reduce_simulations_by_logic,
                            group = interaction(group, reduce_simulations_by_logic))) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  labs(x = "Needed simulation number", y = "RMSE of Mean injury risk reduction",
       colour = NULL, linetype = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of mean injury risk reduction for different sampling methods")

ggsave(sprintf("Output/fig%d.png", fcount), g1, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g2, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g3, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g4, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1
###############
fcount <- fcount + 1
##############
g1 <- ggplot(optimised_group1, aes(x = neff0,y=impact_speed0_KLdiv_sqerr,
                                   colour = group
)) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  #xlim(100, 250) +
  #ylim(0,5) + 
  labs(x = "Needed simulation number", y = "RMSE of impact_speed0_KLdiv",
       colour = NULL, linetype = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of impact_speed0_KLdiv for different sampling methods")

g2 <- ggplot(optimised_group1, aes(x = neff0,y=absolute_impact_speed_reduction_sqerr,
                                   colour = group, linetype = reduce_simulations_by_logic,
                                   group = interaction(group, reduce_simulations_by_logic))) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  # xlim(100, 250) +
  # ylim(0,2.5) + 
  labs(x = "Needed simulation number", y = "RMSE of impact speed reduction",
       colour = NULL, linetype = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of Impact speed reduction for different sampling methods")

g3 <- ggplot(optimised_group1, aes(x = neff0,y = proportion_crashes_avoided_sqerr,
                                   colour = group, linetype = reduce_simulations_by_logic,
                                   group = interaction(group, reduce_simulations_by_logic))) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  # xlim(100, 250) +
  # ylim(0,0.25) + 
  labs(x = "Needed simulation number", y = "RMSE of mean crash avoidance rate",
       colour = NULL, linetype = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of mean crash avoidance rate for different sampling methods")

g4 <- ggplot(optimised_group1, aes(x = neff0,y = absolute_injury_risk_reduction_sqerr,
                                   colour = group, linetype = reduce_simulations_by_logic,
                                   group = interaction(group, reduce_simulations_by_logic))) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  # xlim(100, 250) +
  # ylim(0,0.0025) + 
  labs(x = "Needed simulation number", y = "RMSE of Mean injury risk reduction",
       colour = NULL, linetype = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of mean injury risk reduction for different sampling methods")

ggsave(sprintf("Output/fig%d.png", fcount), g1, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g2, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g3, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g4, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1

g1 <- ggplot(aver_sum1, aes(x = neff0,y=sd_mean_impact_speed0_sqerr,
                            colour = group, linetype = reduce_simulations_by_logic,
                            group = interaction(group, reduce_simulations_by_logic))) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  #xlim(100, 250) +
  #ylim(0,5) + 
  labs(x = "Needed simulation number", y = "sd of RMSE of Impact speed",
       colour = NULL, linetype = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("sd of RMSE of Impact speed for different sampling methods")

g2 <- ggplot(aver_sum1, aes(x = neff0,y=sd_absolute_impact_speed_reduction_sqerr,
                            colour = group, linetype = reduce_simulations_by_logic,
                            group = interaction(group, reduce_simulations_by_logic))) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  # xlim(100, 250) +
  # ylim(0,2.5) + 
  labs(x = "Needed simulation number", y = "sd of RMSE of impact speed reduction",
       colour = NULL, linetype = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("sd of RMSE of Impact speed reduction for different sampling methods")

g3 <- ggplot(aver_sum1, aes(x = neff0,y = sd_proportion_crashes_avoided_sqerr,
                            colour = group, linetype = reduce_simulations_by_logic,
                            group = interaction(group, reduce_simulations_by_logic))) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  # xlim(100, 250) +
  # ylim(0,0.25) + 
  labs(x = "Needed simulation number", y = "sd of RMSE of mean crash avoidance rate",
       colour = NULL, linetype = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("sd of RMSE of mean crash avoidance rate for different sampling methods")

g4 <- ggplot(aver_sum1, aes(x = neff0,y = sd_absolute_injury_risk_reduction_sqerr,
                            colour = group, linetype = reduce_simulations_by_logic,
                            group = interaction(group, reduce_simulations_by_logic))) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  # xlim(100, 250) +
  # ylim(0,0.0025) + 
  labs(x = "Needed simulation number", y = "sd of RMSE of Mean injury risk reduction",
       colour = NULL, linetype = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("sd of RMSE of mean injury risk reduction for different sampling methods")

ggsave(sprintf("Output/fig%d.png", fcount), g1, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g2, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g3, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g4, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1
g1 <- ggplot(sub_sim1) +
  
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2],],
              aes(x = neff0,y = mean_impact_speed0_sqerr,fill = sampling_method),
              trim = FALSE)+
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2] + round(length(plot_iter)/4),],
              aes(x = neff0,y = mean_impact_speed0_sqerr,fill = sampling_method),
              trim = FALSE)+
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2] + 2*round(length(plot_iter)/4),],
              aes(x = neff0,y = mean_impact_speed0_sqerr,fill = sampling_method),
              trim = FALSE) +    
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2] + 3*round(length(plot_iter)/4),],
              aes(x = neff0,y = mean_impact_speed0_sqerr,fill = sampling_method),
              trim = FALSE) +
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[length(plot_iter)],],
              aes(x = neff0,y = mean_impact_speed0_sqerr,fill = sampling_method),
              trim = FALSE) +
  
  labs(x = "Needed simulation number", y = "Impact speed",
       fill = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("IImpact speed for different sampling methods")

g2 <- ggplot(sub_sim1) +
  
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2],],
              aes(x = neff0,y=absolute_impact_speed_reduction_sqerr,fill = sampling_method),
              trim = FALSE)+
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2] + round(length(plot_iter)/4),],
              aes(x = neff0,y=absolute_impact_speed_reduction_sqerr,fill = sampling_method),
              trim = FALSE)+
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2] + 2*round(length(plot_iter)/4),],
              aes(x = neff0,y=absolute_impact_speed_reduction_sqerr,fill = sampling_method),
              trim = FALSE) +    
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2] + 3*round(length(plot_iter)/4),],
              aes(x = neff0,y=absolute_impact_speed_reduction_sqerr,fill = sampling_method),
              trim = FALSE) +
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[length(plot_iter)],],
              aes(x = neff0,y=absolute_impact_speed_reduction_sqerr,fill = sampling_method),
              trim = FALSE) +
  labs(x = "Needed simulation number", y = "Impact speed reduction",
       fill = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("Impact speed reduction for different sampling methods")

g3 <- ggplot(sub_sim1) +
  
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2],],
              aes(x = neff0,y=proportion_crashes_avoided_sqerr,fill = sampling_method),
              trim = FALSE)+
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2] + round(length(plot_iter)/4),],
              aes(x = neff0,y=proportion_crashes_avoided_sqerr,fill = sampling_method),
              trim = FALSE)+
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2] + 2*round(length(plot_iter)/4),],
              aes(x = neff0,y=proportion_crashes_avoided_sqerr,fill = sampling_method),
              trim = FALSE) +    
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2] + 3*round(length(plot_iter)/4),],
              aes(x = neff0,y=proportion_crashes_avoided_sqerr,fill = sampling_method),
              trim = FALSE) +
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[length(plot_iter)],],
              aes(x = neff0,y=proportion_crashes_avoided_sqerr,fill = sampling_method),
              trim = FALSE) +
  labs(x = "Needed simulation number", y = "Mean crash avoidance rate",
       fill = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("Mean crash avoidance rate for different sampling methods")

g4 <- ggplot(sub_sim1) +
  
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2],],
              aes(x = neff0,y=absolute_injury_risk_reduction_sqerr,fill = sampling_method),
              trim = FALSE)+
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2] + round(length(plot_iter)/4),],
              aes(x = neff0,y=absolute_injury_risk_reduction_sqerr,fill = sampling_method),
              trim = FALSE)+
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2] + 2*round(length(plot_iter)/4),],
              aes(x = neff0,y=absolute_injury_risk_reduction_sqerr,fill = sampling_method),
              trim = FALSE) +    
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2] + 3*round(length(plot_iter)/4),],
              aes(x = neff0,y=absolute_injury_risk_reduction_sqerr,fill = sampling_method),
              trim = FALSE) +
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[length(plot_iter)],],
              aes(x = neff0,y=absolute_injury_risk_reduction_sqerr,fill = sampling_method),
              trim = FALSE) +
  labs(x = "Needed simulation number", y = "Mean injury risk reduction",
       fill = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("Mean injury risk reduction for different sampling methods")

g1 <- ggplot(sub_sim1) +
  
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2],],
              aes(x = neff0,y = mean_impact_speed0_sqerr,fill = sampling_method),
              trim = FALSE)+
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2] + round(length(plot_iter)/4),],
              aes(x = neff0,y = mean_impact_speed0_sqerr,fill = sampling_method),
              trim = FALSE)+
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2] + 2*round(length(plot_iter)/4),],
              aes(x = neff0,y = mean_impact_speed0_sqerr,fill = sampling_method),
              trim = FALSE) +    
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2] + 3*round(length(plot_iter)/4),],
              aes(x = neff0,y = mean_impact_speed0_sqerr,fill = sampling_method),
              trim = FALSE) +
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[length(plot_iter)],],
              aes(x = neff0,y = mean_impact_speed0_sqerr,fill = sampling_method),
              trim = FALSE) +
  
  labs(x = "Needed simulation number", y = "Impact speed",
       fill = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("IImpact speed for different sampling methods")

g2 <- ggplot(sub_sim1) +
  
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2],],
              aes(x = neff0,y=absolute_impact_speed_reduction_sqerr,fill = sampling_method),
              trim = FALSE)+
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2] + round(length(plot_iter)/4),],
              aes(x = neff0,y=absolute_impact_speed_reduction_sqerr,fill = sampling_method),
              trim = FALSE)+
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2] + 2*round(length(plot_iter)/4),],
              aes(x = neff0,y=absolute_impact_speed_reduction_sqerr,fill = sampling_method),
              trim = FALSE) +    
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2] + 3*round(length(plot_iter)/4),],
              aes(x = neff0,y=absolute_impact_speed_reduction_sqerr,fill = sampling_method),
              trim = FALSE) +
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[length(plot_iter)],],
              aes(x = neff0,y=absolute_impact_speed_reduction_sqerr,fill = sampling_method),
              trim = FALSE) +
  labs(x = "Needed simulation number", y = "Impact speed reduction",
       fill = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("Impact speed reduction for different sampling methods")

g3 <- ggplot(sub_sim1) +
  
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2],],
              aes(x = neff0,y=proportion_crashes_avoided_sqerr,fill = sampling_method),
              trim = FALSE)+
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2] + round(length(plot_iter)/4),],
              aes(x = neff0,y=proportion_crashes_avoided_sqerr,fill = sampling_method),
              trim = FALSE)+
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2] + 2*round(length(plot_iter)/4),],
              aes(x = neff0,y=proportion_crashes_avoided_sqerr,fill = sampling_method),
              trim = FALSE) +    
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2] + 3*round(length(plot_iter)/4),],
              aes(x = neff0,y=proportion_crashes_avoided_sqerr,fill = sampling_method),
              trim = FALSE) +
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[length(plot_iter)],],
              aes(x = neff0,y=proportion_crashes_avoided_sqerr,fill = sampling_method),
              trim = FALSE) +
  labs(x = "Needed simulation number", y = "Mean crash avoidance rate",
       fill = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("Mean crash avoidance rate for different sampling methods")

g4 <- ggplot(sub_sim1) +
  
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2],],
              aes(x = neff0,y=absolute_injury_risk_reduction_sqerr,fill = sampling_method),
              trim = FALSE)+
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2] + round(length(plot_iter)/4),],
              aes(x = neff0,y=absolute_injury_risk_reduction_sqerr,fill = sampling_method),
              trim = FALSE)+
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2] + 2*round(length(plot_iter)/4),],
              aes(x = neff0,y=absolute_injury_risk_reduction_sqerr,fill = sampling_method),
              trim = FALSE) +    
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[2] + 3*round(length(plot_iter)/4),],
              aes(x = neff0,y=absolute_injury_risk_reduction_sqerr,fill = sampling_method),
              trim = FALSE) +
  geom_violin(data = sub_sim1[sub_sim1$iter == plot_iter[length(plot_iter)],],
              aes(x = neff0,y=absolute_injury_risk_reduction_sqerr,fill = sampling_method),
              trim = FALSE) +
  labs(x = "Needed simulation number", y = "Mean injury risk reduction",
       fill = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("Mean injury risk reduction for different sampling methods")

ggsave(sprintf("Output/fig%d.png", fcount), g1, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1
ggsave(sprintf("Output/fig%d.png", fcount), g2, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1
ggsave(sprintf("Output/fig%d.png", fcount), g3, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1
ggsave(sprintf("Output/fig%d.png", fcount), g4, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1