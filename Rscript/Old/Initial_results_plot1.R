library("tidyverse")
library("ggplot2")
library("ggpubr")
load("Data/res_analysis_data_test.R")
fcount <- 1
param_input <- read_excel("Data/param_input.xlsx")
sampling_input <- read_excel("Data/Sampling_input.xlsx")

averlist1 = list()
averlist3 = list()
averlist4 = list()

for (i in 1:length(res_total[[1]])) {
  aver1 <- res_total[[1]][[i]]$aver
  averlist1[[i]] <- aver1
  aver3 <- res_total[[3]][[i]]$aver
  averlist3[[i]] <- aver3
  aver4 <- res_total[[4]][[i]]$aver
  averlist4[[i]] <- aver4
}
aver_sum1 = do.call(rbind, averlist1)
aver_sum3 = do.call(rbind, averlist3)
aver_sum4 = do.call(rbind, averlist4)

single_sim = list()
for (i in 1:length(res_total[[2]])) {
  single_sim[[i]] <- res_total[[2]][[i]]
}
sin_sim_total = do.call(rbind, single_sim)
sin_sim_total = sin_sim_total[(sin_sim_total$samping_method == "optimised"),]


# fix the label 
# try to install the package :install.packages("ggpubr")
g1 <- ggplot(aver_sum1, aes(x = neff0,y=mean_impact_speed0_sqerr,
                            colour = group, shape = reduce_simulations_by_logic,
                            group = interaction(group, reduce_simulations_by_logic))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(100, 250) +
  ylim(0,5) + 
  labs(x = "Needed simulation number", y = "RMSE of Impact speed",
       colour = NULL, shape = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of Impact speed for different sampling methods")

g2 <- ggplot(aver_sum1, aes(x = neff0,y=absolute_impact_speed_reduction_sqerr,
                            colour = group, shape = reduce_simulations_by_logic,
                            group = interaction(group, reduce_simulations_by_logic))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(100, 250) +
  ylim(0,2.5) + 
  labs(x = "Needed simulation number", y = "RMSE of impact speed reduction",
       colour = NULL, shape = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of Impact speed reduction for different sampling methods")

g3 <- ggplot(aver_sum1, aes(x = neff0,y = proportion_crashes_avoided_sqerr,
                            colour = group, shape = reduce_simulations_by_logic,
                            group = interaction(group, reduce_simulations_by_logic))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(100, 250) +
  ylim(0,0.25) + 
  labs(x = "Needed simulation number", y = "RMSE of mean crash avoidance rate",
       colour = NULL, shape = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of mean crash avoidance rate for different sampling methods")

g4 <- ggplot(aver_sum1, aes(x = neff0,y = absolute_injury_risk_reduction_sqerr,
                            colour = group, shape = reduce_simulations_by_logic,
                            group = interaction(group, reduce_simulations_by_logic))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(100, 250) +
  ylim(0,0.0025) + 
  labs(x = "Needed simulation number", y = "RMSE of Mean injury risk reduction",
       colour = NULL, shape = NULL)+
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

g5 <- ggplot(aver_sum3, aes(x = neff0,y=mean_impact_speed0_sqerr,
                            colour = group, shape = reduce_simulations_by_logic,
                            group = interaction(group, reduce_simulations_by_logic))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(100, 250) +
  ylim(0,5) + 
  labs(x = "Needed simulation number", y = "RMSE of Impact speed",
       colour = NULL, shape = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of Impact speed for different sampling methods")

g6 <- ggplot(aver_sum3, aes(x = neff0,y=absolute_impact_speed_reduction_sqerr,
                            colour = group, shape = reduce_simulations_by_logic,
                            group = interaction(group, reduce_simulations_by_logic))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(100, 250) +
  ylim(0,2.5) + 
  labs(x = "Needed simulation number", y = "RMSE of impact speed reduction",
       colour = NULL, shape = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of Impact speed reduction for different sampling methods")

g7 <- ggplot(aver_sum3, aes(x = neff0,y = proportion_crashes_avoided_sqerr,
                            colour = group, shape = reduce_simulations_by_logic,
                            group = interaction(group, reduce_simulations_by_logic))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(100, 250) +
  ylim(0,0.25) + 
  labs(x = "Needed simulation number", y = "RMSE of mean crash avoidance rate",
       colour = NULL, shape = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of mean crash avoidance rate for different sampling methods")

g8 <- ggplot(aver_sum3, aes(x = neff0,y = absolute_injury_risk_reduction_sqerr,
                            colour = group, shape = reduce_simulations_by_logic,
                            group = interaction(group, reduce_simulations_by_logic))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(100, 250) +
  ylim(0,0.0025) + 
  labs(x = "Needed simulation number", y = "RMSE of Mean injury risk reduction",
       colour = NULL, shape = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of mean injury risk reduction for different sampling methods")

ggsave(sprintf("Output/fig%d.png", fcount), g5, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g6, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g7, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g8, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1

groupnames= unique(aver_sum1$group)
sublist = list()
for(i in 1:length(groupnames)){
  group1 = aver_sum1[aver_sum1$group == groupnames[i],]
  group1$inter_choice = rep(paste(param_input$num_cases_per_iteration[1],'per iteration'),length(group1$group))
  group3 = aver_sum3[aver_sum3$group == groupnames[i],]
  group3$inter_choice = rep(paste(param_input$num_cases_per_iteration[3],'per iteration'),length(group3$group))
  group4 = aver_sum4[aver_sum4$group == groupnames[i],]
  group4$inter_choice = rep(paste(param_input$num_cases_per_iteration[4],'per iteration'),length(group4$group))
  sub_inloop = rbind(group1,group3,group4)
  sublist[[i]] <- sub_inloop
}


g9 <- ggplot(sublist[[1]], aes(x = neff0,y=mean_impact_speed0_sqerr)) +
  geom_point(aes(colour = inter_choice), size = 1) +
  geom_line(aes(colour = inter_choice), size = 1) +
  xlim(100, 250) +
  ylim(0,5) + 
  labs(x = "Needed simulation number", y = "RMSE of Impact speed",
       colour = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("Compare RMSE of Impact speed for different sampling methods")

g10 <- ggplot(sublist[[2]], aes(x = neff0,y=absolute_impact_speed_reduction_sqerr)) +
  geom_point(aes(colour = inter_choice), size = 1) +
  geom_line(aes(colour = inter_choice), size = 1) +
  xlim(100, 250) +
  ylim(0,2.5) + 
  labs(x = "Needed simulation number", y = "RMSE of impact speed reduction",
       colour = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("Compare RMSE of Impact speed reduction for different sampling methods")

g11 <- ggplot(sublist[[3]], aes(x = neff0,y = proportion_crashes_avoided_sqerr)) +
  geom_point(aes(colour = inter_choice), size = 1) +
  xlim(100, 250) +
  ylim(0,0.25) + 
  geom_line(aes(colour = inter_choice), size = 1) +
  labs(x = "Needed simulation number", y = "RMSE of mean crash avoidance rate",
       colour = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("Compare RMSE of mean crash avoidance rate for different sampling methods")

g12 <- ggplot(sublist[[4]], aes(x = neff0,y = absolute_injury_risk_reduction_sqerr)) +
  geom_point(aes(colour = inter_choice), size = 1) +
  xlim(100, 250) +
  ylim(0,0.00255) + 
  geom_line(aes(colour = inter_choice), size = 1) +
  labs(x = "Needed simulation number", y = "RMSE of Mean injury risk reduction",
       colour = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("Compare RMSE of mean injury risk reduction for different sampling methods")

ggsave(sprintf("Output/fig%d.png", fcount), g9, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g10, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g11, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g12, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 

optimised_group = sampling_input$group[1:4]
nonoptimised_group = sampling_input$group[5:length(sampling_input$group)]
aver_sum1_optimised = aver_sum1[aver_sum1$group %in% optimised_group,]
aver_sum1_nonoptimised = aver_sum1[aver_sum1$group %in% nonoptimised_group,]
aver_sum1_mixed = rbind(aver_sum1_optimised,aver_sum1[aver_sum1$group == "propto eoff_acc_prob",])

sin_group1 = sin_sim_total[sin_sim_total$target == optimised_group[1],]
sin_group2 = sin_sim_total[sin_sim_total$target == optimised_group[2],]
sin_group3 = sin_sim_total[sin_sim_total$target == optimised_group[3],]
sin_group4 = sin_sim_total[sin_sim_total$target == optimised_group[4],]

g13 <- ggplot(aver_sum1_optimised, aes(x = neff0,y=mean_impact_speed0_sqerr,
                                       colour = group, shape = reduce_simulations_by_logic,
                                       group = interaction(group, reduce_simulations_by_logic))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(100, 250) +
  ylim(0,5) + 
  labs(x = "Needed simulation number", y = "RMSE of Impact speed",
       colour = NULL, shape = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of Impact speed for different optimised sampling methods")

g14 <- ggplot(aver_sum1_optimised, aes(x = neff0,y=absolute_impact_speed_reduction_sqerr,
                                       colour = group, shape = reduce_simulations_by_logic,
                                       group = interaction(group, reduce_simulations_by_logic))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(100, 250) +
  ylim(0,2.5) + 
  labs(x = "Needed simulation number", y = "RMSE of impact speed reduction",
       colour = NULL, shape = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of Impact speed reduction for different optimised sampling methods")

g15 <- ggplot(aver_sum1_optimised, aes(x = neff0,y = proportion_crashes_avoided_sqerr,
                                       colour = group, shape = reduce_simulations_by_logic,
                                       group = interaction(group, reduce_simulations_by_logic))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(100, 250) +
  ylim(0,0.25) + 
  labs(x = "Needed simulation number", y = "RMSE of mean crash avoidance rate",
       colour = NULL, shape = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of mean crash avoidance rate for different optimised sampling methods")

g16 <- ggplot(aver_sum1_optimised, aes(x = neff0,y = absolute_injury_risk_reduction_sqerr,
                                       colour = group, shape = reduce_simulations_by_logic,
                                       group = interaction(group, reduce_simulations_by_logic))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(100, 250) +
  ylim(0,0.0025) + 
  labs(x = "Needed simulation number", y = "RMSE of Mean injury risk reduction",
       colour = NULL, shape = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of mean injury risk reduction for different optimised sampling methods")

ggsave(sprintf("Output/fig%d.png", fcount), g13, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g14, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g15, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g16, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1

g17 <- ggplot(aver_sum1_nonoptimised, aes(x = neff0,y=mean_impact_speed0_sqerr,
                                          colour = group, shape = reduce_simulations_by_logic,
                                          group = interaction(group, reduce_simulations_by_logic))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(100, 250) +
  ylim(0,5) + 
  labs(x = "Needed simulation number", y = "RMSE of Impact speed",
       colour = NULL, shape = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of Impact speed for different nonoptimised sampling methods")

g18 <- ggplot(aver_sum1_nonoptimised, aes(x = neff0,y=absolute_impact_speed_reduction_sqerr,
                                          colour = group, shape = reduce_simulations_by_logic,
                                          group = interaction(group, reduce_simulations_by_logic))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(100, 250) +
  ylim(0,2.5) + 
  labs(x = "Needed simulation number", y = "RMSE of impact speed reduction",
       colour = NULL, shape = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of Impact speed reduction for different nonoptimised sampling methods")

g19 <- ggplot(aver_sum1_nonoptimised, aes(x = neff0,y = proportion_crashes_avoided_sqerr,
                                          colour = group, shape = reduce_simulations_by_logic,
                                          group = interaction(group, reduce_simulations_by_logic))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(100, 250) +
  ylim(0,0.25) + 
  labs(x = "Needed simulation number", y = "RMSE of mean crash avoidance rate",
       colour = NULL, shape = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of mean crash avoidance rate for different nonoptimised sampling methods")

g20 <- ggplot(aver_sum1_nonoptimised, aes(x = neff0,y = absolute_injury_risk_reduction_sqerr,
                                          colour = group, shape = reduce_simulations_by_logic,
                                          group = interaction(group, reduce_simulations_by_logic))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(100, 250) +
  ylim(0,0.0025) + 
  labs(x = "Needed simulation number", y = "RMSE of Mean injury risk reduction",
       colour = NULL, shape = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of mean injury risk reduction for different nonoptimised sampling methods")

ggsave(sprintf("Output/fig%d.png", fcount), g17, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g18, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g19, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g20, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1

g21 <- ggplot(aver_sum1_mixed, aes(x = neff0,y=mean_impact_speed0_sqerr,
                                   colour = group, shape = reduce_simulations_by_logic,
                                   group = interaction(group, reduce_simulations_by_logic))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(100, 250) +
  ylim(0,5) + 
  labs(x = "Needed simulation number", y = "RMSE of Impact speed",
       colour = NULL, shape = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of Impact speed for different sampling methods")

g22 <- ggplot(aver_sum1_mixed, aes(x = neff0,y=absolute_impact_speed_reduction_sqerr,
                                   colour = group, shape = reduce_simulations_by_logic,
                                   group = interaction(group, reduce_simulations_by_logic))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(100, 250) +
  ylim(0,2.5) + 
  labs(x = "Needed simulation number", y = "RMSE of impact speed reduction",
       colour = NULL, shape = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of Impact speed reduction for different sampling methods")

g23 <- ggplot(aver_sum1_mixed, aes(x = neff0,y = proportion_crashes_avoided_sqerr,
                                   colour = group, shape = reduce_simulations_by_logic,
                                   group = interaction(group, reduce_simulations_by_logic))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(100, 250) +
  ylim(0,0.25) + 
  labs(x = "Needed simulation number", y = "RMSE of mean crash avoidance rate",
       colour = NULL, shape = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of mean crash avoidance rate for different sampling methods")

g24 <- ggplot(aver_sum1_mixed, aes(x = neff0,y = absolute_injury_risk_reduction_sqerr,
                                   colour = group, shape = reduce_simulations_by_logic,
                                   group = interaction(group, reduce_simulations_by_logic))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(100, 250) +
  ylim(0,0.0025) + 
  labs(x = "Needed simulation number", y = "RMSE of Mean injury risk reduction",
       colour = NULL, shape = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of mean injury risk reduction for different sampling methods")

ggsave(sprintf("Output/fig%d.png", fcount), g21, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g22, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g23, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g24, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1

g25 <- ggplot(sin_group1, aes(x = neff0,y=impact_speed0_logmean_se)) +
  geom_point(aes(colour = target), size = 1) +
  geom_line(aes(colour = target), size = 1) +
  labs(x = "Simulation amount", y = "Impact speed log mean se",
       colour = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("Impact speed log mean se")
ggsave(sprintf("Output/fig%d.png", fcount), g25, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1