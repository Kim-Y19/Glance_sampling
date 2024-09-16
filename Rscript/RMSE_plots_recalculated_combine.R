# install.packages("profvis")
library(profvis)
# profvis({
library(RColorBrewer)
library("ggplot2")
library("ggpubr")
library(dplyr)
library(scales)
fcount <- 1

load("Data/Application11_0911_synch_stratify_100groups_nboot1_remove.RData")
simlist1 = list()
for (i in 1:length(res_total[[1]])) {
  sim1 <- res_total[[1]][[i]]$res
  simlist1[[i]] <- sim1
}
sim11 = do.call(rbind, simlist1)

load("Data/Application11_0912_synch_stratify_100groups_nboot1_removepart2.RData")
simlist1 = list()
for (i in 1:length(res_total[[1]])) {
  sim1 <- res_total[[1]][[i]]$res
  simlist1[[i]] <- sim1
}
sim11_2 = do.call(rbind, simlist1)
sim11 = rbind(sim11,sim11_2)

load("Data/Application12_0911_synch_stratify_100groups_nboot1_remove.RData")
simlist1 = list()
for (i in 1:length(res_total[[1]])) {
  sim1 <- res_total[[1]][[i]]$res
  simlist1[[i]] <- sim1
}
sim12 = do.call(rbind, simlist1)
load("Data/Application12_0912_synch_stratify_100groups_nboot1_removepart2.RData")
simlist1 = list()
for (i in 1:length(res_total[[1]])) {
  sim1 <- res_total[[1]][[i]]$res
  simlist1[[i]] <- sim1
}
sim12_2 = do.call(rbind, simlist1)
sim12 = rbind(sim12,sim12_2)

load("Data/Application13_0911_synch_stratify_100groups_nboot1_remove.RData")
simlist1 = list()
for (i in 1:length(res_total[[1]])) {
  sim1 <- res_total[[1]][[i]]$res
  simlist1[[i]] <- sim1
}
sim13 = do.call(rbind, simlist1)
load("Data/Application13_0912_synch_stratify_100groups_nboot1_removepart2.RData")
simlist1 = list()
for (i in 1:length(res_total[[1]])) {
  sim1 <- res_total[[1]][[i]]$res
  simlist1[[i]] <- sim1
}
sim13_2 = do.call(rbind, simlist1)
sim13 = rbind(sim13,sim13_2)

sim1 = rbind(sim11,sim12,sim13)
rm(list = setdiff(ls(),"sim1"))
gc()
# load("Data/test.RData")
# simlist1 = list()
# for (i in 1:length(res_total[[1]])) {
#   sim1 <- res_total[[1]][[i]]$res
#   simlist1[[i]] <- sim1
# }
# sim1 = do.call(rbind, simlist1)

load("Data/Application2_0911_synch_stratify_100groups_nboot1_remove.RData")

simlist2 = list()
for (i in 1:length(res_total[[1]])) {
  sim2 <- res_total[[1]][[i]]$res
  simlist2[[i]] <- sim2
}
sim2 = do.call(rbind, simlist2)
load("Data/Application2_0912_synch_stratify_100groups_nboot1_removepart2.RData")

simlist2 = list()
for (i in 1:length(res_total[[1]])) {
  sim2 <- res_total[[1]][[i]]$res
  simlist2[[i]] <- sim2
}
sim2_2 = do.call(rbind, simlist2)
sim2 = rbind(sim2,sim2_2)

load("Data/Application3_0911_synch_stratify_100groups_nboot1_remove.RData")
simlist3 = list()
for (i in 1:length(res_total[[1]])) {
  sim3 <- res_total[[1]][[i]]$res
  simlist3[[i]] <- sim3
}
sim3 = do.call(rbind, simlist3)
load("Data/Application3_0912_synch_stratify_100groups_nboot1_removepart2.RData")
simlist3 = list()
for (i in 1:length(res_total[[1]])) {
  sim3 <- res_total[[1]][[i]]$res
  simlist3[[i]] <- sim3
}
sim3_2 = do.call(rbind, simlist3)
sim3 = rbind(sim3, sim3_2)

sum = rbind(sim1,sim2,sim3)
rm(list = setdiff(ls(),"sum"))
gc()
cb_palette = c("#1B9E77","#D95F02","#7570B3", "#E7298A", "#E6AB02","#A6761D")
withlogic_df <- sum %>%
  # filter(!(sampling_method == "importance sampling" & use_logic==0)) %>%
  filter(sampling_method == "simple random sampling" | use_logic == 1) %>%
  mutate(group = case_when(
    sampling_method == "simple random sampling" ~ "Simple random sampling",
    sampling_method == "importance sampling" & proposal_dist == "density sampling" ~ "Density importance sampling + all logic",
    sampling_method == "importance sampling" & proposal_dist == "severity sampling" ~ "Severity importance sampling + all logic",
    sampling_method == "active sampling" & target == "impact speed reduction" ~ "Active sampling, target = impact speed reduction",
    sampling_method == "active sampling" & target == "crash avoidance" ~ "Active sampling, target = crash avoidance",
    sampling_method == "active sampling" & target == "injury risk reduction" ~ "Active sampling, target = injury risk reduction"
  ))%>%
  mutate(group = factor(group, levels = c(
    "Active sampling, target = impact speed reduction",
    "Active sampling, target = crash avoidance",
    "Active sampling, target = injury risk reduction",
    "Simple random sampling",
    "Density importance sampling + all logic",
    "Severity importance sampling + all logic"
  )))%>%
  group_by(group,iter) %>%
  summarise(
    sampling_method = sampling_method,
    proposal_dist = proposal_dist,
    use_logic = use_logic,
    mean_isr = sqrt(mean(mean_impact_speed_reduction_sqerr,na.rm = TRUE)),
    mean_ca = sqrt(mean(crash_avoidance_rate_sqerr,na.rm = TRUE)),
    mean_irr = sqrt(mean(mean_injury_risk_reduction_sqerr,na.rm = TRUE)),
    sim_n = round(mean(nsim_tot,na.rm = TRUE)),
    target = target
  )
# without logics
withoutlogic_df <- sum %>%
  filter(use_logic ==0) %>%
  mutate(group = case_when(
    sampling_method == "simple random sampling" ~ "Simple random sampling",
    sampling_method == "importance sampling" & proposal_dist == "density sampling" ~ "Density importance sampling without logic",
    sampling_method == "importance sampling" & proposal_dist == "severity sampling" ~ "Severity importance sampling without logic",
    sampling_method == "active sampling" & target == "impact speed reduction" ~ "Active sampling, target = impact speed reduction",
    sampling_method == "active sampling" & target == "crash avoidance" ~ "Active sampling, target = crash avoidance",
    sampling_method == "active sampling" & target == "injury risk reduction" ~ "Active sampling, target = injury risk reduction"
  ))%>%
  mutate(group = factor(group, levels = c(
    "Active sampling, target = impact speed reduction",
    "Active sampling, target = crash avoidance",
    "Active sampling, target = injury risk reduction",
    "Simple random sampling",
    "Density importance sampling without logic",
    "Severity importance sampling without logic"
  )))%>%
  group_by(group,iter) %>%
  summarise(
    sampling_method = sampling_method,
    proposal_dist = proposal_dist,
    use_logic = use_logic,
    mean_isr = sqrt(mean(mean_impact_speed_reduction_sqerr,na.rm = TRUE)),
    mean_ca = sqrt(mean(crash_avoidance_rate_sqerr,na.rm = TRUE)),
    mean_irr = sqrt(mean(mean_injury_risk_reduction_sqerr,na.rm = TRUE)),
    sim_n = round(mean(neff_tot,na.rm = TRUE)),
    target = target
  )  
###
as <- withlogic_df %>%
  bind_rows(withoutlogic_df) %>%
  filter(sampling_method == "active sampling")%>%
  mutate(group = case_when(
    use_logic == TRUE & target == "impact speed reduction" ~ "Target = impact speed reduction with logic",
    use_logic == TRUE & target == "crash avoidance" ~ "Target = crash avoidance with logic",
    use_logic == TRUE & target == "injury risk reduction" ~ "Target = injury risk reduction with logic",
    use_logic == FALSE & target == "impact speed reduction" ~ "Target = impact speed reduction without logic",
    use_logic == FALSE & target == "crash avoidance" ~ "Target = crash avoidance without logic",
    use_logic == FALSE & target == "injury risk reduction" ~ "Target = injury risk reduction without logic"
  )) %>%
  mutate(group = factor(group, levels = c(
    "Target = impact speed reduction with logic",
    "Target = crash avoidance with logic",
    "Target = injury risk reduction with logic",
    "Target = impact speed reduction without logic",
    "Target = crash avoidance without logic",
    "Target = injury risk reduction without logic"
  )))
###
# check <- withlogic_df %>%
#   filter(sampling_method == "simple random sampling") %>%
#   inner_join(withoutlogic_df %>%
#                filter(sampling_method == "simple random sampling"), 
#              by = "sampling_method")  %>%
#   mutate(difference = mean_isr.x - mean_isr.y)
# 
# check1 <- withlogic_df %>%
#   filter(sampling_method == "simple random sampling")%>%
#   select(mean_isr)
# 
# check2 <- withoutlogic_df %>%
#   filter(sampling_method == "simple random sampling")%>%
#   select(mean_isr)
# diff <- check1$mean_isr - check2$mean_isr
###
fcount <- 1
ptsize <- 11
theme_set(theme_bw()) 
theme_update(axis.text = element_text(size = ptsize, colour = "black", family = "serif"),
             axis.line = element_line(colour = "black", size = 0.25), 
             axis.ticks = element_line(colour = "black", size = 0.25), 
             legend.key.width = unit(1.6, "cm"),
             legend.key.height = unit(0.4, "cm"),
             legend.margin= margin(0,200,0,200),
             legend.spacing =  unit(0, "cm"),
             legend.position = "bottom",
             legend.text = element_text(size = ptsize, colour = "black", family = "serif"),
             legend.title = element_text(size = ptsize, colour = "black", family = "serif"),
             strip.background.x = element_blank(),
             panel.border = element_blank(),
             panel.grid = element_blank(),  
             plot.subtitle = element_text(size = ptsize, colour = "black", family = "serif", face = "plain", hjust = 0),
             plot.title = element_text(size = ptsize, colour = "black", family = "serif", face = "plain", hjust = 0),
             text = element_text(size = ptsize, colour = "black", family = "serif"))

g1 <- ggplot(withlogic_df,
             aes(x = sim_n,y=mean_isr,
                 colour = group,linetype = group
                 
             )) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(0,4000) +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  
  labs(x = "Number of simulations", y = "RMSE of impact speed reduction",
       colour = NULL, linetype = NULL)+
  guides(color = guide_legend(override.aes = list(size = 1),nrow=7))+
  scale_color_manual("",values= c(cb_palette)) +
  scale_linetype_manual("",values = c(2,3,4,5,6,1)) +
  theme(legend.position='none')
# +ggtitle("RMSE of Impact speed reduction for different sampling methods")

g2 <- ggplot(withlogic_df,
             aes(x = sim_n,y = mean_ca,
                 colour = group,linetype = group
             )) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(0, 4000) +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  
  labs(x = "Number of simulations", y = "RMSE of mean crash avoidance rate",
       colour = NULL, linetype = NULL)+
  guides(color = guide_legend(override.aes = list(size = 1),nrow=6))+
  scale_color_manual("",values= c(cb_palette)) +
  scale_linetype_manual("",values = c(2,3,4,5,6,1)) +
  theme(legend.position = c(0.45, -0.8))

# +  ggtitle("RMSE of mean crash avoidance rate for different sampling methods")

g3 <- ggplot(withlogic_df,
             aes(x = sim_n,y = mean_irr,
                 colour = group,linetype = group
             )) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(0, 4000) +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  
  labs(x = "Number of simulations", y = "RMSE of mean injury risk reduction",
       colour = NULL, linetype = NULL)+
  guides(color = guide_legend(override.aes = list(size = 1),nrow=7))+
  scale_color_manual("",values= c(cb_palette)) +
  scale_linetype_manual("",values = c(2,3,4,5,6,1)) +
  theme(legend.position='none'
  )

c1 <- ggarrange(g1, g2 ,g3 ,
                ncol = 2, nrow = 2,common.legend = FALSE,align = "hv", labels=c('A','B','C'),
                font.label = list(size = 10))
# +
#   theme(legend.margin = ggplot2::margin(t = 2, unit = 'cm')) 
ggsave(sprintf("Output/remove_200n_0912_recalculated_active_sampling_withlogic_vs_importance_sampling_withlogic_vs_simulationcount.png", fcount), c1, dpi = 1000, width =180, height =160, unit = "mm")
fcount <- fcount + 1

g1 <- ggplot(withoutlogic_df,
             aes(x = sim_n,y=mean_isr,
                 colour = group,linetype = group
                 
             )) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(0, 5000) +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  
  labs(x = "Number of simulations", y = "RMSE of impact speed reduction",
       colour = NULL, linetype = NULL)+
  guides(color = guide_legend(override.aes = list(size = 1),nrow=7))+
  scale_color_manual("",values= c(cb_palette)) +
  scale_linetype_manual("",values = c(2,3,4,5,6,1)) +
  theme(legend.position='none')
# +ggtitle("RMSE of Impact speed reduction for different sampling methods")

g2 <- ggplot(withoutlogic_df,
             aes(x = sim_n,y = mean_ca,
                 colour = group,linetype = group
             )) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(0, 5000) +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  
  labs(x = "Number of simulations", y = "RMSE of mean crash avoidance rate",
       colour = NULL, linetype = NULL)+
  guides(color = guide_legend(override.aes = list(size = 1),nrow=6))+
  scale_color_manual("",values= c(cb_palette)) +
  scale_linetype_manual("",values = c(2,3,4,5,6,1)) +
  theme(legend.position = c(0.4, -0.8))

# +  ggtitle("RMSE of mean crash avoidance rate for different sampling methods")

g3 <- ggplot(withoutlogic_df,
             aes(x = sim_n,y = mean_irr,
                 colour = group,linetype = group
             )) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(0, 5000) +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  
  labs(x = "Number of simulations", y = "RMSE of mean injury risk reduction",
       colour = NULL, linetype = NULL)+
  guides(color = guide_legend(override.aes = list(size = 1),nrow=7))+
  scale_color_manual("",values= c(cb_palette)) +
  scale_linetype_manual("",values = c(2,3,4,5,6,1)) +
  theme(legend.position='none'
  )

c1 <- ggarrange(g1, g2 ,g3 ,
                ncol = 2, nrow = 2,common.legend = FALSE,align = "hv", labels=c('A','B','C'),
                font.label = list(size = 10))+
  theme(legend.margin = ggplot2::margin(t = 10, unit = 'cm'))
ggsave(sprintf("Output/remove_200n_0912_recalculated_active_sampling_withoutlogic_vs_importance_sampling_withoutlogic_vs_simulationcount.png", fcount), c1, dpi = 1000, width =190, height =160, unit = "mm")
fcount <- fcount + 1
##
g1 <- ggplot(as,
             aes(x = sim_n,y=mean_isr,
                 colour = as.factor(target),linetype = as.factor(use_logic)
                 
             )) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(0, 5000) +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  
  labs(x = "Number of simulations", y = "RMSE of impact speed reduction",
       colour = NULL, linetype = NULL)+
  guides(color = guide_legend(override.aes = list(size = 1),nrow=3))+
  scale_color_manual("",values= c(cb_palette)) +
  scale_linetype_manual("",values = c(1,3)) +
  theme(legend.position='none')
# +ggtitle("RMSE of Impact speed reduction for different sampling methods")

g2 <- ggplot(as,
             aes(x = sim_n,y = mean_ca,
                 colour = target,linetype = as.factor(use_logic)
             )) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(0, 5000) +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  
  labs(x = "Number of simulations", y = "RMSE of mean crash avoidance rate",
       colour = "Target", linetype = "Use logic")+
  guides(color = guide_legend(override.aes = list(size = 1),nrow=3))+
  scale_color_manual("",values= c(cb_palette)) +
  scale_linetype_manual("",values = c(1,3)) +
  theme(legend.position = c(0.4, -0.8))

# +  ggtitle("RMSE of mean crash avoidance rate for different sampling methods")

g3 <- ggplot(as,
             aes(x = sim_n,y = mean_irr,
                 colour = target,linetype = as.factor(use_logic)
             )) +
  #geom_point(size = 2) +
  geom_line(size = 1) +
  xlim(0, 5000) +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  
  labs(x = "Number of simulations", y = "RMSE of mean injury risk reduction",
       colour = NULL, linetype = NULL)+
  guides(color = guide_legend(override.aes = list(size = 1),nrow=3))+
  scale_color_manual("",values= c(cb_palette)) +
  scale_linetype_manual("",values = c(1,3)) +
  theme(legend.position='none'
  )

c1 <- ggarrange(g1, g2 ,g3 ,
                ncol = 2, nrow = 2,common.legend = FALSE,align = "hv", labels=c('A','B','C'),
                font.label = list(size = 10))+
  theme(legend.margin = ggplot2::margin(t = 10, unit = 'cm'))
ggsave(sprintf("Output/remove_200n_0912_active_sampling_with_without_logic.png"), c1, dpi = 1000, width =190, height =160, unit = "mm")


# })