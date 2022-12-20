
ptsize <- 12
ptsmall <- 8

theme_set(theme_bw()) 
theme_update(axis.text = element_text(size = ptsize, colour = "black", family = "sans"),
             axis.line = element_line(colour = "black", size = 0.25), 
             axis.ticks = element_line(colour = "black", size = 0.25), 
             legend.key.width = unit(1.25, "cm"),
             legend.key.height = unit(0.4, "cm"),
             legend.margin = ggplot2::margin(t = -0.25, unit = 'cm'),
             legend.spacing =  unit(0, "cm"),
             legend.position = "bottom",
             legend.text = element_text(size = ptsmall, colour = "black", family = "sans"),
             legend.title = element_text(size = ptsmall, colour = "black", family = "sans"),
             strip.background.x = element_blank(),
             panel.border = element_blank(),
             panel.grid = element_blank(),  
             plot.subtitle = element_text(size = ptsmall, colour = "black", family = "sans", face = "plain", hjust = 0),
             plot.title = element_text(size = ptsize, colour = "black", family = "sans", face = "plain", hjust = 0),
             text = element_text(size = ptsize, colour = "black", family = "sans"))

ggplot(unlabelled, aes(x = eoff, y = acc, colour = collision_prob0_pred)) + 
  geom_point() + 
  facet_wrap(~caseID, labeller = labeller(caseID = function(x) paste0("ID = ", x))) + 
  scale_colour_viridis_c() 

ggplot(unlabelled, aes(x = eoff, y = acc, colour = collision_prob1_pred)) + 
  geom_point() + 
  facet_wrap(~caseID, labeller = labeller(caseID = function(x) paste0("ID = ", x))) + 
  scale_colour_viridis_c() 

ggplot(unlabelled, aes(x = eoff, y = acc, colour = impact_speed0_pred)) + 
  geom_point() + 
  facet_wrap(~caseID, labeller = labeller(caseID = function(x) paste0("ID = ", x))) + 
  scale_colour_viridis_c() 

ggplot(unlabelled, aes(x = eoff, y = acc, colour = impact_speed1_pred)) + 
  geom_point() + 
  facet_wrap(~caseID, labeller = labeller(caseID = function(x) paste0("ID = ", x))) + 
  scale_colour_viridis_c() 

ggplot(unlabelled, aes(x = eoff, y = acc, colour = injury_risk0_pred)) + 
  geom_point() + 
  facet_wrap(~caseID, labeller = labeller(caseID = function(x) paste0("ID = ", x))) + 
  scale_colour_viridis_c() 

ggplot(unlabelled, aes(x = eoff, y = acc, colour = injury_risk1_pred)) + 
  geom_point() + 
  facet_wrap(~caseID, labeller = labeller(caseID = function(x) paste0("ID = ", x))) + 
  scale_colour_viridis_c() 

ggplot(unlabelled, aes(x = eoff, y = acc, colour = sampling_probability)) + 
  geom_point() + 
  facet_wrap(~caseID, labeller = labeller(caseID = function(x) paste0("ID = ", x))) + 
  scale_colour_viridis_c() 
