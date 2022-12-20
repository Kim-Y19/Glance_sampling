update_predictions <- function(labelled, 
                               unlabelled, 
                               target = c("impact speed reduction",
                                          "injury risk reduction", 
                                          "crash avoidance"),
                               use_logic = FALSE, # TRUE or FALSE
                               verbose = FALSE, 
                               plot = FALSE,
                               iter = NA) {

  # Check input parameters.
  target <- match.arg(target)
  
  # Store default handling of warning messages.
  defaultW <- getOption("warn")
  
  # caseID to factor.
  labelled$caseID <- factor(labelled$caseID)
  unlabelled$caseID <- factor(unlabelled$caseID)
  
  # Crashes.
  crashes <- labelled %>% filter(impact_speed0 > 0)
  

  # Hyper-parameter tuning grid for random forest regression and classification.
  regGrid <- crossing(splitrule = "variance", 
                      min.node.size = 1:20,
                      mtry = 1:3)
  
  classGrid <- crossing(splitrule = "gini", 
                        min.node.size = 1:20,
                        mtry = 1:3)
  
  
  # Estimate baseline collision probability. ----
  
  # Train random forest.  
  options(warn = -1)
  grid <- classGrid[sample(1:nrow(classGrid), 3), ]
  rf <- safe_caret_train(factor(impact_speed0 > 0, labels = paste0("Y", 0:1)) ~ eoff + acc + impact_speed_max0, 
                         data = labelled,
                         method = "ranger",
                         num.trees = 100,
                         tuneGrid = as.data.frame(grid),
                         trControl = trainControl(method = "cv",
                                                  number = 5,
                                                  classProbs = TRUE))
  options(warn = defaultW)
  
  if ( !is.null(rf) ) { # If able to fit model: calculate predictions.
    
    # Prediction on labelled and unlabelled data.
    p0_train <- predict(rf, labelled, type = "prob")$Y1
    p0_train[p0_train <= 0] <- min(p0_train[p0_train > 0]) # Avoid zero probabilities.
    
    p0_test <- predict(rf, unlabelled, type = "prob")$Y1
    p0_test[p0_test <= 0] <- min(p0_test[p0_test > 0]) # Avoid zero probabilities.
    
    # Accuracy.
    ix <- with(rf$results, which(min.node.size == rf$finalModel$min.node.size & mtry == rf$finalModel$mtry))
    p0_acc <- rf$results[ix, "Accuracy"]
    
  } else { # If unable to fit model: set to constant.
    
    p0_train <- 1
    p0_test <- 1
    p0_acc <- NA
    
  } # End !is.null(rf).
  
  
  # Predict baseline impact speed. ----
  
  # Train random forest.  
  options(warn = -1)
  grid <- regGrid[sample(1:nrow(regGrid), 3), ]
  rf <- safe_caret_train(impact_speed0 ~ eoff + acc + impact_speed_max0, 
                         data = crashes,
                         method = "ranger",
                         num.trees = 100,
                         tuneGrid = as.data.frame(grid),
                         trControl = trainControl(method = "cv",
                                                  number = 5))
  options(warn = defaultW)
  
  if ( !is.null(rf) ) { # If able to fit model: calculate predictions.
    
    # Prediction on labelled and unlabelled data.
    xhat_train <- predict(rf, crashes)
    xhat_test <- predict(rf, unlabelled)

    # Root mean squared prediction error and prediction R-squared.
    rmse_xhat <- sqrt(rf$finalModel$prediction.error) 
    r2_xhat <- rf$finalModel$r.squared
    
  } else { # If unable to fit model: set to constant.
    
    xhat_train <- 1
    xhat_test <- 1
    rmse_xhat <- NA
    r2_xhat <- NA
    
  }
  
  crashes$impact_speed0_pred <- xhat_train
  unlabelled$impact_speed0_pred <- xhat_test
  
 
  # Predict impact speed reduction ----
  if ( target == "impact speed reduction" ) {
    
    # Train random forest.  
    options(warn = -1)
    grid <- regGrid[sample(1:nrow(regGrid), 3), ]
    rf <- safe_caret_train(impact_speed_reduction ~ eoff + acc + impact_speed_max0, 
                           data = crashes,
                           method = "ranger",
                           num.trees = 100,
                           tuneGrid = as.data.frame(grid),
                           trControl = trainControl(method = "cv",
                                                    number = 5))
    options(warn = defaultW)
    
    if ( !is.null(rf) ) { # If able to fit model: calculate predictions.
      
      # Prediction on labelled and unlabelled data.
      yhat_train <- predict(rf, crashes)
      yhat_test <- predict(rf, unlabelled)
      
      # Root mean squared prediction error and prediction R-squared.
      rmse_yhat <- sqrt(rf$finalModel$prediction.error) 
      r2_yhat <- rf$finalModel$r.squared
      
    } else { # If unable to fit model: set to constant.
      
      yhat_train <- 1
      yhat_test <- 1
      rmse_yhat <- NA
      r2_yhat <- NA
      
    }
  } else { # If target not impact speed reduction: don't fit model, set to constant.
    
    yhat_train <- 1
    yhat_test <- 1
    rmse_yhat <- NA
    r2_yhat <- NA
    
  }
  
  
  # Predict injury risk reduction. ----
  if ( target == "injury risk reduction" ) {
    
    # Train random forest.  
    options(warn = -1)
    grid <- regGrid[sample(1:nrow(regGrid), 3), ]
    rf <- safe_caret_train(injury_risk_reduction ~ eoff + acc + impact_speed_max0, 
                           data = crashes,
                           method = "ranger",
                           num.trees = 100,
                           respect.unordered.factors = "partition",
                           tuneGrid = as.data.frame(grid),
                           trControl = trainControl(method = "cv",
                                                    number = 5))
    options(warn = defaultW)
    
    if ( !is.null(rf) ) { # If able to fit model: calculate predictions.
      
      # Prediction on labelled and unlabelled data.
      zhat_train <- predict(rf, crashes)
      zhat_test <- predict(rf, unlabelled)
      
      # Root mean squared prediction error and prediction R-squared.
      rmse_zhat <- sqrt(rf$finalModel$prediction.error) 
      r2_zhat <- rf$finalModel$r.squared
      
    } else { # If unable to fit model: set to constant.
      
      zhat_train <- 1
      zhat_test <- 1
      rmse_zhat <- NA
      r2_zhat <- NA
      
    }
  } else { # If target not injury risk reduction: don't fit model, set to constant.
    
    zhat_train <- 1
    zhat_test <- 1
    rmse_zhat <- NA
    r2_zhat <- NA
    
  }
  
  
  # Estimate counter-measure collision probability. ----
  if ( target == "crash avoidance" ) {
    
    # Train random forest.  
    options(warn = -1)
    grid <- classGrid[sample(1:nrow(classGrid), 3), ]
    rf <- safe_caret_train(factor(impact_speed1 > 0, labels = paste0("Y", 0:1)) ~ eoff + acc + impact_speed_max0, 
                           data = crashes,
                           method = "ranger",
                           num.trees = 100,
                           tuneGrid = as.data.frame(grid),
                           trControl = trainControl(method = "cv",
                                                    number = 5,
                                                    classProbs = TRUE))
    options(warn = defaultW)
    
    if ( !is.null(rf) ) { # If able to fit model: calculate predictions.
      
      # Prediction on labelled and unlabelled data.
      p1_train <- predict(rf, crashes, type = "prob")$Y1
      p1_train[p1_train <= 0] <- min(p1_train[p1_train > 0]) # Avoid zero probabilities.
      
      p1_test <- predict(rf, unlabelled, type = "prob")$Y1
      p1_test[p1_test <= 0] <- min(p1_test[p1_test > 0]) # Avoid zero probabilities.
      
      # Accuracy.
      ix <- with(rf$results, which(min.node.size == rf$finalModel$min.node.size & mtry == rf$finalModel$mtry))
      p1_acc <- rf$results[ix, "Accuracy"]
      
    } else { # If unable to fit model: set to constant.
      
      p1_train <- 1
      p1_test <- 1
      p1_acc <- NA
      
    } # End !is.null(rf).
  } else { # If target not crash avoidance: don't fit model, set to constant.
    
    p1_train <- 1
    p1_test <- 1
    p1_acc <- NA
    
  }
  
  
  # Combine to calculate predictions on labelled  data. 
  labelled %<>% 
    mutate(collision_prob0_pred = p0_train)
  
  crashes %<>%
    mutate(impact_speed0_pred = xhat_train, 
           impact_speed_reduction_pred = yhat_train, 
           injury_risk_reduction_pred = zhat_train, 
           collision_prob1_pred = p1_train) 
  
  # Print.
  if ( verbose ) {
    
    cat(sprintf("Out-of-bag R-squared:
Baseline impact speed = %.2f
Impact speed reduction = %.2f
Injury risk reduction = %.2f.

Out-of-bag Accuracy:
Baseline crash probability = %.2f.
Counter-meature crash probability = %.2f.
                ", 
                r2_xhat, 
                r2_yhat,
                r2_zhat,
                p0_acc,
                p1_acc))
    cat("\n")
    
  }
  
  
  # Add logic. ----
  if ( use_logic ) {
    p0_test <- pmax(p0_test, unlabelled$crash0, na.rm = TRUE)  # Certainty crashes.
    p0_test <- pmin(p0_test, 1 - unlabelled$non_crash0, na.rm = TRUE)  # Certainty non-crashes.
    p1_test <- pmax(p1_test, unlabelled$crash1, na.rm = TRUE)  # Certainty crashes.
    p1_test <- pmin(p1_test, 1 - unlabelled$non_crash1, na.rm = TRUE)  # Certainty non-crashes.
    xhat_test <- with(unlabelled, ifelse(!is.na(max_impact0) & max_impact0 == 1, impact_speed_max0, xhat_test)) # Certainty maximal impact speed crashes. 
    xhat_test <- pmin(unlabelled$impact_speed_max0, xhat_test) # Predictions should not exceed known maximum impact speed.
  }
  
  
  # Plot. ----
  if ( plot ) {
    
    # Plot data.
    plt <- unlabelled %>% 
      mutate(caseID = as.numeric(caseID)) %>%
      mutate(collision_prob0 = p0_test,
             collision_prob1 = p1_test,
             impact_speed0_pred = xhat_test,
             impact_speed_reduction_pred = yhat_test,
             injury_risk_reduction_pred = zhat_test) %>% 
    filter(caseID <= 42) # Plot 42 cases on 7x6 grid. 

    
    # Baseline collision probability.
    
    # 1D.
    ggplot(plt) +
      geom_point(aes(x = eoff, y = 100 * collision_prob0, colour = -acc)) +
      scale_colour_continuous(type = "viridis") +
      labs(x = "OEOFF (s)",
           y = "Baseline collision probability (%)",
           colour = bquote('Maximal deceleration '(km/s^2))) +
      facet_wrap(~caseID, ncol = 7, nrow = 6, labeller = labeller(caseID = function(x) "")) + 
      theme_classic() + 
      theme(panel.spacing = unit(0.1, "cm"), # Increase/decrease to increase/reduce horizontal white space between cases
            strip.background = element_blank(),
            strip.placement = "outside",
            strip.text = element_text(size = 0), # Change size to increase/reduce vertical white space between cases. Set to element_blank() for no white space.
            legend.direction = "horizontal",
            legend.position = "top",
            legend.key.height = unit(0.3, "cm"), # Change width and height as necessary.
            legend.key.width = unit(2, "cm")) +
      guides(colour = guide_colourbar(title.position = "top", title.hjust = 0.5))
    
    filename <- sprintf("Output/BaselineCollisionProbability_1D_Iter%d.png", iter)
    ggsave(filename, width = 160, height = 100, unit = "mm", dpi = 1000)
    
    # 2D.
    ggplot(plt) +
      geom_rect(aes(xmin = eoff - 0.05, xmax = eoff + 0.05, ymin = -acc - 0.25, ymax = -acc + 0.25, fill = 100 * collision_prob0)) +
      scale_fill_continuous(type = "viridis") +
      labs(x = "OEOFF (s)",
           y = bquote('Maximal deceleration '(km/s^2)),
           fill = "Baseline collision probability (%)") +
      facet_wrap(~caseID, ncol = 7, nrow = 6, labeller = labeller(caseID = function(x) "")) + 
      theme_classic() + 
      theme(panel.spacing = unit(0.1, "cm"), # Increase/decrease to increase/reduce horizontal white space between cases
            strip.background = element_blank(),
            strip.placement = "outside",
            strip.text = element_text(size = 0), # Change size to increase/reduce vertical white space between cases. Set to element_blank() for no white space.
            legend.direction = "horizontal",
            legend.position = "top",
            legend.key.height = unit(0.3, "cm"), # Change width and height as necessary.
            legend.key.width = unit(2, "cm")) +
      guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
    
    filename <- sprintf("Output/BaselineCollisionProbability_2D_Iter%d.png", iter)
    ggsave(filename, width = 160, height = 100, unit = "mm", dpi = 1000)
    

    # Baseline impact speed.
    
    # 1D.
    ggplot(plt) +
      geom_point(aes(x = eoff, y = impact_speed0_pred, colour = -acc)) +
      scale_colour_continuous(type = "viridis") +
      labs(x = "OEOFF (s)",
           y = "Baseline impact speed (km/h)",
           colour = bquote('Maximal deceleration '(km/s^2))) +
      facet_wrap(~caseID, ncol = 7, nrow = 6, labeller = labeller(caseID = function(x) "")) + 
      theme_classic() + 
      theme(panel.spacing = unit(0.1, "cm"), # Increase/decrease to increase/reduce horizontal white space between cases
            strip.background = element_blank(),
            strip.placement = "outside",
            strip.text = element_text(size = 0), # Change size to increase/reduce vertical white space between cases. Set to element_blank() for no white space.
            legend.direction = "horizontal",
            legend.position = "top",
            legend.key.height = unit(0.3, "cm"), # Change width and height as necessary.
            legend.key.width = unit(2, "cm")) +
      guides(colour = guide_colourbar(title.position = "top", title.hjust = 0.5))
    
    filename <- sprintf("Output/BaselinImpactSpeed_Pred_1D_Iter%d.png", iter)
    ggsave(filename, width = 160, height = 100, unit = "mm", dpi = 1000)
    
    # 2D.
    ggplot(plt) +
      geom_rect(aes(xmin = eoff - 0.05, xmax = eoff + 0.05, ymin = -acc - 0.25, ymax = -acc + 0.25, fill = impact_speed0_pred)) +
      scale_fill_continuous(type = "viridis") +
      labs(x = "OEOFF (s)",
           y = bquote('Maximal deceleration '(km/s^2)),
           fill = "Baseline impact speed (km/h)") +
      facet_wrap(~caseID, ncol = 7, nrow = 6, labeller = labeller(caseID = function(x) "")) + 
      theme_classic() + 
      theme(panel.spacing = unit(0.1, "cm"), # Increase/decrease to increase/reduce horizontal white space between cases
            strip.background = element_blank(),
            strip.placement = "outside",
            strip.text = element_text(size = 0), # Change size to increase/reduce vertical white space between cases. Set to element_blank() for no white space.
            legend.direction = "horizontal",
            legend.position = "top",
            legend.key.height = unit(0.3, "cm"), # Change width and height as necessary.
            legend.key.width = unit(2, "cm")) +
      guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))

    filename <- sprintf("Output/BaselinImpactSpeed_Pred_2D_Iter%d.png", iter)
    ggsave(filename, width = 160, height = 100, unit = "mm", dpi = 1000)

    
    # Impact speed reduction.
    if ( target == "impact speed reduction" ) {
      
      # 1D.
      ggplot(plt) +
        geom_point(aes(x = eoff, y = impact_speed_reduction_pred, colour = -acc)) +
        scale_colour_continuous(type = "viridis") +
        labs(x = "OEOFF (s)",
             y = "Impact speed reduction (km/h)",
             colour = bquote('Maximal deceleration '(km/s^2))) +
        facet_wrap(~caseID, ncol = 7, nrow = 6, labeller = labeller(caseID = function(x) "")) + 
        theme_classic() + 
        theme(panel.spacing = unit(0.1, "cm"), # Increase/decrease to increase/reduce horizontal white space between cases
              strip.background = element_blank(),
              strip.placement = "outside",
              strip.text = element_text(size = 0), # Change size to increase/reduce vertical white space between cases. Set to element_blank() for no white space.
              legend.direction = "horizontal",
              legend.position = "top",
              legend.key.height = unit(0.3, "cm"), # Change width and height as necessary.
              legend.key.width = unit(2, "cm")) +
        guides(colour = guide_colourbar(title.position = "top", title.hjust = 0.5))
      
      filename <- sprintf("Output/ImpactSpeedReduction_Pred_1D_Iter%d.png", iter)
      ggsave(filename, width = 160, height = 100, unit = "mm", dpi = 1000)
      
      
      # 2D.
      ggplot(plt) +
        geom_rect(aes(xmin = eoff - 0.05, xmax = eoff + 0.05, ymin = -acc - 0.25, ymax = -acc + 0.25, fill = impact_speed_reduction_pred)) +
        scale_fill_continuous(type = "viridis") +
        labs(x = "OEOFF (s)",
             y = bquote('Maximal deceleration '(km/s^2)),
             fill = "Impact speed reduction (km/h)") +
        facet_wrap(~caseID, ncol = 7, nrow = 6, labeller = labeller(caseID = function(x) "")) + 
        theme_classic() + 
        theme(panel.spacing = unit(0.1, "cm"), # Increase/decrease to increase/reduce horizontal white space between cases
              strip.background = element_blank(),
              strip.placement = "outside",
              strip.text = element_text(size = 0), # Change size to increase/reduce vertical white space between cases. Set to element_blank() for no white space.
              legend.direction = "horizontal",
              legend.position = "top",
              legend.key.height = unit(0.3, "cm"), # Change width and height as necessary.
              legend.key.width = unit(2, "cm")) +
        guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
      
      filename <- sprintf("Output/ImpactSpeedReduction_Pred_2D_Iter%d.png", iter)
      ggsave(filename, width = 160, height = 100, unit = "mm", dpi = 1000)

    }
    
    
    # Injury risk reduction.
    if ( target == "injury risk reduction" ) {
      
      # 1D.
      ggplot(plt) +
        geom_point(aes(x = eoff, y = 100 * injury_risk_reduction_pred, colour = -acc)) +
        scale_colour_continuous(type = "viridis") +
        labs(x = "OEOFF (s)",
             y = "Injury risk reduction (percentage points)",
             colour = bquote('Maximal deceleration '(km/s^2))) +
        facet_wrap(~caseID, ncol = 7, nrow = 6, labeller = labeller(caseID = function(x) "")) + 
        theme_classic() + 
        theme(panel.spacing = unit(0.1, "cm"), # Increase/decrease to increase/reduce horizontal white space between cases
              strip.background = element_blank(),
              strip.placement = "outside",
              strip.text = element_text(size = 0), # Change size to increase/reduce vertical white space between cases. Set to element_blank() for no white space.
              legend.direction = "horizontal",
              legend.position = "top",
              legend.key.height = unit(0.3, "cm"), # Change width and height as necessary.
              legend.key.width = unit(2, "cm")) +
        guides(colour = guide_colourbar(title.position = "top", title.hjust = 0.5))
      
      filename <- sprintf("Output/InjuryRiskReduction_Pred_1D_Iter%d.png", iter)
      ggsave(filename, width = 160, height = 100, unit = "mm", dpi = 1000)
      
      
      # 2D.
      ggplot(plt) +
        geom_rect(aes(xmin = eoff - 0.05, xmax = eoff + 0.05, ymin = -acc - 0.25, ymax = -acc + 0.25, fill = 100 * injury_risk_reduction_pred)) +
        scale_fill_continuous(type = "viridis") +
        labs(x = "OEOFF (s)",
             y = bquote('Maximal deceleration '(km/s^2)),
             fill = "Injury risk reduction (percentage points)") +
        facet_wrap(~caseID, ncol = 7, nrow = 6, labeller = labeller(caseID = function(x) "")) + 
        theme_classic() + 
        theme(panel.spacing = unit(0.1, "cm"), # Increase/decrease to increase/reduce horizontal white space between cases
              strip.background = element_blank(),
              strip.placement = "outside",
              strip.text = element_text(size = 0), # Change size to increase/reduce vertical white space between cases. Set to element_blank() for no white space.
              legend.direction = "horizontal",
              legend.position = "top",
              legend.key.height = unit(0.3, "cm"), # Change width and height as necessary.
              legend.key.width = unit(2, "cm")) +
        guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
      
      filename <- sprintf("Output/InjuryRiskReduction_Pred_2D_Iter%d.png", iter)
      ggsave(filename, width = 160, height = 100, unit = "mm", dpi = 1000)
      
    }
    
    
    # Countermeasure collision probability.
    if ( target == "crash avoidance" ) {
      
      # 1D.
      ggplot(plt) +
        geom_point(aes(x = eoff, y = 100 * collision_prob1, colour = -acc)) +
        scale_colour_continuous(type = "viridis") +
        labs(x = "OEOFF (s)",
             y = "Countermeasure collision probability (%)",
             colour = bquote('Maximal deceleration '(km/s^2))) +
        facet_wrap(~caseID, ncol = 7, nrow = 6, labeller = labeller(caseID = function(x) "")) + 
        theme_classic() + 
        theme(panel.spacing = unit(0.1, "cm"), # Increase/decrease to increase/reduce horizontal white space between cases
              strip.background = element_blank(),
              strip.placement = "outside",
              strip.text = element_text(size = 0), # Change size to increase/reduce vertical white space between cases. Set to element_blank() for no white space.
              legend.direction = "horizontal",
              legend.position = "top",
              legend.key.height = unit(0.3, "cm"), # Change width and height as necessary.
              legend.key.width = unit(2, "cm")) +
        guides(colour = guide_colourbar(title.position = "top", title.hjust = 0.5))
      
      filename <- sprintf("Output/CountermeasureCollisionProbability_1D_Iter%d.png", iter)
      ggsave(filename, width = 160, height = 100, unit = "mm", dpi = 1000)
      
      
      # 2D.
      ggplot(plt) +
        geom_rect(aes(xmin = eoff - 0.05, xmax = eoff + 0.05, ymin = -acc - 0.25, ymax = -acc + 0.25, fill = 100 * collision_prob1)) +
        scale_fill_continuous(type = "viridis") +
        labs(x = "OEOFF (s)",
             y = bquote('Maximal deceleration '(km/s^2)),
             fill = "Countermeasure collision probability (%)") +
        facet_wrap(~caseID, ncol = 7, nrow = 6, labeller = labeller(caseID = function(x) "")) + 
        theme_classic() + 
        theme(panel.spacing = unit(0.1, "cm"), # Increase/decrease to increase/reduce horizontal white space between cases
              strip.background = element_blank(),
              strip.placement = "outside",
              strip.text = element_text(size = 0), # Change size to increase/reduce vertical white space between cases. Set to element_blank() for no white space.
              legend.direction = "horizontal",
              legend.position = "top",
              legend.key.height = unit(0.3, "cm"), # Change width and height as necessary.
              legend.key.width = unit(2, "cm")) +
        guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
      
      filename <- sprintf("Output/CountermeasureCollisionProbability_2D_Iter%d.png", iter)
      ggsave(filename, width = 160, height = 100, unit = "mm", dpi = 1000)

    }

  } # End plot.
  
  
  # Return.
  return(list(collision_prob0 = p0_test,
              collision_prob1 = p1_test,
              impact_speed0_pred = xhat_test,
              impact_speed_reduction_pred = yhat_test,
              injury_risk_reduction_pred = zhat_test,
              rmse_impact_speed_reduction = rmse_yhat,
              rmse_injury_risk_reduction = rmse_zhat,
              r2_impact_speed0 = r2_xhat,
              r2_impact_speed_reduction = r2_yhat,
              r2_injury_risk_reduction = r2_zhat,
              accuracy_crash0 = p0_acc,
              accuracy_crash1 = p1_acc))
  
}
