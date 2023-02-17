
# Update case weights.
inc_w <- new_sample %>% 
  add_row(certainty_selections) %>% 
  mutate(w = eoff_acc_prob * nhits * sampling_weight * (impact_speed0 > 0)) %>% 
  group_by(caseID) %>% 
  summarise(w = sum(w), .groups = "keep") %>% 
  pull(w)

sum_w <- rewt * sum_w + bwt * inc_w
case_weight <- 1 / sum_w
case_weight[is.infinite(case_weight)] <- 0
df_case_wt <- tibble(case = 1:n_cases, case_weight = case_weight)


# If bootstrap is used: run every 10th/25th/50th/100th new observation, etc. 
# Find corresponding iterations.
if ( nboot > 0 & niter * batch_size >= 10) {
  
  n_update <- c(seq(10, 50, 10), seq(75, 200, 25), seq(250, 500, 50), seq(600, 1000, 100), seq(1250, 2500, 250), seq(3000, 5000, 500), seq(6000, 10000, 1000))
  boot_update_iterations <- vapply(1:length(n_update), function(ix) which(c(n_seq, max(n_seq) + 1) >= n_update[ix] & c(0, n_seq) >= n_update[ix])[1] - 1, FUN.VALUE = numeric(1))
  boot_update_iterations <- unique(as.numeric(na.omit(boot_update_iterations)))
  
  if ( verbose & length(n_update) > 0 ) {
    print(sprintf("Bootstrap standard error updated at iterations %s", paste(boot_update_iterations, collapse = ", ")))
    print(sprintf("after %s observations", paste(n_seq[boot_update_iterations], collapse = ", ")))
    cat("\n")
  } 
} else {
  boot_update_iterations <- NA
}


# Variance estimation using classical survey sampling method (Sen-Yates-Grundy estimator). ----
Y <- new_sample %>% 
  mutate(baseline_crash = impact_speed0 > 0) %>% 
  dplyr::select(impact_speed_reduction, injury_risk_reduction, crash_avoidance, baseline_crash) %>%
  as.matrix()
n <- batch_size
X <- with(new_sample, t((t(eoff_acc_prob * Y / mu) - totals[i, ] / n)))
W <- with(new_sample, diag(nhits, nrow = nrow(X), ncol = nrow(X)))

covest_classic <- rewt^2 * covest_classic + bwt^2 * n / (n - 1) * t(X) %*% W %*% X 

G <- matrix(data = c(1 / t_y[4], 0, 0,-t_y[1] / t_y[4]^2,
                     0, 1 / t_y[4], 0,-t_y[2] / t_y[4]^2,
                     0, 0, 1 / t_y[4],-t_y[3] / t_y[4]^2), 
            byrow = FALSE, nrow = 4, ncol = 3)

se_classic <- sqrt(diag(t(G) %*% covest_classic %*% G))


# Variance estimation using bootstrap method. ----

# If an element is selected multiple times: split into multiple observations/rows.
ix <- rep(1:nrow(labelled), labelled$nhits) # To repeat rows.
crashes <- labelled[ix, ] %>%
  mutate(final_weight = eoff_acc_prob * batch_weight * sampling_weight) %>%
  add_row(certainty_selections) %>% 
  filter(impact_speed0 > 0 & final_weight > 0)

# If any crashes have been generated.
# Run bootstrap at selected iterations (every 10th new observation).
if ( nrow(crashes) > 0 & i %in% boot_update_iterations ) { 
  boot <- boot(crashes, 
               statistic = function(data, ix) estimate_targets(data[ix, ], weightvar = "final_weight"), 
               R = nboot) 
  se_boot <- apply(boot$t, 2 , sd) # Standard error of estimates.
}  


lower_classic <- est - qnorm(0.975) * se_classic 
upper_classic <- est + qnorm(0.975) * se_classic
lower_boot <- est - qnorm(0.975) * se_boot 
upper_boot <- est + qnorm(0.975) * se_boot
cov_classic <- as.numeric(lower_classic < ground_truth & ground_truth < upper_classic)
cov_boot <- as.numeric(lower_boot < ground_truth & ground_truth < upper_boot)

covest_classic <- matrix(0, nrow = 4, ncol = 4) # To store covariance matrix estimates per iteration.
se_boot <- rep(NA, 3)# To store bootstrap standard error.


ground_truth <- estimate_targets(data, weightvar = "eoff_acc_prob") # Calculate target quantities on full data.

data %>% 
  group_by(caseID) %>% 
  summarise(mean = sum(crash_avoidance * (impact_speed0 > 0) * eoff_acc_prob) / sum( (impact_speed0 > 0) * eoff_acc_prob), .groups = "keep") %>% 
  pull() %>% mean()




sum <- matrix(0, nrow = 4, ncol = 4)
Sigma_kl <- n * ( diag(new_sample$pi) - tcrossprod((new_sample$pi)) )
mu_kl <- Sigma_kl + tcrossprod(new_sample$mu)
for ( k in 1:nrow(new_sample) ) {
  for ( l in 1:nrow(new_sample) ) {
    sum <- sum - 0.5 * new_sample$nhits[k] * new_sample$nhits[l] * Sigma_kl[k, l] / mu_kl[k, l] * tcrossprod(new_sample$eoff_acc_prob[k] * Y[k, ]/new_sample$mu[k] - new_sample$eoff_acc_prob[l] * Y[l, ]/new_sample$mu[l])
  }
}

sum2 <- matrix(0, nrow = 4, ncol = 4)
for ( k in 1:nrow(new_sample) ) {
  sum2 <- sum2 + n / (n - 1) * with(new_sample, nhits * tcrossprod(Y[k, ] * eoff_acc_prob / mu - totals[i, ] / n))
}



# Check.
print(sum)
print(sum2)
print(n / (n - 1) * t(X) %*% W %*% X )


# Variance estimation using bootstrap method. ----
crashes <- labelled %>% 
  filter(impact_speed0 > 0 & final_weight > 0)

# If any crashes have been generated.
# Run bootstrap at selected iterations (every 10th new observation).
if ( nrow(crashes) > 0 & i %in% boot_update_iterations ) { 
  boot <- boot(crashes, 
               statistic = function(data, w) {
                 data$final_weight = data$final_weight * w 
                 estimate_targets(data, weightvar = "final_weight")
               }, 
               R = nboot,
               stype = "w",
               weights = crashes$nhits) 
  se_boot <- apply(boot$t, 2 , sd) # Standard error of estimates.
} 



impact_speed0_logmean <- with(crashes, sum(w * log(impact_speed0)) / sum(w))
impact_speed0_logSD <- with(crashes, sqrt(sum(w * (log(impact_speed0) - impact_speed0_logmean)^2) / sum(w)))

impact_speed0_logmean <- NA
impact_speed0_logSD <- NA

# "impact_speed0_logmean" = impact_speed0_logmean,
# "impact_speed0_logSD" = impact_speed0_logSD

rmse_log_xhat <- with(crashes, sd(log(impact_speed0) - log(impact_speed0_pred)))
rmse_log_impact_speed0 = rmse_log_xhat
sigma_log_impact_speed0 = pred$rmse_log_impact_speed0


add_column(impact_speed0_KLdiv = KL(ground_truth["impact_speed0_logmean"], 
                                    ground_truth["impact_speed0_logSD"],
                                    est["impact_speed0_logmean"], 
                                    est["impact_speed0_logSD"])) %>% # Kullback-Leibler divergence.
  
  
  
  
  if ( target %in% c("crash avoidance", "all") ) {
  
  # If prediction accuracy is missing or negative: set all equal.
  if ( is.na(r2$accuracy_crash1) | r2$accuracy_crash1 < 0 ) {  
    unlabelled$collision_prob1_pred <- 1
  } 
  
  rr <- 1 - est$crash_avoidance_rate
  if ( modify_scheme == "naive" ) {
    sigma2 <- 0
  } else {
    sigma2 <- with(unlabelled, collision_prob1_pred * (1 - collision_prob1_pred))
  }
  
  size2 <- (unlabelled$collision_prob1_pred - rr)^2 + sigma2
  size <- sqrt(size2)
  size_mat[, 4] <- size
  
} 

# Naive scheme: plug in predictions with no account for prediction uncertainty.
if ( modify_scheme == "naive" ) {
  unlabelled %<>%
    mutate(sigma_log_impact_speed0 = 0,
           sigma_impact_speed_reduction = 0,
           sigma_injury_risk_reduction = 0,
           sigma_collision1 = 0)
}  

if ( is.na(r2) | r2 < 0 ) { 
  mu <- 0
  pred <- 1
  sigma <- 0
} else { 
  
}

if ( target %in% c("baseline impact speed distribution", "all") ) {
  
  # If prediction R-square is missing or negative: set all equal.
  # Else: calculate Z-score.
  if ( is.na(r2$impact_speed0) | r2$impact_speed0 < 0 ) { 
    Z <- 1
  } else { 
    Z <- (log(unlabelled$impact_speed0_pred) - est$impact_speed0_logmean) / 
      est$impact_speed0_logSD
  }
  
  r <- rmse$log_impact_speed0 / est$impact_speed0_logSD
  size <- sqrt(Z^2 + r^2 + 0.25 * (1 + 2 * Z^2 + Z^4 + 6 * Z^2 * r^2 + 3 * r^2))
  size_mat[, 3] <- size
  
} 

size_mat <- matrix(NA, nrow = nrow(unlabelled), ncol = 4) # Matrix to store results.

# Check that all 'sizes' are valid.
if ( target == "all" ) {
  
  for ( i in 1:ncol(size_mat) ) {
    
    size <- size_mat[, i]
    
    # If any invalid or no positive 'sizes' found -> set all equal. 
    if ( any(is.na(size)) || !any(size > 0) || any(is.infinite(size)) ) { 
      size <- 1
    }
    size[size <= 0] <- min(size[size > 0]) # Zeroes and negative values not allowed.
    
    size <- size^2 / sum(size^2) # Calculate squares and standardise.
    size_mat[, i] <- size # Store.
    
  }
  
} else {
  
  # If any invalid or no positive 'sizes' found -> set all equal. 
  if ( any(is.na(size)) || !any(size > 0) || any(is.infinite(size)) ) { 
    size <- 1
  }
  size[size <= 0] <- min(size[size > 0]) # Zeroes and negative values not allowed.
  
}


# If target = "all": use 'average' (root mean squared size).
if ( target == "all" ) { 
  size <- sqrt(rowMeans(size_mat)) 
}



# Adjustment to account for sampling of multiple cases per iteration. ----
case_probability <- tapply(sampling_probability, unlabelled$caseID, sum)

cases <- unique(unlabelled$caseID)
certainty_selection_cases <- NULL
while ( any(case_probability > 1) ) {
  
  ix <- as.numeric(names(which(case_probability > 1)))
  certainty_selection_cases <- union(certainty_selection_cases, ix)
  
  for ( i in seq_along(ix) ) {
    
    jx <- which(unlabelled$caseID == ix[i])
    sampling_probability[jx] <- sampling_probability[jx] / case_probability[paste(ix[i])]
    
  }
  
  jx <- which( !(unlabelled$caseID %in% certainty_selection_cases) )
  sampling_probability[jx] <- (n_cases - length(certainty_selection_cases)) * size[jx] / sum(size[jx]) 
  
  case_probability <- tapply(sampling_probability, unlabelled$caseID, sum)
  
} 


# Sample cases.
cases <- as.numeric(names(table(unlabelled$caseID)))
prob$case_probability[prob$case_probability >= (1 - 1e-3)] <- 1
if ( all(prob$case_probability == 1) ) {
  new_cases <- cases
} else {
  new_cases <- cases[which(UPmaxentropy(prob$case_probability) == 1)]
}

ix <- rep(0, nrow(unlabelled)) # Binary selection indicator.
for ( j in seq_along(new_cases) ) {
  
  jx <- which(unlabelled$caseID == new_cases[j]) 
  ix[jx] <- as.numeric(rmultinom(n = 1, size = 1, prob = prob$sampling_probability[jx]))
  
}
# Predict impact speed ratio ----

# Train random forest.  
options(warn = -1)
rf <- safe_caret_train(impact_speed_ratio ~ caseID + eoff + acc, 
                       data = labelled %>% filter(impact_speed0 > 0),
                       method = "ranger",
                       num.trees = 100,
                       tuneLength = 3,
                       trControl = trainControl(method = "cv",
                                                number = 5,
                                                search = "random"))
options(warn = defaultW)

if ( !is.null(rf) ) { # If able to fit model: calculate predictions.
  
  # Prediction on labelled and unlabelled data.
  zhat_train <- predict(rf, labelled)
  zhat_test <- predict(rf, unlabelled)
  
  # Root mean squared error.
  rmse_zhat <- sqrt(rf$finalModel$prediction.error) 
  
} else { # If unable to fit model: set to constant.
  
  zhat_train <- 1
  zhat_test <- 1
  rmse_zhat <- 0
  
}

mutate(impact_speed0 = impact_speed0 / maximpact0,
impact_speed1 = impact_speed1 / maximpact0,
impact_speed_ratio = impact_speed1 / impact_speed0)

# Prepare data for model fitting with LASSO.
form <- ~ -1 + scenario + eoff + log(0.1 + eoff) + acc + log(-acc) + 
  caseID*scenario*eoff*acc + caseID*scenario*log(0.1 + eoff)*log(-acc)
X <- model.matrix(form, data = labelled)
newX <- model.matrix(form, data = unlabelled)
ntrain <- nrow(X)
ix <- sample(1:ntrain) # Scramble training data.
y <- labelled$impact_speed[ix]
X <- X[ix, ]


# LASSO regression.
ix <- which(y > 0 & y < ymax)
nfolds <- ifelse(length(ix) < 10, length(ix), 5)

options(warn = -1)
lasso <- safe_cv_glmnet(X[ix, ], log(y[ix]), 
                        grouped = FALSE, nfolds = nfolds, 
                        nlambda = 50, maxit = 1e3,
                        penalty.factor = c(0, rep(1, ncol(X) - 1))) # No penalty on main effect of counter-measure.
options(warn = defaultW)

if ( !is.null(lasso) ) { # If able to fit model: calculate predictions.
  
  # Prediction on labelled data.
  yhat2_train <- exp(as.numeric(predict(lasso, newx = X, s = "lambda.1se", type = "response")))
  yhat2_train[yhat2_train <= 0] <- max(min(yhat2_train[yhat2_train > 0]), 0)
  yhat2_train[yhat2_train > ymax] <- ymax
  
  # Prediction unlabelled data.
  yhat2 <- exp(as.numeric(predict(lasso, newx = newX, s = "lambda.1se", type = "response")))
  yhat2[yhat2 <= 0] <- max(min(yhat2[yhat2 > 0]), 0)
  yhat2[yhat2 > ymax] <- ymax
  
} else { # If unable to fit model: use predictions from random forest. 
  
  yhat2_train <- yhat1_train
  yhat2 <- yhat1
  
}



# LASSO logistic regression with outcome-stratified cross-validation. 
nstrata <- min(min(table(y > 0)), 5)
events <- which(y > 0) 
nonevents <- which(!(y > 0)) 
foldid <- rep(0, length(y))
foldid[events] <- rep(1:nstrata, length(events))[1:length(events)]
foldid[nonevents] <- rep(1:nstrata, length(nonevents))[1:length(nonevents)]

options(warn = -1)
lasso <- safe_cv_glmnet(X, as.factor(y > 0), family = "binomial", 
                        grouped = FALSE, foldid = foldid, 
                        nlambda = 50, maxit = 1e3,
                        penalty.factor = c(0, rep(1, ncol(X) - 1))) # No penalty on main effect of counter-measure.
options(warn = defaultW)

if ( !is.null(lasso) ) { # If able to fit model: calculate predictions. 
  
  # Prediction on labelled data.
  ppos2_train <- as.numeric(predict(lasso, newx = X, s = "lambda.1se", type = "response"))
  ppos2_train[ppos2_train <= 0] <- min(ppos2_train[ppos2_train > 0])
  
  # Prediction on unlabelled data.
  ppos2 <- as.numeric(predict(lasso, newx = newX, s = "lambda.1se", type = "response"))
  ppos2[ppos2 <= 0] <- min(ppos2[ppos2 > 0])
  
} else { # If unable to fit model: use predictions from random forest. 
  
  ppos2_train <- ppos1_train
  ppos2 <- ppos1
  
} 



# LASSO logistic regression with outcome-stratified cross-validation.
nstrata <- min(min(table(y == ymax)), 5)
events <- which(y == ymax) 
nonevents <- which(!(y == ymax)) 
foldid <- rep(0, length(y))
foldid[events] <- rep(1:nstrata, length(events))[1:length(events)]
foldid[nonevents] <- rep(1:nstrata, length(nonevents))[1:length(nonevents)]

options(warn = -1)
lasso <- safe_cv_glmnet(X, as.factor(y == ymax), family = "binomial", 
                        grouped = FALSE, foldid = foldid, 
                        nlambda = 50, maxit = 1e3,
                        penalty.factor = c(0, rep(1, ncol(X) - 1))) # No penalty on main effect of counter-measure.
options(warn = defaultW)

if ( !is.null(lasso) ) { # If able to fit model: calculate predictions. 
  
  # Prediction on labelled data.
  pmax2_train <- as.numeric(predict(lasso, newx = X, s = "lambda.1se", type = "response"))
  pmax2_train[pmax2_train <= 0] <- min(pmax2_train[pmax2_train > 0])
  
  # Prediction on unlabelled data.
  pmax2 <- as.numeric(predict(lasso, newx = newX, s = "lambda.1se", type = "response"))
  pmax2[pmax2 <= 0] <- min(pmax2[pmax2 > 0])
  
} else { # If unable to fit model: use predictions from random forest. 
  
  pmax2 <- pmax1
  pmax2_train <- pmax1_train
  
} 







else if ( target == "injury risk reduction, stratified" ) {
  
  nStrata <- 10
  sizeMat <- matrix(0, nrow = nrow(unlabelled), ncol = nStrata)
  for ( i in 1:nStrata ) {
    sizeMat[, i] <- with(unlabelled, ifelse(impact_speed0_pred > 10 * (i - 1) & impact_speed0_pred <= 10 * i, sqrt(collision_prob0_pred) * eoff_acc_prob * sqrt((injury_risk0_pred - injury_risk1_pred)^2 + sigma^2), rep(0, length(injury_risk0_pred))))
  }
  
  # Pooled 'size' is root sum of squares of standardised individual 'sizes'.
  # Without standardisation: produces same result as non-stratified version.
  size2 <- sizeMat^2
  csum <- colSums(size2)
  ix <- which(csum > 0)
  size <- sqrt(rowSums(scale(size2[, ix], center = FALSE, scale = csum[ix]))) 
  
  # To account for probability of (deceleration, glance) pair and probability of crash in baseline scenario. 
  # Note: use division here since we multiple both on row 98 and 118.
  size <- with(unlabelled, (sqrt(collision_prob0_pred) * eoff_acc_prob)^(-1) * size)
  size[is.na(size)] <- 0
  
} 


# Combine optimised sampling and importance sampling
# with exponential decay on weight for importance sampling
# to favour exploration in early iterations.
k <- log(2) / n_cases *  num_cases_per_iteration 
w <- 1 - exp(-k * (n_cases / num_cases_per_iteration * (nburnin > 0) + i - (nburnin + 1)))

prob1 <- calculate_sampling_scheme(unlabelled, labelled, sampling_method, proposal_dist, target, num_cases_per_iteration)

prob2 <- calculate_sampling_scheme(unlabelled, labelled,
                                   sampling_method = "importance sampling",
                                   proposal_dist = "propto eoff_acc_prob",
                                   target = "NA",
                                   num_cases = num_cases_per_iteration)

prob <- prob1
prob$sampling_probability <- w * prob1$sampling_probability + (1 - w) * prob2$sampling_probability
prob$case_probability <- w * prob1$case_probability + (1 - w) * prob2$case_probability





update_predictions <- function(labelled, unlabelled) {
  
  defaultW <- getOption("warn")
  
  ymin <- min(labelled$impact_speed0)
  ymax <- max(labelled$impact_speed0)
  
  # Labelled, wide to long format.
  labelled0 <- labelled %>% 
    dplyr::select(caseID, eoff, acc, eoff_acc_prob, impact_speed0) %>% 
    mutate(scenario = 0) %>% 
    dplyr::rename("impact_speed" = impact_speed0)
  
  labelled1 <- labelled %>% 
    dplyr::select(caseID, eoff, acc, eoff_acc_prob, impact_speed1) %>% 
    mutate(scenario = 1) %>% 
    dplyr::rename("impact_speed" = impact_speed1)
  
  labelled <- labelled0 %>% 
    add_row(labelled1)
  
  # Unlabelled, wide to long format.
  unlabelled0 <- unlabelled %>% 
    dplyr::select(caseID, eoff, acc, eoff_acc_prob, crash0, non_crash0, max_impact0) %>% 
    mutate(scenario = 0) %>% 
    dplyr::rename("crash" = crash0,
                  "non_crash" = non_crash0,
                  "max_impact" = max_impact0)
  
  unlabelled1 <- unlabelled %>% 
    dplyr::select(caseID, eoff, acc, eoff_acc_prob, crash1, non_crash1, max_impact1) %>% 
    mutate(scenario = 1) %>% 
    dplyr::rename("crash" = crash1,
                  "non_crash" = non_crash1,
                  "max_impact" = max_impact1)
  
  unlabelled <- unlabelled0 %>% 
    add_row(unlabelled1)
  
  crash <- unlabelled$crash
  non_crash <- unlabelled$non_crash
  max_impact <- unlabelled$max_impact
  
  
  # Prepare data for model fitting using fractional polynomials.
  form <- ~ -1 + I((-acc)^(-3)) + I((-acc)^(-2)) +
    I((-acc)^(-1)) + I((-acc)^(-0.5)) +
    log(-acc) + I(-acc) + I((-acc)^0.5) + I((-acc)^2) + I((-acc)^3) +
    I((0.1 + eoff)^(-3)) + I((0.1 + eoff)^(-2)) +
    I((0.1 + eoff)^(-1)) + I((0.1 + eoff)^(-0.5)) +
    log(0.1 + eoff) + eoff + I(eoff^0.5) + I(eoff^2) + I(eoff^3)
  
  X <- model.matrix(form, data = labelled)
  X <- cbind(labelled$scenario, X, X * labelled$scenario)
  colnames(X) <- paste0("X", 1:ncol(X))
  
  newX <- model.matrix(form, data = unlabelled)
  newX <- cbind(unlabelled$scenario, newX, newX * unlabelled$scenario)
  colnames(newX) <- paste0("X", 1:ncol(newX))
  
  ntrain <- nrow(X)
  ix <- sample(1:ntrain) # Scramble training data.
  y <- labelled$impact_speed[ix]
  X <- X[ix, ]
  
  
  # Estimate impact speed. ----
  
  # Random forest.
  options(warn = -1)
  rf <- safe_random_forest(impact_speed ~ scenario + eoff + acc, data = labelled %>% filter(impact_speed > 0 & impact_speed < ymax))
  options(warn = defaultW)
  
  if ( !is.null(rf) ) { # If able to fit model: calculate predictions.
    
    yhat1 <- predict(rf, unlabelled)
    yhat1[yhat1 <= 0] <- max(min(yhat1[yhat1 > 0]), ymin)
    yhat1[yhat1 > ymax] <- ymax
    
  } else { # If unable to fit model: use mean of 0 and max impact speed. 
    
    yhat1 <- rep(ymax / 2, nrow(unlabelled))
    
  }
  
  
  # Fractional polynomial LASSO regression for log impact speed.
  ix <- which(y > 0 & y < ymax)
  nfolds <- ifelse(length(ix) < 10, length(ix), 10)
  
  options(warn = -1)
  lasso <- safe_cv_glmnet(X[ix, ], log(y[ix]), grouped = FALSE, nfolds = nfolds, 
                          penalty.factor = c(0, rep(1, ncol(X) - 1)))
  options(warn = defaultW)
  
  if ( !is.null(lasso) ) { # If able to fit model: calculate predictions.
    
    yhat2 <- exp(as.numeric(predict(lasso, newx = newX, s = "lambda.1se", type = "response")))
    yhat2[yhat2 <= 0] <- max(min(yhat2[yhat2 > 0]), ymin)
    yhat2[yhat2 > ymax] <- ymax
    
  } else { # If unable to fit model: use predictions from random forest. 
    
    yhat2 <- yhat1
    
  }
  
  
  # Estimate collision probability. ----
  
  # Random forest.
  options(warn = -1)
  rf <- safe_random_forest(as.factor(impact_speed > 0) ~ scenario + eoff + acc, data = labelled)
  options(warn = defaultW)
  
  if ( !is.null(rf) ) { # If able to fit model: calculate predictions.
    
    ppos1 <- as.numeric(predict(rf, unlabelled, type = "prob")[, 2])
    ppos1[ppos1 <= 0] <- min(ppos1[ppos1 > 0])
    
  } else { # If unable to fit model: set probabilities manually.
    
    if ( ymax == 0 ) { # If no crashes.
      
      ppos1 <- rep(0, nrow(unlabelled))
      
    } else if ( ymin > 0 ) { # If no non-crashes.
      
      ppos1 <- rep(1, nrow(unlabelled))
      
    } else {
      
      stop("Error in update_predictions > Estimate collision probability > Random forest >  Unable to fit model. Case not implemented. ")
      
    }
    
  } # End !is.null(rf).
  
  
  # Fractional polynomial LASSO logistic regression with outcome-stratified cross-validation. 
  nstrata <- min(min(table(y > 0)), 10)
  events <- which(y > 0) 
  nonevents <- which(!(y > 0)) 
  foldid <- rep(0, length(y))
  foldid[events] <- rep(1:nstrata, length(events))[1:length(events)]
  foldid[nonevents] <- rep(1:nstrata, length(nonevents))[1:length(nonevents)]
  
  options(warn = -1)
  lasso <- safe_cv_glmnet(X, as.factor(y > 0), family = "binomial", grouped = FALSE, foldid = foldid, 
                          penalty.factor = c(0, rep(1, ncol(X) - 1)))
  options(warn = defaultW)
  
  if ( !is.null(lasso) ) { # If able to fit model: calculate predictions. 
    
    ppos2 <- as.numeric(predict(lasso, newx = newX, s = "lambda.1se", type = "response"))
    ppos2[ppos2 <= 0] <- min(ppos2[ppos2 > 0])
    
  } else { # If unable to fit model: use predictions from random forest. 
    
    ppos2 <- ppos1
    
  } 
  
  
  # Estimate max impact speed collision probability. ----
  
  # Random forest.
  options(warn = -1)
  rf <- safe_random_forest(as.factor(impact_speed == ymax) ~ scenario + eoff + acc, data = labelled)
  options(warn = defaultW)
  
  if ( !is.null(rf) ) { # If able to fit model: calculate predictions.
    
    pmax1 <- predict(rf, unlabelled, type = "prob")[, 2]
    pmax1[pmax1 <= 0] <- min(pmax1[pmax1 > 0])
    
  } else { # If unable to fit model: set probabilities manually.
    
    if ( ymin == ymax ) { # If all crashes at maximal impact speed.
      
      pmax1 <- pmax2 <- rep(1, nrow(unlabelled))
      
    } else {
      
      stop("Error in update_predictions > Estimate max impact speed collision probability > Random forest >  Unable to fit model. Case not implemented. ")
      
    }
  }
  
  
  # Fractional polynomial LASSO logistic regression with outcome-stratified cross-validation.
  nstrata <- min(min(table(y == ymax)), 10)
  events <- which(y == ymax) 
  nonevents <- which(!(y == ymax)) 
  foldid <- rep(0, length(y))
  foldid[events] <- rep(1:nstrata, length(events))[1:length(events)]
  foldid[nonevents] <- rep(1:nstrata, length(nonevents))[1:length(nonevents)]
  
  options(warn = -1)
  lasso <- safe_cv_glmnet(X, as.factor(y == ymax), family = "binomial", grouped = FALSE, foldid = foldid, 
                          penalty.factor = c(0, rep(1, ncol(X) - 1)))
  options(warn = defaultW)
  
  if ( !is.null(lasso) ) { # If able to fit model: calculate predictions. 
    
    pmax2 <- as.numeric(predict(lasso, newx = newX, s = "lambda.1se", type = "response"))
    pmax2[pmax2 <= 0] <- min(pmax2[pmax2 > 0])
    
  } else { # If unable to fit model: use predictions from random forest. 
    
    pmax2 <- pmax1
    
  } 
  
  
  # Combine predictions + add logic.
  ppos <- pmax((ppos1 + ppos2) / 2, crash, na.rm = TRUE) 
  ppos <- ifelse( !is.na(non_crash) & non_crash, rep(0, length(ppos)), ppos)
  pmax <- pmax((pmax1 + pmax2) / 2, max_impact, na.rm = TRUE) 
  pmax <- ifelse( !is.na(non_crash) & non_crash, rep(0, length(pmax)), pmax)
  yhat <- (yhat1 + yhat2) / 2
  ypred <- ppos * yhat + pmax * (ymax - yhat)
  
  
  return(list(collision_prob0 = as.numeric(ppos[unlabelled$scenario == 0]), 
              collision_prob1 = as.numeric(ppos[unlabelled$scenario == 1]),
              impact_speed_pred0 = as.numeric(ypred[unlabelled$scenario == 0]),
              impact_speed_pred1 = as.numeric(ypred[unlabelled$scenario == 1])))
  
}