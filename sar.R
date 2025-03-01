

fct_sar = function(form, data, weights){
  
  start_time <- proc.time()
  
  sar_model <- spatialreg::spautolm(form, data = data, listw = weights, family = "SAR")
  
  end_time <- proc.time()
  time_taken <- end_time - start_time
  
  sar_summary <- summary(sar_model)

  residuals <- sar_model$fit$residuals
  predicted <- sar_model$fit$fitted.values
  actual <- data[[all.vars(as.formula(form))[1]]] # TODO: use this method fpr the other models
  sst <- sum((actual - mean(actual))^2)  # Total sum of squares
  sse <- sum((actual - predicted)^2)    # Residual sum of squares
  r_squared <- 1 - (sse / sst)
  
  n <- length(actual)                    # Number of observations
  p <- length(sar_model$fit$coefficients)  # Number of predictors
  adj_r_squared <- 1 - ((1 - r_squared) * (n - 1) / (n - p - 1))
  
  data$residuals = residuals
  data$predicted = predicted
  
  sar_mape <- mean(abs((actual - predicted) / actual)) * 100
  sar_rmse <- sqrt(mean((predicted - actual)^2))

  sar_shapiro = shapiro.test(residuals)
  sar_skew = skewness(residuals)
  sar_kurt = kurtosis(residuals) 
  
  
  fct_globalMI <- function(var="gama"){
    mi  = spdep::moran.mc(data[[var]], lw, nsim=599)$statistic %>% as.numeric(.)
    sig = spdep::moran.mc(data[[var]], lw, nsim=599)$p.value %>% as.numeric(.)
    return(list(mi=mi, sig=sig))
  }
  
  return(
    list(
      model   = sar_model,
      summary = sar_summary,
      r2      = r_squared,
      r2adj   = adj_r_squared,
      rmse    = sar_rmse,
      mape    = sar_mape,
      shapiro = sar_shapiro,
      skew    = sar_skew,
      kurt    = sar_kurt,
      compTime = time_taken["elapsed"],
      fct_globalMI = fct_globalMI
    )
  )
}
