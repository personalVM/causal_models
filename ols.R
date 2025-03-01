source("volume/etl/util_loadPackages.R")

fct_ols = function(form, data){
  start_time <- proc.time()
  
  ols_model = lm(as.formula(form), data = data)
  
  end_time <- proc.time()
  time_taken <- end_time - start_time

  ols_summary <- summary(ols_model)
  r2 <- ols_summary$r.squared
  r2adj <- ols_summary$adj.r.squared
  
  data$residuals = ols_model$residuals
  # data$predicted = predicted
  data$fit = as.vector(ols_model$fitted.values)
  data$y = as.vector(ols_model$model[, 1])
  
  # MAPE = (1/n) * Σ (|Aᵢ - Fᵢ| / Aᵢ) * 100
  mape <- mean(abs((data$y - data$fit) / data$y)) * 100
  rmse <- sqrt(mean((data$fit - data$y)^2))
  
  shapiro = shapiro.test(data$residuals)
  skew = e1071::skewness(data$residuals)
  kurt = kurtosis(data$residuals) 
  
  fct_globalMI <- function(var="gama"){
    mi  = spdep::moran.mc(data[[var]], lw, nsim=599)$statistic %>% as.numeric(.)
    sig = spdep::moran.mc(data[[var]], lw, nsim=599)$p.value %>% as.numeric(.)
    return(list(mi=mi, sig=sig))
  }
  
  return(
    list(
      df      = data,
      model   = ols_model,
      summary = ols_summary,
      R2      = r2,
      R2adj   = r2adj,
      mape    = mape,
      rmse    = rmse,
      skew    = skew,
      kurt    = kurt,
      shapiro = shapiro,
      compTime = time_taken["elapsed"],
      fct_globalMI = fct_globalMI
    )
  )
}

