source("volume/etl/util_loadPackages.R")

fct_ols = function(form, data){
  ols_model = lm(as.formula(form), data = data)
  ols_summary <- summary(ols_model)
  r2 <- ols_summary$r.squared
  r2adj <- ols_summary$adj.r.squared
  
  data$residuals = ols_model$residuals
  # data$predicted = predicted
  data$fit = as.vector(ols_model$fitted.values)
  data$y = as.vector(ols_model$model[, 1])
  
  ols_mape <- MLmetrics::MAPE(data$fit, data$y) 
  rmse <- sqrt(mean((data$y - data$fit)^2))
  
  fct_globalMI <- function(var="gama"){
    mi  = spdep::moran.mc(data[[var]], lw, nsim=599)$statistic %>% as.numeric(.)
    sig = spdep::moran.mc(data[[var]], lw, nsim=599)$p.value %>% as.numeric(.)
    return(list(mi=mi, sig=sig))
  }
  
  return(
    list(
      model   = ols_model,
      summary = ols_summary,
      R2      = r2,
      R2adj   = r2adj,
      mape    = ols_mape,
      rmse    = rmse,
      fct_globalMI = fct_globalMI
    )
  )
}
