# https://doi.org/10.1016/j.regsciurbeco.2017.04.001
# rm(list = ls())
gc()
source("volume/etl/util_loadPackages.R")

fct_MGWR = function(
    form = as.formula("ln_emig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
    data, 
    fixedv = c("Intercept", "is_coastal", "region_North", "region_Northeast", "region_South", "region_Southeast")
){
  
  bw <- bandwidths_mgwrsar(
    formula            = form, 
    data               = data,
    coords             = as.matrix(data[,c("centlng", "centlat")]),
    fixed_vars         = fixedv,
    Models             = 'MGWR',
    candidates_Kernels = 'bisq',
    control            = list(
      NN = nrow(data), 
      adaptive = TRUE
    ),
    control_search     = list()
  )
  
  W <- mgwrsar::kernel_matW(
    H        = bw$MGWR_bisq_adaptive$config_model$H,
    kernels  = bw$MGWR_bisq_adaptive$config_model$kernels,
    coord_i  = as.matrix(data[, c("centlng", "centlat")]),
    NN       = bw$MGWR_bisq_adaptive$model$NN,
    adaptive = TRUE,
    diagnull = TRUE,
    rowNorm  = TRUE
  )
  
  mgwr <- mgwrsar::MGWRSAR(
    formula    = form, 
    data       = data,
    coords     = as.matrix(data[,c("centlng", "centlat")]), 
    fixed_vars = fixedv,
    kernels    = bw$MGWR_bisq_adaptive$config_model$kernels,
    H          = bw$MGWR_bisq_adaptive$config_model$H, 
    Model      = 'MGWR',
    control    = list(SE=FALSE,adaptive=TRUE,W=W)
  )
  
  
  fit = mgwr$fit
  residuals = mgwr$residuals
  actual <- mgwr$data[[all.vars(as.formula(form))[1]]]
  betav = mgwr$Betav
  colnames(betav) <- paste0("betav_", colnames(betav))  
  betac = t(as.data.frame(mgwr$Betac))
  colnames(betac) <- paste0("betac_", colnames(betac))  
  
  df_mgwr <- cbind(dfs_shp, betav, betac, fit, residuals, actual) %>%
    sf::st_sf(.) %>% 
    sf::st_set_crs(4326)
  
  fct_globalMI <- function(var="betav_ECI"){
    mi  = spdep::moran.mc(df_mgwr[[var]], lw, nsim=599)$statistic %>% as.numeric(.)
    sig = spdep::moran.mc(df_mgwr[[var]], lw, nsim=599)$p.value %>% as.numeric(.)
    return(list(mi=mi, sig=sig))
  }
  # fct_globalMI(var="residuals")
  
  sst <- sum((actual - mean(actual))^2)
  sse <- sum((actual - fit)^2)
  r_squared <- 1 - (sse / sst)
  n <- length(actual)
  
  p <- df_mgwr[, grep("beta", colnames(df_mgwr))] %>% as.data.frame() %>% select(-geometry) %>% ncol() -1
  adj_r_squared <- 1 - ((1 - r_squared) * (n - 1) / (n - p - 1))
  rmse <- sqrt(mean((actual - fit)^2))
  mape <- mean(abs((actual - fit) / actual)) * 100
  
  return(
    list(
      df_mgwr       = df_mgwr,
      model         = mgwr,
      summary       = summary_mgwrsar(mgwr),
      rmse          = rmse,
      r_squared     = r_squared,
      adj_r_squared = adj_r_squared,
      mape          = mape,
      fct_globalMI  = fct_globalMI
    )
  )
}

