# https://doi.org/10.1016/j.regsciurbeco.2017.04.001
# rm(list = ls())
gc()
source("volume/etl/util_loadPackages.R")

fct_GWR = function(
    form = as.formula("ln_emig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
    dep_var,
    data
){
  
  start_time <- proc.time()
  
  bw <- bandwidths_mgwrsar(
    formula            = form, 
    data               = data,
    coords             = as.matrix(data[,c("centlng", "centlat")] %>% as.data.frame() %>% select(-geometry)),
    Models             = 'GWR',
    # candidates_Kernels = 'gauss',
    candidates_Kernels = 'bisq',
    control            = list(
      NN = nrow(data), 
      adaptive = TRUE
    ),
    control_search     = list()
  )
  
  W <- mgwrsar::kernel_matW(
    H        = bw$GWR_bisq_adaptive$config_model$H,
    # H        = bw$MGWR_gauss_adaptive$config_model$H,
    kernels  = bw$GWR_bisq_adaptive$config_model$kernels,
    # kernels  = bw$MGWR_gauss_adaptive$config_model$kernels,
    coord_i  = as.matrix(data[,c("centlng", "centlat")] %>% as.data.frame() %>% select(-geometry)),
    NN       = bw$GWR_bisq_adaptive$model$NN,
    # NN       = bw$MGWR_gauss_adaptive$model$NN,
    adaptive = TRUE,
    diagnull = TRUE,
    rowNorm  = TRUE
  )
  
  gwr <- mgwrsar::MGWRSAR(
    formula    = form, 
    data       = data,
    coords     = as.matrix(data[,c("centlng", "centlat")] %>% as.data.frame() %>% select(-geometry)), 
    # kernels    = bw$MGWR_gauss_adaptive$config_model$kernels,
    kernels    = bw$GWR_bisq_adaptive$config_model$kernels,
    # H          = bw$MGWR_gauss_adaptive$config_model$H, 
    H          = bw$GWR_bisq_adaptive$config_model$H, 
    Model      = 'GWR',
    control    = list(SE=FALSE,adaptive=TRUE,W=W)
  )
  
  end_time <- proc.time()
  time_taken <- end_time - start_time
  
  fit = gwr$fit
  residuals = gwr$residuals
  actual <- gwr$data[[all.vars(as.formula(form))[1]]]
  betas = gwr$Betav
  colnames(betas)[2:ncol(betas)] <- paste0("beta_", colnames(betas)[2:ncol(betas)])  
  # betac = t(as.data.frame(mgwr$Betac))
  # colnames(betac) <- paste0("beta_", colnames(betac))  
  
  df_gwr <- cbind(dfs_shp, betas, fit, residuals, actual) %>%
    sf::st_sf(.) %>% 
    sf::st_set_crs(4326)
  
  fct_globalMI <- function(var="betav_ECI"){
    mi  = spdep::moran.mc(df_gwr[[var]], lw, nsim=599)$statistic %>% as.numeric(.)
    sig = spdep::moran.mc(df_gwr[[var]], lw, nsim=599)$p.value %>% as.numeric(.)
    return(list(mi=mi, sig=sig))
  }
  # fct_globalMI(var="residuals")
  
  sst <- sum((actual - mean(actual))^2)
  sse <- sum((actual - fit)^2)
  r_squared <- 1 - (sse / sst)
  n <- length(actual)
  
  p <- df_gwr[, grep("beta", colnames(df_gwr))] %>% as.data.frame() %>% select(-geometry) %>% ncol() -1
  adj_r_squared <- 1 - ((1 - r_squared) * (n - 1) / (n - p - 1))
  # rmse <- sqrt(mean((actual - fit)^2))
  # mape <- mean(abs((actual - fit) / actual)) * 100
  
  gwr_mape <- mean(abs((df_gwr[[dep_var]] - fit) / df_gwr[[dep_var]])) * 100
  gwr_rmse <- sqrt(mean((fit - df_gwr[[dep_var]])^2))
  gwr_shapiro = shapiro.test(residuals)
  gwr_skew = skewness(residuals)
  gwr_kurt = kurtosis(residuals) 
  
  return(
    list(
      df            = df_gwr,
      model         = gwr,
      summary       = summary_mgwrsar(gwr),
      r_squared     = r_squared,
      adj_r_squared = adj_r_squared,
      mape          = gwr_mape,
      rmse          = gwr_rmse,
      shapiro       = gwr_shapiro,
      skew          = gwr_skew,
      kurt          = gwr_kurt,
      compTime      = time_taken["elapsed"],
      fct_globalMI  = fct_globalMI
    )
  )
}

