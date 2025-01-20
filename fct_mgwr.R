# https://doi.org/10.1016/j.regsciurbeco.2017.04.001
# rm(list = ls())
gc()
source("volume/etl/util_loadPackages.R")

fct_MGWR = function(
    mgwr_form = as.formula("ln_emig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
    data, 
    fixedv = c("Intercept", "is_coastal", "region_North", "region_Northeast", "region_South", "region_Southeast")
){
  
  bw <- bandwidths_mgwrsar(
    formula            = mgwr_form, 
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
  
  mgwr <- MGWRSAR(
    formula    = mgwr_form, 
    data       = data,
    coords     = as.matrix(data[,c("centlng", "centlat")]), 
    fixed_vars = fixedv,
    kernels    = bw$MGWR_bisq_adaptive$config_model$kernels,
    H          = bw$MGWR_bisq_adaptive$config_model$H, 
    Model      = 'MGWR',
    control    = list(SE=FALSE,adaptive=TRUE,W=W)
  )
  
  mgwr_fit = mgwr$fit
  mgwr_residuals = mgwr$residuals
  mgwr_betav = mgwr$Betav
  colnames(mgwr_betav) <- paste0("betav_", colnames(mgwr_betav))  
  mgwr_betac = t(as.data.frame(mgwr$Betac))
  colnames(mgwr_betac) <- paste0("betac_", colnames(mgwr_betac))  
  
  df_mgwr <- cbind(dfs_shp, mgwr_betav, mgwr_betac, mgwr_fit, mgwr_residuals) %>%
    sf::st_sf(.) %>% 
    sf::st_set_crs(4326)
  
  return(
    list(
      df_mgwr   = df_mgwr,
      model     = mgwr,
      summary   = summary_mgwrsar(mgwr),
      rmse      = mgwr$RMSE,
      rmsen     = mgwr$RMSEn,
      ssr       = mgwr$SSR,
      etime     = mgwr$ctime
    )
  )
}

# MGWRSAR <- fct_MGWRSAR(
#     mgwar_form = as.formula("ln_emig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
#     data       = dfs_shp, 
#     fixedv     = c("Intercept", "is_coastal", "region_North", "region_Northeast", "region_South", "region_Southeast")
# )

# MGWRSAR


