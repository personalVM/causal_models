# https://doi.org/10.1016/j.regsciurbeco.2017.04.001
# rm(list = ls())
gc()
source("volume/etl/util_loadPackages.R")

fct_MGWRSAR = function(
    form = as.formula("ln_emig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
    data, 
    fixedv = c("Intercept", "is_coastal", "region_North", "region_Northeast", "region_South", "region_Southeast")
    ){

    bw <- bandwidths_mgwrsar(
      formula            = form, 
      data               = data,
      coords             = as.matrix(data[,c("centlng", "centlat")]),
      fixed_vars         = fixedv,
      Models             = 'MGWRSAR',
      candidates_Kernels = 'bisq',
      control            = list(
        NN = nrow(data), 
        adaptive = TRUE
      ),
      control_search     = list()
    )

    W <- mgwrsar::kernel_matW(
      H        = bw$MGWRSAR_bisq_adaptive$config_model$H,
      kernels  = bw$MGWRSAR_bisq_adaptive$config_model$kernels,
      coord_i  = as.matrix(data[, c("centlng", "centlat")]),
      NN       = bw$MGWRSAR_bisq_adaptive$model$NN,
      adaptive = TRUE,
      diagnull = TRUE,
      rowNorm  = TRUE)
  
    mgwar <- MGWRSAR(
      formula    = form, 
      data       = data,
      coords     = as.matrix(data[,c("centlng", "centlat")]), 
      fixed_vars = fixedv,
      kernels    = bw$MGWRSAR_bisq_adaptive$config_model$kernels,
      H          = bw$MGWRSAR_bisq_adaptive$config_model$H, 
      Model      = 'MGWRSAR_1_kc_kv',
      control    = list(SE=FALSE,adaptive=TRUE,W=W))
    
    mgwar_fit = mgwar$fit
    mgwar_residuals = mgwar$residuals
    mgwar_betav = mgwar$Betav
    colnames(mgwar_betav) <- paste0("betav_", colnames(mgwar_betav))  
    mgwar_betac = t(as.data.frame(mgwar$Betac))
    colnames(mgwar_betac) <- paste0("betac_", colnames(mgwar_betac))  
    
    df_mgwar <- cbind(dfs_shp, mgwar_betav, mgwar_betac, mgwar_fit, mgwar_residuals) %>%
      sf::st_sf(.) %>% 
      sf::st_set_crs(4326)
    
  return(
    list(
      data      = mgwar$data,
      df_mgwar  = df_mgwar,
      model     = mgwar,
      rmse      = mgwar$RMSE,
      rmsen     = mgwar$RMSEn,
      etime     = mgwar$ctime
    )
  )
}

# MGWRSAR <- fct_MGWRSAR(
#     mgwar_form = as.formula("ln_emig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
#     data       = dfs_shp, 
#     fixedv     = c("Intercept", "is_coastal", "region_North", "region_Northeast", "region_South", "region_Southeast")
# )

# MGWRSAR


