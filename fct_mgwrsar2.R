# https://doi.org/10.1016/j.regsciurbeco.2017.04.001
# rm(list = ls())
gc()
source("volume/etl/util_loadPackages.R")

fct_MGWRSAR = function(
    form = as.formula("ln_emig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
    data, 
    fixedv = c("Intercept", "is_coastal", "region_North", "region_Northeast", "region_South", "region_Southeast")
){
  
  bw <- mgwrsar::bandwidths_mgwrsar(
    formula            = form, 
    data               = data,
    coords             = as.matrix(data[,c("centlng", "centlat")]),
    fixed_vars         = fixedv,
    Models             = 'MGWRSAR_1_kv_kc',
    candidates_Kernels = c('gauss', 'bisq'), # bisq, gauss
    control            = list(
      NN = nrow(data), 
      adaptive = TRUE
    ),
    control_search     = list()
  )
  
  W_in2=kernel_matW(
    # H=nrow(data)-10,
    # H=bw$MGWRSAR_1_kv_kc_gauss_adaptive$config_model$H,
    # H=bw$MGWRSAR_1_kv_0_gauss_adaptive$config_model$H,
    # H=bw$rectangle_bisq_adaptive$config_model$H,
    # H=bw$MGWRSAR_1_kv_kc_bisq_adaptive$config_model$H,
    H=bw$MGWRSAR_1_kv_kc_bisq_adaptive$config_model$H,
    kernels='bisq',
    coord_i=as.matrix(data[,c("centlng", "centlat")]),
    NN=nrow(data),
    adaptive=TRUE,
    diagnull=TRUE,
    rowNorm=T
  )
  
  mgwar <- mgwrsar::MGWRSAR(
    formula = form, 
    data = data,
    coord=as.matrix(data[,c("centlng", "centlat")]), 
    fixed_vars=c("Intercept", "is_coastal", "region_North", "region_Northeast", "region_South", "region_Southeast"),
    # kernels=c('bisq'), # bisq, gauss
    kernels=c('gauss'), # bisq, gauss
    # H=11, 
    # H=bw$MGWRSAR_1_kv_kc_gauss_adaptive$config_model$H,
    # H=bw$MGWRSAR_1_kv_0_gauss_adaptive$config_model$H,
    # H=bw$rectangle_gauss_adaptive$config_model$H,
    H=bw$MGWRSAR_1_kv_kc_bisq_adaptive$config_model$H,
    # H=bw$MGWRSAR_1_kv_kc_bisq_adaptive$config_model$H,
    Model = 'MGWRSAR_1_kc_kv',
    control=list(W=W_in,adaptive=TRUE,isgcv=F)
  )
  summary_mgwrsar(mgwar)
  
  lambda<-MGWRSAR(formula = form,data = data,coord=as.matrix(data[,c("centlng", "centlat")]),fixed_vars=c("Intercept", "is_coastal", "region_North", "region_Northeast", "region_South", "region_Southeast"),kernels=c('gauss'),H=bw$MGWRSAR_1_kv_kc_gauss_adaptive$config_model$H,Model = 'MGWRSAR_1_kc_kv',control=list(W=W_in,adaptive=TRUE,isgcv=F))$Betav %>% 
    as.data.frame() %>% select(lambda)
  fit = mgwar$fit
  residuals = mgwar$residuals
  actual <- mgwar$data[[all.vars(as.formula(form))[1]]]
  betav = mgwar$Betav
  colnames(betav) <- paste0("betav_", colnames(betav))  
  betac = t(as.data.frame(mgwar$Betac))
  colnames(betac) <- paste0("betac_", colnames(betac))
  
  df_mgwar <- cbind(dfs_shp, betav, betac, lambda, fit, residuals, actual) %>%
    sf::st_sf(.) %>% 
    sf::st_set_crs(4326)
  
  fct_globalMI <- function(var="betav_ECI"){
    mi  = spdep::moran.mc(df_mgwar[[var]], lw, nsim=599)$statistic %>% as.numeric(.)
    sig = spdep::moran.mc(df_mgwar[[var]], lw, nsim=599)$p.value %>% as.numeric(.)
    return(list(mi=mi, sig=sig))
  }
  # fct_globalMI(var="residuals")
  
  sst <- sum((actual - mean(actual))^2)
  sse <- sum((actual - fit)^2)
  r_squared <- 1 - (sse / sst)
  n <- length(actual)
  
  p <- df_mgwar[, grep("beta", colnames(df_mgwar))] %>% as.data.frame() %>% select(-geometry) %>% ncol() -1
  adj_r_squared <- 1 - ((1 - r_squared) * (n - 1) / (n - p - 1))
  rmse <- sqrt(mean((actual - fit)^2))
  mape <- mean(abs((actual - fit) / actual)) * 100
  
  return(
    list(
      df            = df_mgwar,
      model         = mgwar,
      summary       = summary_mgwrsar(mgwar),
      rmse          = rmse,
      r_squared     = r_squared,
      adj_r_squared = adj_r_squared,
      mape          = mape,
      fct_globalMI  = fct_globalMI
    )
  )
}

# MGWRSAR <- fct_MGWRSAR(
#     mgwar_form = as.formula("ln_emig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
#     data       = dfs_shp, 
#     fixedv     = c("Intercept", "is_coastal", "region_North", "region_Northeast", "region_South", "region_Southeast")
# )

# MGWRSAR


# test_data <- data.frame(
#   centlng = runif(10, -180, 180),
#   centlat = runif(10, -90, 90),
#   var1 = rnorm(10),
#   var2 = rnorm(10)
# )
# coords <- as.matrix(test_data[, c("centlng", "centlat")])
# fixed_vars <- c("var1", "var2")
# bw <- bandwidths_mgwrsar(
#   formula = var1 ~ var2,
#   data = test_data,
#   coords = coords,
#   fixed_vars = fixed_vars,
#   Models = 'MGWRSAR_1_kv_kc',
#   candidates_Kernels = c('gauss', 'bisq'),
#   control = list(NN = nrow(test_data), adaptive = TRUE),
#   control_search = list()
# )
