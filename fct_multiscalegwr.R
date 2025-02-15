# https://doi.org/10.1016/j.regsciurbeco.2017.04.001
# rm(list = ls())
gc()
source("volume/etl/util_loadPackages.R")

fct_MultiscaleGWR = function(
    form = as.formula("ln_emig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_South + region_Southeast + region_North + region_Northeast"), 
    dep_var,
    data,
    init='GWR',
    nstable=3,
    crit=0.1
){
  
  # install.packages("fastDummies")
  # library("fastDummies")
  # data=dfs_shp
  data$region = data$region_
  data <- fastDummies::dummy_cols(data, select_columns = "region", remove_first_dummy = FALSE)
  coords=as.matrix(data[,c("centlng", "centlat")] %>% as.data.frame())
  # data
  # form = as.formula("ln_emig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_South + region_Southeast + region_North + region_Northeast")
  mmultiscale <- mgwrsar::multiscale_gwr(
    formula=form,
    data=data,
    coords=coords,
    kernels='bisq',
    # kernels='gauss',
    ncore = 4,
    control=list(
      SE=FALSE,
      adaptive=TRUE,
      NN=100,
      isgcv=FALSE
    ),
    init=init,
    nstable=nstable,
    crit=crit
  )
  
  fit = mmultiscale$fitted %>% as.numeric()
  residuals = mmultiscale$residuals
  actual <- data[[all.vars(as.formula(form))[1]]]
  betav = mmultiscale$Betav
  colnames(betav) <- paste0("beta_", colnames(betav))
  
  df_mmultiscale <- cbind(dfs_shp, mmultiscale$Betav, fit, residuals, actual) %>%
    sf::st_sf(.) %>% 
    sf::st_set_crs(4326)
  
  mgwr_mape <- mean(abs((actual - fit) / actual)) * 100
  mgwr_rmse <- sqrt(mean((fit - actual)^2))
  mgwr_shapiro = shapiro.test(residuals)
  mgwr_skew = skewness(residuals)
  mgwr_kurt = kurtosis(residuals) 
  
  fct_globalMI <- function(var="betav_ECI"){
    mi  = spdep::moran.mc(df_mmultiscale[[var]], lw, nsim=599)$statistic %>% as.numeric(.)
    sig = spdep::moran.mc(df_mmultiscale[[var]], lw, nsim=599)$p.value %>% as.numeric(.)
    return(list(mi=mi, sig=sig))
  }
  # fct_globalMI(var="residuals")
  
  sst <- sum((actual - mean(actual))^2)
  sse <- sum((actual - fit)^2)
  r_squared <- 1 - (sse / sst)
  n <- length(actual)
  p <- df_mmultiscale[, grep("beta", colnames(df_mmultiscale))] %>% as.data.frame() %>% select(-geometry) %>% ncol() -1
  adj_r_squared <- 1 - ((1 - r_squared) * (n - 1) / (n - p - 1))
  
  return(
    list(
      df            = df_mmultiscale,
      model         = mmultiscale,
      r_squared     = r_squared,
      adj_r_squared = adj_r_squared,
      mape          = mgwr_mape,
      rmse          = mgwr_rmse,
      shapiro       = mgwr_shapiro,
      skew          = mgwr_skew,
      kurt          = mgwr_kurt,
      fct_globalMI  = fct_globalMI
    )
  )
}

