
# rm(list = ls())
gc()
# install.packages("paletteer")
library(paletteer)

source("volume/causal_models/util_loadPackages.R")
source("volume/causal_models/viz_map.R")

source("volume/causal_models/data_wrangler.R")
source("volume/causal_models/viz_map2.R")
# viz_map2(data=dfs_shp %>% sf::st_as_sf(), varviz="emig_pc", titleviz="", legendviz="", paletteer=1)
# viz_map2(data=dfs_shp %>% sf::st_as_sf(), varviz="ln_emig_pc", titleviz="", legendviz="ln_emig_pc", paletteer=1)
# viz_map2(data=dfs_shp %>% sf::st_as_sf(), varviz="ln_imig_pc", titleviz="", legendviz="ln_emig_pc", paletteer=1)
# viz_map2(data=dfs_shp %>% sf::st_as_sf(), varviz="ln_intraMig_pc", titleviz="", legendviz="ln_emig_pc", paletteer=1)

fct_globalMI <- function(m1var="ECI"){
  spdep::moran.test(dfs_shp[[m1var]], listw = lw)
  m1=spdep::moran.mc(dfs_shp[[m1var]], lw, nsim=599)$statistic %>% as.numeric(.)
  return(m1)
}
# fct_globalMI(m1var="emig_pc")
# fct_globalMI(m1var="imig_pc")
# fct_globalMI(m1var="intraMig_pc")

# fct_localMI(dfs_shp %>% sf::st_as_sf(), lw=lw, varviz="emig_pc", titleviz="", legendviz="", sig_alpha=0.35)
# fct_localMI(dfs_shp %>% sf::st_as_sf(), lw=lw, varviz="imig_pc", titleviz="", legendviz="", sig_alpha=0.35)
# fct_localMI(dfs_shp %>% sf::st_as_sf(), lw=lw, varviz="intraMig_pc", titleviz="", legendviz="", sig_alpha=0.35)


# OLS:
source("volume/causal_models/ols.R")
emig_ols = fct_ols(
  form = "ln_emig_pc  ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", 
  data = dfs_shp
)
# emig_ols = fct_ols(form = "ln_emig_pc  ~ w_ln_emig_pc + mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", dfs_shp)
# emig_ols$summary
# emig_ols$R2
# emig_ols$R2adj
# emig_ols$mape
# emig_ols$rmse
# emig_ols$skew
# emig_ols$kurt
# emig_ols$shapiro
# emig_ols$fct_globalMI(var = "residuals")
# emig_ols$

imig_ols = fct_ols(
  form = "ln_imig_pc  ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", 
  data = dfs_shp
)
# imig_ols$summary
# imig_ols$R2
# imig_ols$R2adj
# imig_ols$mape
# imig_ols$rmse
# imig_ols$skew
# imig_ols$kurt
# imig_ols$shapiro
# imig_ols$fct_globalMI(var = "residuals")

intraMig_ols = fct_ols(
  form = "ln_intraMig_pc  ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", 
  data = dfs_shp
)
# intraMig_ols$summary
# internalMigration_ols$R2
# internalMigration_ols$R2adj
# internalMigration_ols$mape
# internalMigration_ols$rmse
# internalMigration_ols$skew
# internalMigration_ols$kurt
# internalMigration_ols$shapiro
# internalMigration_ols$fct_globalMI(var = "residuals")


# SAR:
source("volume/causal_models/sar.R")
emig_sar = fct_sar("ln_emig_pc  ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", dfs_shp, lw)
# emig_sar$summary
# emig_sar$r2
# emig_sar$r2adj
# emig_sar$rmse
# emig_sar$mape
# emig_sar$skew
# emig_sar$kurt
# emig_sar$shapiro
# emig_sar$fct_globalMI(var = "residuals")

imig_sar = fct_sar("ln_imig_pc  ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", dfs_shp, lw)
# imig_sar$summary
# imig_sar$r2
# imig_sar$r2adj
# imig_sar$rmse
# imig_sar$mape
# imig_sar$skew
# imig_sar$kurt
# imig_sar$shapiro
# imig_sar$fct_globalMI(var = "residuals")

intraMig_sar = fct_sar("ln_intraMig_pc  ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", dfs_shp, lw)
# intraMig_sar$summary
# internalMigration_sar$r2
# internalMigration_sar$r2adj
# internalMigration_sar$rmse
# internalMigration_sar$mape
# internalMigration_sar$skew
# internalMigration_sar$kurt
# internalMigration_sar$shapiro
# internalMigration_sar$fct_globalMI(var = "residuals")

# PS.: This can not be done:
# emig_ols = fct_ols("ln_emig_pc  ~ w_ln_emig_pc + mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", dfs_shp)
# emig_ols$ols_summary
# Otherwise Wy tends to 1.
# Thus, Log Likelihood must be used

# GWR:
source("volume/causal_models/fct_gwr_q.R")
source("volume/causal_models/fct_gwr.R")
emig_gwr_q = fct_gwr_q(
    form    = as.formula("ln_emig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
    dep_var = "ln_emig_pc",
    data    = dfs_shp
)
emig_gwr = fct_GWR(
  form    = as.formula("ln_emig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
  dep_var = "ln_emig_pc",
  data    = dfs_shp
)
# emig_gwr$model
# emig_gwr$df_gwr
# emig_gwr$viz1(var_viz = "beta_mean_salary")
# emig_gwr$viz_sig(var = "mean_salary_sig")
# emig_gwr$viz1(var_viz = "beta_ln_higherEduc_pc")
# emig_gwr$viz_sig(var = "ln_higherEduc_pc_sig")
# emig_gwr$viz1(var_viz = "beta_ECI")
# emig_gwr$viz_sig(var = "ECI_sig")
# emig_gwr$viz1(var_viz = "beta_is_coastal")
# emig_gwr$viz_sig(var = "is_coastal_sig")
# emig_gwr$r_squared
# emig_gwr$adj_r_squared
# emig_gwr$MAPE
# emig_gwr$rmse
# emig_gwr$shapiro
# emig_gwr$skew
# emig_gwr$kurt
# emig_gwr$fct_globalMI(var="residuals")
# emig_gwr$table(localTable_path = "volume/causal_models/tables/emig_gwr.html")

imig_gwr_q = fct_gwr_q(
  form    = as.formula("ln_imig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
  dep_var = "ln_imig_pc",
  data    = dfs_shp
)
imig_gwr = fct_GWR(
  form    = as.formula("ln_imig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
  dep_var = "ln_imig_pc",
  data    = dfs_shp
)

# imig_gwr$model
# imig_gwr$viz1(var_viz = "beta_mean_salary")
# imig_gwr$viz_sig(var = "mean_salary_sig")
# imig_gwr$viz1(var_viz = "beta_ln_higherEduc_pc")
# imig_gwr$viz_sig(var = "ln_higherEduc_pc_sig")
# imig_gwr$viz1(var_viz = "beta_ECI")
# imig_gwr$viz_sig(var = "ECI_sig")
# imig_gwr$viz1(var_viz = "beta_is_coastal")
# imig_gwr$viz_sig(var = "is_coastal_sig")
# imig_gwr$shapiro
# imig_gwr$skew
# imig_gwr$kurt
# imig_gwr$fct_globalMI(var="residuals")
# MLmetrics::MAPE(imig_gwr$df_gwr$pred, dfs_shp$ln_imig_pc)
# imig_gwr$table(localTable_path = "volume/causal_models/tables/imig_gwr.html")

intraMig_gwr_q = fct_gwr_q(
  form    = as.formula("ln_intraMig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
  dep_var = "ln_intraMig_pc",
  data    = dfs_shp
)
intraMig_gwr = fct_GWR(
  form    = as.formula("ln_intraMig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
  dep_var = "ln_intraMig_pc",
  data    = dfs_shp
)

# intraMig_gwr$model
# intraMig_gwr$viz1(var_viz = "beta_mean_salary")
# intraMig_gwr$viz_sig(var = "mean_salary_sig")
# intraMig_gwr$viz1(var_viz = "beta_ln_higherEduc_pc")
# intraMig_gwr$viz_sig(var = "ln_higherEduc_pc_sig")
# intraMig_gwr$viz1(var_viz = "beta_ECI")
# intraMig_gwr$viz_sig(var = "ECI_sig")
# intraMig_gwr$viz1(var_viz = "beta_is_coastal")
# intraMig_gwr$viz_sig(var = "is_coastal_sig")
# intraMig_gwr$fct_globalMI(var = "beta_ECI")
# intraMig_gwr$shapiro
# intraMig_gwr$skew
# intraMig_gwr$kurt
# intraMig_gwr$fct_globalMI(var="residuals")
# intraMig_gwr$table(localTable_path = "volume/causal_models/tables/intraMig_gwr.html")

# GWAR:
# skip

# MixedGWR
source("volume/causal_models/fct_mixedgwr.R")
emig_mixedgwr <- fct_MixedGWR(
  form = as.formula("ln_emig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
  dep_var="ln_emig_pc",
  data=dfs_shp,
  fixedv = c("Intercept", "is_coastal", "region_North", "region_Northeast", "region_South", "region_Southeast")
)
# mgwrsar::summary_mgwrsar(emig_mixedgwr$model)
# viz_map(data=emig_mixedgwr$df_mgwr, var_viz = "betav_mean_salary")
# viz_map(data=emig_mixedgwr$df_mgwr, var_viz = "betav_ln_higherEduc_pc")
# viz_map(data=emig_mixedgwr$df_mgwr, var_viz = "betav_ECI")
# emig_mixedgwr$fct_globalMI(var = "residuals")
# emig_mixedgwr$fct_globalMI(var = "betav_ECI")
# emig_mixedgwr$rmse
# emig_mixedgwr$r_squared
# emig_mixedgwr$adj_r_squared
# emig_mixedgwr$mape
# emig_mixedgwr$shapiro
# emig_mixedgwr$skew
# emig_mixedgwr$kurt

imig_mixedgwr <- fct_MixedGWR(
  form = as.formula("ln_imig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
  dep_var="ln_imig_pc",
  data=dfs_shp,
  fixedv = c("Intercept", "is_coastal", "region_North", "region_Northeast", "region_South", "region_Southeast")
)
# mgwrsar::summary_mgwrsar(imig_mixedgwr$model)
# viz_map(data=imig_mixedgwr$df_mgwr, var_viz = "betav_mean_salary")
# viz_map(data=imig_mixedgwr$df_mgwr, var_viz = "betav_ln_higherEduc_pc")
# viz_map(data=imig_mixedgwr$df_mgwr, var_viz = "betav_ECI")
# imig_mixedgwr$fct_globalMI(var = "residuals")
# imig_mixedgwr$fct_globalMI(var = "betav_ECI")
# imig_mixedgwr$rmse
# imig_mixedgwr$r_squared
# imig_mixedgwr$adj_r_squared
# imig_mixedgwr$mape
# imig_mixedgwr$mape
# imig_mixedgwr$shapiro
# imig_mixedgwr$skew
# imig_mixedgwr$kurt

intraMig_mixedgwr <- fct_MixedGWR(
  form = as.formula("ln_intraMig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
  dep_var="ln_intraMig_pc",
  data=dfs_shp,
  fixedv = c("Intercept", "is_coastal", "region_North", "region_Northeast", "region_South", "region_Southeast")
)
# mgwrsar::summary_mgwrsar(intraMig_mixedgwr$model)
# viz_map(data=intraMig_mixedgwr$df_mgwr, var_viz = "betav_mean_salary")
# viz_map(data=intraMig_mixedgwr$df_mgwr, var_viz = "betav_ln_higherEduc_pc")
# viz_map(data=intraMig_mixedgwr$df_mgwr, var_viz = "betav_ECI")
# intraMig_mixedgwr$fct_globalMI(var = "residuals")
# intraMig_mixedgwr$fct_globalMI(var = "betav_ECI")
# intraMig_mixedgwr$rmse
# intraMig_mixedgwr$r_squared
# intraMig_mixedgwr$adj_r_squared
# intraMig_mixedgwr$mape


# Multiscale-GWR
source("volume/causal_models/fct_multiscalegwr.R")
emig_multiscalegwr <- fct_MultiscaleGWR(
  form = as.formula("ln_emig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_South + region_Southeast + region_North + region_Northeast"), 
  dep_var="ln_emig_pc",
  data=dfs_shp,
  init='GWR',
  nstable=6,
  crit=0.01
)
# mgwrsar::summary_mgwrsar(emig_multiscalegwr$model)
# viz_map(data=emig_multiscalegwr$df, var_viz = "mean_salary.1")
# viz_map(data=emig_multiscalegwr$df, var_viz = "ln_higherEduc_pc.1")
# viz_map(data=emig_multiscalegwr$df, var_viz = "ECI.1")
# viz_map(data=emig_multiscalegwr$df, var_viz = "residuals")
# emig_multiscalegwr$fct_globalMI(var = "residuals")
# emig_multiscalegwr$fct_globalMI(var = "betav_ECI")
# emig_multiscalegwr$r_squared
# emig_multiscalegwr$adj_r_squared
# emig_multiscalegwr$mape
# emig_multiscalegwr$rmse
# emig_multiscalegwr$shapiro
# emig_multiscalegwr$skew
# emig_multiscalegwr$kurt

imig_multiscalegwr <- fct_MultiscaleGWR(
  form = as.formula("ln_imig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_South + region_Southeast + region_North + region_Northeast"), 
  dep_var="ln_imig_pc",
  data=dfs_shp,
  init='GWR',
  nstable=6,
  crit=0.01
)
# mgwrsar::summary_mgwrsar(imig_multiscalegwr$model)
# mgwrsar::summary_mgwrsar(imig_multiscalegwr$model)
# viz_map(data=imig_multiscalegwr$df, var_viz = "mean_salary.1")
# viz_map(data=imig_multiscalegwr$df, var_viz = "ln_higherEduc_pc.1")
# viz_map(data=imig_multiscalegwr$df, var_viz = "ECI.1")
# viz_map(data=imig_multiscalegwr$df, var_viz = "residuals")
# imig_multiscalegwr$fct_globalMI(var = "residuals")
# imig_multiscalegwr$fct_globalMI(var = "betav_ECI")
# imig_multiscalegwr$r_squared
# imig_multiscalegwr$adj_r_squared
# imig_multiscalegwr$mape
# imig_multiscalegwr$rmse
# imig_multiscalegwr$shapiro
# imig_multiscalegwr$skew
# imig_multiscalegwr$kurt

intraMig_multiscalegwr <- fct_MultiscaleGWR(
  form = as.formula("ln_intraMig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_South + region_Southeast + region_North + region_Northeast"), 
  dep_var="ln_intraMig_pc",
  data=dfs_shp,
  init='GWR',
  nstable=6,
  crit=0.01
)
# mgwrsar::summary_mgwrsar(intraMig_multiscalegwr$model)
# viz_map(data=intraMig_multiscalegwr$df, var_viz = "mean_salary.1")
# viz_map(data=intraMig_multiscalegwr$df, var_viz = "ln_higherEduc_pc.1")
# viz_map(data=intraMig_multiscalegwr$df, var_viz = "ECI.1")
# viz_map(data=intraMig_multiscalegwr$df, var_viz = "residuals")
# intraMig_multiscalegwr$fct_globalMI(var = "residuals")
# intraMig_multiscalegwr$fct_globalMI(var = "betav_ECI")
# intraMig_multiscalegwr$r_squared
# intraMig_multiscalegwr$adj_r_squared
# intraMig_multiscalegwr$mape
# intraMig_multiscalegwr$rmse
# intraMig_multiscalegwr$shapiro
# intraMig_multiscalegwr$skew
# intraMig_multiscalegwr$kurt


# MGWRSAR
# source("volume/causal_models/fct_mgwrsar.R")
# emig_mgwar <- fct_MGWRSAR(
#   form = as.formula("ln_emig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
#   data=dfs_shp,
#   fixedv = c("Intercept", "is_coastal", "region_North", "region_Northeast", "region_South", "region_Southeast")
# )
# mgwrsar::summary_mgwrsar(emig_mgwar$model)
# emig_mgwar$rmse
# emig_mgwar$r_squared
# emig_mgwar$adj_r_squared
# emig_mgwar$mape
# emig_mgwar$fct_globalMI(var="residuals")
# viz_map(data=emig_mgwar$df, var_viz = "betav_mean_salary")
# viz_map(data=emig_mgwar$df, var_viz = "betav_ln_higherEduc_pc")
# viz_map(data=emig_mgwar$df, var_viz = "betav_ECI")
# viz_map(data=emig_mgwar$df, var_viz = "lambda")
# 
# imig_mgwar <- fct_MGWRSAR(
#   form = as.formula("ln_imig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
#   data=dfs_shp,
#   fixedv = c("Intercept", "is_coastal", "region_North", "region_Northeast", "region_South", "region_Southeast")
# )
# mgwrsar::summary_mgwrsar(imig_mgwar$model)
# imig_mgwar$rmse
# imig_mgwar$r_squared
# imig_mgwar$adj_r_squared
# imig_mgwar$mape
# imig_mgwar$fct_globalMI(var="residuals")
# viz_map(data=imig_mgwar$df, var_viz = "betav_mean_salary")
# viz_map(data=imig_mgwar$df, var_viz = "betav_ln_higherEduc_pc")
# viz_map(data=imig_mgwar$df, var_viz = "betav_ECI")
# viz_map(data=imig_mgwar$df, var_viz = "lambda")
# 
# intraMig_mgwar <- fct_MGWRSAR(
#   form = as.formula("ln_intraMig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
#   data=dfs_shp,
#   fixedv = c("Intercept", "is_coastal", "region_North", "region_Northeast", "region_South", "region_Southeast")
# )
# mgwrsar::summary_mgwrsar(intraMig_mgwar$model)
# intraMig_mgwar$rmse
# intraMig_mgwar$r_squared
# intraMig_mgwar$adj_r_squared
# intraMig_mgwar$mape
# intraMig_mgwar$fct_globalMI(var="residuals")
# viz_map(data=intraMig_mgwar$df, var_viz = "betav_mean_salary")
# viz_map(data=intraMig_mgwar$df, var_viz = "betav_ln_higherEduc_pc")
# viz_map(data=intraMig_mgwar$df, var_viz = "betav_ECI")
# viz_map(data=intraMig_mgwar$df, var_viz = "lambda")

# Final Tables:

(final_table_emig = data.frame(
  R2=c(emig_ols$R2, emig_sar$r2, emig_gwr_q$r_squared, emig_gwr$r_squared, emig_mixedgwr$r_squared, emig_multiscalegwr$r_squared),
  R2adj=c(emig_ols$R2adj, emig_sar$r2adj, emig_gwr_q$adj_r_squared, emig_gwr$adj_r_squared, emig_mixedgwr$adj_r_squared, emig_multiscalegwr$adj_r_squared),
  MAPE=c(emig_ols$rmse, emig_sar$rmse, emig_gwr_q$rmse, emig_gwr$rmse, emig_mixedgwr$rmse, emig_multiscalegwr$rmse),
  RMSE=c(emig_ols$mape, emig_sar$mape, emig_gwr_q$mape, emig_gwr$mape, emig_mixedgwr$mape, emig_multiscalegwr$mape),
  MI_res=c(emig_ols$fct_globalMI(var = "residuals")$mi, emig_sar$fct_globalMI(var = "residuals")$mi, emig_gwr_q$fct_globalMI(var="residuals")$mi, emig_gwr$fct_globalMI(var="residuals")$mi, emig_mixedgwr$fct_globalMI(var="residuals")$mi, emig_multiscalegwr$fct_globalMI(var="residuals")$mi),
  MI_sig=c(emig_ols$fct_globalMI(var = "residuals")$sig, emig_sar$fct_globalMI(var = "residuals")$sig, emig_gwr_q$fct_globalMI(var="residuals")$sig, emig_gwr$fct_globalMI(var="residuals")$sig, emig_mixedgwr$fct_globalMI(var="residuals")$sig, emig_multiscalegwr$fct_globalMI(var="residuals")$sig),
  row.names = c("OLS", "SAR", "GWRq", "GWR", "MixedGWR", "MultiscaleGWR")
))

(final_table_imig = data.frame(
  R2=c(imig_ols$R2, imig_sar$r2, imig_gwr_q$r_squared, imig_gwr$r_squared, imig_mixedgwr$r_squared, imig_multiscalegwr$r_squared),
  R2adj=c(imig_ols$R2adj, imig_sar$r2adj, imig_gwr_q$adj_r_squared, imig_gwr$adj_r_squared, imig_mixedgwr$adj_r_squared, imig_multiscalegwr$adj_r_squared),
  MAPE=c(imig_ols$rmse, imig_sar$rmse, imig_gwr_q$rmse, imig_gwr$rmse, imig_mixedgwr$rmse, imig_multiscalegwr$rmse),
  RMSE=c(imig_ols$mape, imig_sar$mape, imig_gwr_q$mape, imig_gwr$mape, imig_mixedgwr$mape, imig_multiscalegwr$mape),
  MI_res=c(imig_ols$fct_globalMI(var = "residuals")$mi, imig_sar$fct_globalMI(var = "residuals")$mi, imig_gwr_q$fct_globalMI(var="residuals")$mi, imig_gwr$fct_globalMI(var="residuals")$mi, imig_mixedgwr$fct_globalMI(var="residuals")$mi, imig_multiscalegwr$fct_globalMI(var="residuals")$mi),
  MI_sig=c(imig_ols$fct_globalMI(var = "residuals")$sig, imig_sar$fct_globalMI(var = "residuals")$sig, imig_gwr_q$fct_globalMI(var="residuals")$sig, imig_gwr$fct_globalMI(var="residuals")$sig, imig_mixedgwr$fct_globalMI(var="residuals")$sig, imig_multiscalegwr$fct_globalMI(var="residuals")$sig),
  row.names = c("OLS", "SAR", "GWRq", "GWR", "MixedGWR", "MultiscaleGWR")
))

(final_table_intraMig = data.frame(
  R2=c(intraMig_ols$R2, intraMig_sar$r2, intraMig_gwr_q$r_squared, intraMig_gwr$r_squared, intraMig_mixedgwr$r_squared, intraMig_multiscalegwr$r_squared),
  R2adj=c(intraMig_ols$R2adj, intraMig_sar$r2adj, intraMig_gwr_q$adj_r_squared, intraMig_gwr$adj_r_squared, intraMig_mixedgwr$adj_r_squared, intraMig_multiscalegwr$adj_r_squared),
  MAPE=c(intraMig_ols$rmse, intraMig_sar$rmse, intraMig_gwr_q$rmse, intraMig_gwr$rmse, intraMig_mixedgwr$rmse, intraMig_multiscalegwr$rmse),
  RMSE=c(intraMig_ols$mape, intraMig_sar$mape, intraMig_gwr_q$mape, intraMig_gwr$mape, intraMig_mixedgwr$mape, intraMig_multiscalegwr$mape),
  MI_res=c(intraMig_ols$fct_globalMI(var = "residuals")$mi, intraMig_sar$fct_globalMI(var = "residuals")$mi, intraMig_gwr_q$fct_globalMI(var="residuals")$mi, intraMig_gwr$fct_globalMI(var="residuals")$mi, intraMig_mixedgwr$fct_globalMI(var="residuals")$mi, intraMig_multiscalegwr$fct_globalMI(var="residuals")$mi),
  MI_sig=c(intraMig_ols$fct_globalMI(var = "residuals")$sig, intraMig_sar$fct_globalMI(var = "residuals")$sig, intraMig_gwr_q$fct_globalMI(var="residuals")$sig, intraMig_gwr$fct_globalMI(var="residuals")$sig, intraMig_mixedgwr$fct_globalMI(var="residuals")$sig, intraMig_multiscalegwr$fct_globalMI(var="residuals")$sig),
  row.names = c("OLS", "SAR", "GWRq", "GWR", "MixedGWR", "MultiscaleGWR")
))


