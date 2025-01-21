
# rm(list = ls())
gc()

source("volume/causal_models/util_loadPackages.R")
source("volume/causal_models/viz_map.R")

source("volume/causal_models/data_wrangler.R")

# OLS:
source("volume/causal_models/ols.R")

emig_ols = fct_ols(form = "ln_emig_pc  ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", dfs_shp)
emig_ols$summary
emig_ols$R2
emig_ols$R2adj
emig_ols$mape
emig_ols$rmse
emig_ols$fct_globalMI(var = "residuals")
# emig_ols$

imig_ols = fct_ols(form = "ln_imig_pc  ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", dfs_shp)
imig_ols$summary
imig_ols$R2
imig_ols$R2adj
imig_ols$mape
imig_ols$rmse
imig_ols$fct_globalMI(var = "residuals")


internalMigration_ols = fct_ols("ln_intraMig_pc  ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", dfs_shp)
internalMigration_ols$summary
internalMigration_ols$R2
internalMigration_ols$R2adj
internalMigration_ols$mape
internalMigration_ols$rmse
internalMigration_ols$fct_globalMI(var = "residuals")



# SAR:
source("volume/causal_models/sar.R")

emig_sar = fct_sar("ln_emig_pc  ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", dfs_shp, lw)
emig_sar$summary
emig_sar$r2
emig_sar$r2adj
emig_sar$rmse
emig_sar$mape
emig_sar$fct_globalMI(var = "residuals")

imig_sar = fct_sar("ln_imig_pc  ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", dfs_shp, lw)
imig_sar$summary
imig_sar$r2
imig_sar$r2adj
imig_sar$rmse
imig_sar$mape
imig_sar$fct_globalMI(var = "residuals")

internalMigration_sar = fct_sar("ln_intraMig_pc  ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", dfs_shp, lw)
internalMigration_sar$summary
internalMigration_sar$r2
internalMigration_sar$r2adj
internalMigration_sar$rmse
internalMigration_sar$mape
internalMigration_sar$fct_globalMI(var = "residuals")

# PS.: This can not be done:
# emig_ols = fct_ols("ln_emig_pc  ~ w_ln_emig_pc + mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", dfs_shp)
# emig_ols$ols_summary


# GWR:
source("volume/causal_models/gwr.R")
# rm(emig_gwr)
# emig_ols
emig_gwr = fct_gwr(
  gwr_form = "ln_emig_pc  ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_",
  data = sdp,
  weights = lw
)
emig_gwr$model
emig_gwr$df_gwr
emig_gwr$viz1(var_viz = "beta_mean_salary")
emig_gwr$viz_sig(var = "mean_salary_sig")
emig_gwr$viz1(var_viz = "beta_ln_higherEduc_pc")
emig_gwr$viz_sig(var = "ln_higherEduc_pc_sig")
emig_gwr$viz1(var_viz = "beta_ECI")
emig_gwr$viz_sig(var = "ECI_sig")
# emig_gwr$viz1(var_viz = "beta_is_coastal")
# emig_gwr$viz_sig(var = "is_coastal_sig")
emig_gwr$fct_globalMI(var="residuals")
emig_gwr$table(localTable_path = "volume/causal_models/tables/emig_gwr.html")

imig_ols
imig_gwr = fct_gwr(
  gwr_form = "ln_imig_pc  ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", 
  data = sdp, 
  weights = lw
)
imig_gwr$model
imig_gwr$viz1(var_viz = "beta_mean_salary")
imig_gwr$viz_sig(var = "mean_salary_sig")
imig_gwr$viz1(var_viz = "beta_ln_higherEduc_pc")
imig_gwr$viz_sig(var = "ln_higherEduc_pc_sig")
imig_gwr$viz1(var_viz = "beta_ECI")
imig_gwr$viz_sig(var = "ECI_sig")
# imig_gwr$viz1(var_viz = "beta_is_coastal")
# imig_gwr$viz_sig(var = "is_coastal_sig")
imig_gwr$fct_globalMI(var="residuals")
imig_gwr$table(localTable_path = "volume/causal_models/tables/imig_gwr.html")

# intraMig_ols
intraMig_gwr = fct_gwr(
  gwr_form = "ln_intraMig_pc  ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", 
  data = sdp, 
  weights = lw)
intraMig_gwr$model
intraMig_gwr$viz1(var_viz = "beta_mean_salary")
intraMig_gwr$viz_sig(var = "mean_salary_sig")
intraMig_gwr$viz1(var_viz = "beta_ln_higherEduc_pc")
intraMig_gwr$viz_sig(var = "ln_higherEduc_pc_sig")
intraMig_gwr$viz1(var_viz = "beta_ECI")
intraMig_gwr$viz_sig(var = "ECI_sig")
# intraMig_gwr$viz1(var_viz = "beta_is_coastal")
# intraMig_gwr$viz_sig(var = "is_coastal_sig")
intraMig_gwr$fct_globalMI(var = "beta_ECI")
intraMig_gwr$fct_globalMI(var="residuals")
intraMig_gwr$table(localTable_path = "volume/causal_models/tables/intraMig_gwr.html")

# GWAR:
# skip

# MGWR
source("volume/causal_models/fct_mgwr.R")
emig_mgwr <- fct_MGWR(
  form = as.formula("ln_emig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
  data=dfs_shp,
  fixedv = c("Intercept", "is_coastal", "region_North", "region_Northeast", "region_South", "region_Southeast")
)
mgwrsar::summary_mgwrsar(emig_mgwr$model)
viz_map(data=emig_mgwr$df_mgwr, var_viz = "betav_mean_salary")
viz_map(data=emig_mgwr$df_mgwr, var_viz = "betav_ln_higherEduc_pc")
viz_map(data=emig_mgwr$df_mgwr, var_viz = "betav_ECI")
emig_mgwr$fct_globalMI(var = "residuals")
emig_mgwr$fct_globalMI(var = "betav_ECI")
emig_mgwr$rmse
emig_mgwr$r_squared
emig_mgwr$adj_r_squared
emig_mgwr$mape


imig_mgwr <- fct_MGWR(
  form = as.formula("ln_imig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
  data=dfs_shp,
  fixedv = c("Intercept", "is_coastal", "region_North", "region_Northeast", "region_South", "region_Southeast")
)
mgwrsar::summary_mgwrsar(imig_mgwr$model)
viz_map(data=imig_mgwr$df_mgwr, var_viz = "betav_mean_salary")
viz_map(data=imig_mgwr$df_mgwr, var_viz = "betav_ln_higherEduc_pc")
viz_map(data=imig_mgwr$df_mgwr, var_viz = "betav_ECI")
imig_mgwr$fct_globalMI(var = "residuals")
imig_mgwr$fct_globalMI(var = "betav_ECI")
imig_mgwr$rmse
imig_mgwr$r_squared
imig_mgwr$adj_r_squared
imig_mgwr$mape


intraMig_mgwr <- fct_MGWR(
  form = as.formula("ln_intraMig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
  data=dfs_shp,
  fixedv = c("Intercept", "is_coastal", "region_North", "region_Northeast", "region_South", "region_Southeast")
)
mgwrsar::summary_mgwrsar(intraMig_mgwr$model)
viz_map(data=intraMig_mgwr$df_mgwr, var_viz = "betav_mean_salary")
viz_map(data=intraMig_mgwr$df_mgwr, var_viz = "betav_ln_higherEduc_pc")
viz_map(data=intraMig_mgwr$df_mgwr, var_viz = "betav_ECI")
intraMig_mgwr$fct_globalMI(var = "residuals")
intraMig_mgwr$fct_globalMI(var = "betav_ECI")
intraMig_mgwr$rmse
intraMig_mgwr$r_squared
intraMig_mgwr$adj_r_squared
intraMig_mgwr$mape


# MGWRSAR
source("volume/causal_models/fct_mgwrsar.R")
emig_mgwar <- fct_MGWRSAR(
  form = as.formula("ln_emig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
  data=dfs_shp,
  fixedv = c("Intercept", "is_coastal", "region_North", "region_Northeast", "region_South", "region_Southeast")
)
mgwrsar::summary_mgwrsar(emig_mgwar$model)
viz_map(data=emig_mgwar$df_mgwar, var_viz = "betav_mean_salary")
viz_map(data=emig_mgwar$df_mgwar, var_viz = "betav_ln_higherEduc_pc")
viz_map(data=emig_mgwar$df_mgwar, var_viz = "betav_ECI")
viz_map(data=emig_mgwar$df_mgwar, var_viz = "betav_lambda")

imig_mgwar <- fct_MGWRSAR(
  form = as.formula("ln_imig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
  data=dfs_shp,
  fixedv = c("Intercept", "is_coastal", "region_North", "region_Northeast", "region_South", "region_Southeast")
)
mgwrsar::summary_mgwrsar(imig_mgwar$model)
viz_map(data=imig_mgwar$df_mgwar, var_viz = "betav_mean_salary")
viz_map(data=imig_mgwar$df_mgwar, var_viz = "betav_ln_higherEduc_pc")
viz_map(data=imig_mgwar$df_mgwar, var_viz = "betav_ECI")
viz_map(data=imig_mgwar$df_mgwar, var_viz = "betav_lambda")

intraMig_mgwar <- fct_MGWRSAR(
  form = as.formula("ln_intraMig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
  data=dfs_shp,
  fixedv = c("Intercept", "is_coastal", "region_North", "region_Northeast", "region_South", "region_Southeast")
)
mgwrsar::summary_mgwrsar(intraMig_mgwar$model)
viz_map(data=intraMig_mgwar$df_mgwar, var_viz = "betav_mean_salary")
viz_map(data=intraMig_mgwar$df_mgwar, var_viz = "betav_ln_higherEduc_pc")
viz_map(data=intraMig_mgwar$df_mgwar, var_viz = "betav_ECI")
viz_map(data=intraMig_mgwar$df_mgwar, var_viz = "betav_lambda")






