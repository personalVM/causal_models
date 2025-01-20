
# rm(list = ls())
gc()

source("volume/causal_models/util_loadPackages.R")
source("volume/causal_models/viz_map.R")

source("volume/causal_models/data_wrangler.R")

# OLS:
source("volume/causal_models/ols.R")

emig_ols = fct_ols("ln_emig_pc  ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", dfs_shp)
emig_ols$ols_summary

imig_ols = fct_ols("ln_imig_pc  ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", dfs_shp)
imig_ols$ols_summary

internalMigration_ols = fct_ols("ln_intraMig_pc  ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", dfs_shp)
internalMigration_ols$ols_summary


# SAR:
source("volume/causal_models/sar.R")

emig_sar = fct_sar("ln_emig_pc  ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", dfs_shp, lw)
emig_sar$summary

imig_sar = fct_sar("ln_imig_pc  ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", dfs_shp, lw)
imig_sar$summary

internalMigration_sar = fct_sar("ln_intraMig_pc  ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", dfs_shp, lw)
internalMigration_sar$summary

# PS.: This can not be done:
# emig_ols = fct_ols("ln_emig_pc  ~ w_ln_emig_pc + mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", dfs_shp)
# emig_ols$ols_summary


# GWR:
source("volume/causal_models/gwr.R")

emig_gwr = fct_gwr("ln_emig_pc  ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", sdp, lw)
emig_gwr$model
emig_gwr$viz(var_viz = "mean_salary")
emig_gwr$viz(var_viz = "ln_higherEduc_pc")
emig_gwr$viz(var_viz = "ECI")
emig_gwr$summary

imig_gwr = fct_gwr("ln_imig_pc  ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", sdp, lw)
imig_gwr$model
imig_gwr$viz(var_viz = "mean_salary")
imig_gwr$viz(var_viz = "ln_higherEduc_pc")
imig_gwr$viz(var_viz = "ECI")
imig_gwr$summary

intraMig_gwr = fct_gwr("ln_intraMig_pc  ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", sdp, lw)
intraMig_gwr$model
intraMig_gwr$viz(var_viz = "mean_salary")
intraMig_gwr$viz(var_viz = "ln_higherEduc_pc")
intraMig_gwr$viz(var_viz = "ECI")
intraMig_gwr$summary


# GWAR:
# skip

# MGWR
source("volume/causal_models/fct_mgwr.R")
emig_mgwr <- fct_MGWR(
  mgwr_form = as.formula("ln_emig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
  data=dfs_shp,
  fixedv = c("Intercept", "is_coastal", "region_North", "region_Northeast", "region_South", "region_Southeast")
)
mgwrsar::summary_mgwrsar(emig_mgwr$model)
viz_map(data=emig_mgwr$df_mgwr, var_viz = "betav_mean_salary")
viz_map(data=emig_mgwr$df_mgwr, var_viz = "betav_ln_higherEduc_pc")
viz_map(data=emig_mgwr$df_mgwr, var_viz = "betav_ECI")

imig_mgwr <- fct_MGWR(
  mgwr_form = as.formula("ln_imig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
  data=dfs_shp,
  fixedv = c("Intercept", "is_coastal", "region_North", "region_Northeast", "region_South", "region_Southeast")
)
mgwrsar::summary_mgwrsar(imig_mgwr$model)
viz_map(data=imig_mgwr$df_mgwr, var_viz = "betav_mean_salary")
viz_map(data=imig_mgwr$df_mgwr, var_viz = "betav_ln_higherEduc_pc")
viz_map(data=imig_mgwr$df_mgwr, var_viz = "betav_ECI")

intraMig_mgwr <- fct_MGWR(
  mgwr_form = as.formula("ln_intraMig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"), 
  data=dfs_shp,
  fixedv = c("Intercept", "is_coastal", "region_North", "region_Northeast", "region_South", "region_Southeast")
)
mgwrsar::summary_mgwrsar(intraMig_mgwr$model)
viz_map(data=intraMig_mgwr$df_mgwr, var_viz = "betav_mean_salary")
viz_map(data=intraMig_mgwr$df_mgwr, var_viz = "betav_ln_higherEduc_pc")
viz_map(data=intraMig_mgwr$df_mgwr, var_viz = "betav_ECI")


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






