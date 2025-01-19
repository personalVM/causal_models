
source("volume/etl/util_loadPackages.R")

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

# emig_ols = fct_ols("ln_emig_pc  ~ w_ln_emig_pc + mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", dfs_shp)
# emig_ols$ols_summary

emig_sar = fct_sar("ln_emig_pc  ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", dfs_shp, lw)
emig_sar$summary

imig_sar = fct_sar("ln_imig_pc  ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", dfs_shp, lw)
imig_sar$summary

internalMigration_sar = fct_sar("ln_intraMig_pc  ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", dfs_shp, lw)
internalMigration_sar$summary

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
source("volume/causal_models/gwr.R")

emig_gwr = fct_gwr("ln_emig_pc  ~ W_ln_emig_pc + mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", sdp, lw)
emig_gwr$model
emig_gwr$viz(var_viz = "W_ln_emig_pc")
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



