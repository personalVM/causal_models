

fct_sar = function(sar_form, data, weights){
  
  sar_model <- spatialreg::spautolm(sar_form, data = data, listw = weights, family = "SAR")
  sar_summary <- summary(sar_model)
  
  return(
    list(
      model   = sar_model,
      summary = sar_summary
    )
  )
}




# sar_form = as.formula(paste0("ln_emig_total_pc  ~ ", indv))
# y = dfs_shp$ln_emig_total_pc
# imig_form = as.formula(paste0("ln_immigrants_pc  ~ ", indv))
# y = dfs_shp$ln_immigrants_pc
# internal_form = as.formula(paste0("ln_nao_naturais_da_unidade_da_federacao_pc  ~ ", indv))
# y = dfs_shp$ln_nao_naturais_da_unidade_da_federacao_pc


# (emig_sar_model <- spatialreg::spautolm(emig_form, data = dfs_shp, listw = lw, family = "SAR"))
# (imig_sar_model <- spatialreg::spautolm(imig_form, data = dfs_shp, listw = lw, family = "SAR"))
# (internalMigration_sar_model <- spatialreg::spautolm(internal_form, data = dfs_shp, listw = lw, family = "SAR"))

# (emig_sar_summary <- summary(emig_sar_model))
# (imig_sar_summary <- summary(imig_sar_model))
# (internalMigration_sar_summary <- summary(internalMigration_sar_model))


# emig_sar = emig_sar_model
# fitted_values <- fitted(sar_model)
# residuals <- residuals(sar_model)
# TSS <- sum((y - mean(y))^2)
# RSS <- sum(residuals^2)
# pseudo_r2 <- 1 - (RSS / TSS)
# pseudo_r2
# 
# 
# actual_values <- fitted_values + residuals
# mape <- mean(abs((actual_values - fitted_values) / actual_values)) * 100
# print(paste("MAPE:", mape))
# 
# 
# residuals <- residuals(sar_model)
# tss <- sum((dfs_shp$ln_emig_total_pc - mean(dfs_shp$ln_emig_total_pc))^2)
# tss <- sum((dfs_shp$ln_immigrants_pc - mean(dfs_shp$ln_emig_total_pc))^2)
# rss <- sum(residuals^2)
# r_squared <- 1 - (rss / tss)
# n <- length(residuals)  # Number of observations
# k <- length(sar_model$coefficients) - 1  # Number of predictors (excluding the intercept)
# adjusted_r_squared <- 1 - ((1 - r_squared) * (n - 1)) / (n - k - 1)
# 
# cat("Adjusted R-squared:", adjusted_r_squared, "\n")




