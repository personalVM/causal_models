


df

library(HSAR)
library(spatialreg)
?HSAR::sar()
install.packages("spdep")
library(spdep)
dfs_shp
nb <- poly2nb(dfs_shp, row.names = dfs_shp$cd_micro)
lw <- nb2listw(nb)

# neighbors <- poly2nb(your_spatial_data, queen = FALSE) 
# weights <- nb2listw(neighbors, style = "W")

indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + sg_region" ### Cool!!
indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + ln_inhabitants_pkm + sg_region" ### Cool!!
indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + sg_region"
indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + ln_exports_pc + sg_region"
indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + ln_labor_pc + sg_region" ### Cool!!
indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + ln_firms_pc + sg_region" ### Cool!!
indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + ln_firms_pkm + sg_region" ### Cool!!
indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + ln_labor_pc + dist_coast"
indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + ln_labor_pc + is_coastal_50km" ### Cool!!
indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + ln_labor_pc + is_coastal_50km + sg_region" ### Cool!!
emig_form = as.formula(paste0("ln_emig_total_pc  ~ ", indv))
imig_form = as.formula(paste0("ln_immigrants_pc  ~ ", indv))
internal_form = as.formula(paste0("ln_nao_naturais_da_unidade_da_federacao_pc  ~ ", indv))


library(spatialreg)
sar_model <- spatialreg::spautolm(emig_form, data = dfs_shp, listw = lw, family = "SAR")
sar_model <- spatialreg::spautolm(imig_form, data = dfs_shp, listw = lw, family = "SAR")
sar_model <- spatialreg::spautolm(internal_form, data = dfs_shp, listw = lw, family = "SAR")
summary(sar_model)

residuals <- residuals(sar_model)
tss <- sum((dfs_shp$ln_emig_total_pc - mean(dfs_shp$ln_emig_total_pc))^2)
tss <- sum((dfs_shp$ln_immigrants_pc - mean(dfs_shp$ln_emig_total_pc))^2)
rss <- sum(residuals^2)
r_squared <- 1 - (rss / tss)
n <- length(residuals)  # Number of observations
k <- length(sar_model$coefficients) - 1  # Number of predictors (excluding the intercept)
adjusted_r_squared <- 1 - ((1 - r_squared) * (n - 1)) / (n - k - 1)

cat("Adjusted R-squared:", adjusted_r_squared, "\n")
