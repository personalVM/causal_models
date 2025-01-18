


source("volume/etl/util_loadPackages.R")

treated_df
treated_df$

## Emigration model:
indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + ln_inhabitants_pkm + sg_region" ### Cool!!
indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + sg_region"
indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + ln_exports_pc + sg_region"
indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + ln_labor_pc + sg_region" ### Cool!!
indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + ln_firms_pc + sg_region" ### Cool!!
indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + ln_firms_pkm + sg_region" ### Cool!!
indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + ln_labor_pc + dist_coast"
indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + ln_labor_pc + is_coastal_50km" ### Cool!!
indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + ln_labor_pc + is_coastal_50km + sg_region" ### Cool!!
indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + eci_subn + is_coastal_50km + sg_region" ### Cool!!

indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + eci_subn + ln_labor_pc + is_coastal_50km + sg_region" ### Cool!!
indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + eci_subn + ln_firms_pkm + is_coastal_100km + sg_region" ### Cool!!
indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + eci_subn + ln_inhabitants_pkm + is_coastal_100km + sg_region" ### Cool!!

indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + eci_subn + ln_firms_pkm + ln_inhabitants_pkm + sg_region" ### Cool!!
indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + eci_subn + ln_inhabitants_pkm + sg_region" ### Cool!!

indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + eci_subn + is_coastal_100km + sg_region" ### Cool!!
indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + eci_subn + sg_region"
indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + eci_subn + sg_region"
indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + eci_subn"




indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + eci_subn + is_coastal_100km + sg_region" ### Cool!!
emig_form = as.formula(paste0("ln_emig_total_pc  ~ ", indv))
imig_form = as.formula(paste0("ln_immigrants_pc  ~ ", indv))
internal_form = as.formula(paste0("ln_nao_naturais_da_unidade_da_federacao_pc  ~ ", indv))
# formula <- as.formula(form)
# lm_model <- lm(formula, data = df)
(lm_summary <- summary(lm(emig_form, data = df)))
(lm_summary <- summary(lm(imig_form, data = df)))
(lm_summary <- summary(lm(internal_form, data = df)))




