

library(dplyr)
library(readr)
exported_table2 <- read_csv("volume/data/golden_data/micro/exported-table2.csv")
# View(exported_table2)
df <- exported_table2
# df$

# formula_vars = "ICE ~ ln_HABES_pc + ln_PIB_pc + ln_EMIG_pc + ln_IMIG_pc + dist_costa + rg_"
# formula_vars = "emig_total_pc ~ foreigners_pc + gdp_pc + inhabitants_with_higherEducation_total_pc + mean_salary_total + salary_total_pc + labor_total_pc + mean_salary_total_pkm"
formula_vars = "emig_total_pc ~ gdp_pc + inhabitants_with_higherEducation_total_pc + labor_total_pkm + mean_salary_total + inhabitants_pkm"
formula_vars = "emig_total_pc ~ gdp_pc + labor_total_pkm + mean_salary_total + inhabitants_pkm"
formula_vars = "immigrants_pc ~ gdp_pc + inhabitants_with_higherEducation_total_pc + labor_total_pkm + mean_salary_total + inhabitants_pkm"
formula_vars = "immigrants_pc ~ gdp_pc + labor_total_pkm + mean_salary_total + inhabitants_pkm"
formula_vars = "immigrants_pc ~ labor_total_pc"
formula_vars = "gdp_pc ~ immigrants_pc + labor_total_pkm + mean_salary_total + inhabitants_pkm"
# formula_vars = "immigrants_pc ~ mean_salary_total"
df <- df %>% filter(cd_micro != 26019) # "Fernando de Noronha"
lm <- lm(formula = formula_vars, data = df)
predictions <- predict(lm)
residuals <- as.numeric(lm$residuals)
df$lm_pred = predictions
df$lm_res = residuals
model <- lm$model
(slm <- summary(lm))
lm_pred <- as.numeric(predict(lm))
lm_mape <- MLmetrics::MAPE(lm_pred, df$ICE)
bp <- lmtest::bptest(lm) # Breusch–Pagan p-value low leads to Heteroscedasticity (H1: residuals are distributed with unequal variance)
aic <- AIC(lm)








