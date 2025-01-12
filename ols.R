


source("volume/etl/util_loadPackages.R")

exported_table3 <- readr::read_csv("volume/data/golden_data/micro/exported-table3.csv")

df <- exported_table3 %>% filter(cd_micro != 26019)

# Logarithm transformation # TODO: add this to the ELT process:
df$ln_emig_total_pc = log(df$emig_total_pc)
df$ln_immigrants_pc = log(df$immigrants_pc)
df$ln_nao_naturais_da_unidade_da_federacao = log(df$nao_naturais_da_unidade_da_federacao)
df$ln_nao_naturais_da_unidade_da_federacao_pc = log(df$nao_naturais_da_unidade_da_federacao / df$inhabitants)

df$ln_labor_total_pc = log(df$labor_total_pc)
df$ln_labor_total_pkm = log(df$labor_total_pkm)
df$ln_mean_salary_total = log(df$mean_salary_total)
df$ln_inhabitants_pkm = log(df$inhabitants_pkm)
df$ln_inhabitants_with_higherEducation_total = log(df$inhabitants_with_higherEducation_total)
df$ln_inhabitants_with_higherEducation_total_pc = log(df$inhabitants_with_higherEducation_total_pc)

df$ln_exports_pc = log(df$exports_pc)
df$ln_labor_pc = log(df$labor_total_pc)

df$ln_firms_pc = log(df$firms_total_pc)
df$ln_firms_pkm = log(df$firms_total_pkm)

df$ln_inhabitants_with_higherEducation_total_pc = log(df$inhabitants_with_higherEducation_total_pc)
df$ln_inhabitants_with_higherEducation_total_pc = log(df$inhabitants_with_higherEducation_total_pc)

rio::export(x = df, file = "volume/causal_models/treated_df.csv")


source("volume/etl/util_loadPackages.R")

# Load data:
## get treated data:
# treated_df <- readr::read_csv("volume/causal_models/treated_df.csv") %>% 
treated_df <- df %>% 
  dplyr::mutate(cd_micro=as.character(cd_micro))
df = treated_df
# treated_df$
  # exports
  # labor
  # firms
  # dist_coast
  # is_coastal_50km
# ln_exports_pc
# ln_labor_pc
# ln_firms_pc
# ln_firms_pkm
# ln_inhabitants_with_higherEducation_total_pc
# exports
# labor
# firms
# dist_coast
# is_coastal_50km


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
emig_form = as.formula(paste0("ln_emig_total_pc  ~ ", indv))
imig_form = as.formula(paste0("ln_immigrants_pc  ~ ", indv))
internal_form = as.formula(paste0("ln_nao_naturais_da_unidade_da_federacao_pc  ~ ", indv))
# formula <- as.formula(form)
# lm_model <- lm(formula, data = df)
(lm_summary <- summary(lm(emig_form, data = df)))
(lm_summary <- summary(lm(imig_form, data = df)))
(lm_summary <- summary(lm(internal_form, data = df)))




