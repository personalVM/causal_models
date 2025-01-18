


source("volume/etl/util_loadPackages.R")

# df <- readr::read_csv("volume/data/golden_data/micro/golden_micro.csv")
# df <- df %>% dplyr::filter(cd_micro != 26019)

library(arrow)
df <- read_parquet("volume/causal_models/golden_micro") %>% 
  dplyr::filter(cd_micro != 26019) %>% 
  dplyr::mutate(cd_micro=as.character(cd_micro))

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

# rio::export(x = df, file = "volume/causal_models/treated_df.csv")
# 
# 
# source("volume/etl/util_loadPackages.R")

# Load data:
## get treated data:
# treated_df <- readr::read_csv("volume/causal_models/treated_df.csv") %>% 
dfl = df_locations_munic %>% select(-c(cd_munic, nm_munic, cd_rgime, nm_rgime, cd_rgint, nm_rgint)) %>% mutate(cd_micro=as.character(cd_micro)) %>% distinct()
treated_df <- left_join(dfl, df) %>% 
  dplyr::mutate(cd_micro=as.character(cd_micro))
df = treated_df


indv = "mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + eci_subn + is_coastal_100km + sg_region" ### Cool!!

