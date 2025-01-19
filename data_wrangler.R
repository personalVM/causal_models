

df <- read_parquet("volume/data/golden_data/micro/golden_micro") %>% 
  dplyr::filter(cd_micro != 26019) %>% 
  dplyr::mutate(cd_micro=as.character(cd_micro))

df_locations_micro <- readr::read_csv("volume/data/curated_data/micro/df_locations_micro.csv") %>% mutate(cd_micro=as.character(cd_micro))
df <- left_join(df_locations_micro, df) %>% 
  dplyr::mutate(cd_micro=as.character(cd_micro)) %>% 
  dplyr::filter(cd_micro != 26019)
# df

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

# ---------- Spatial data:

dfs_shp <- sf::st_read(paste0("volume/data/clean_data/micro/shp/")) %>%
  janitor::clean_names() %>%
  mutate(across(where(is.numeric), as.character)) %>%
  select(cd_micro) %>% 
  sf::st_set_crs(4326) %>% 
  left_join(df, .) %>% 
  filter(cd_micro != 26019) %>% 
  sf::st_sf()

nb <- poly2nb(dfs_shp, row.names = dfs_shp$cd_micro)
lw <- nb2listw(nb)
print("I am Ready!")

library(spdep)
dfs_shp$Wy_ln_emig_total_pc <- lag.listw(lw, dfs_shp$ln_emig_total_pc)
dfs_shp$Wy_ln_immigrants_pc <- lag.listw(lw, dfs_shp$ln_immigrants_pc)
dfs_shp$Wy_ln_nao_naturais_da_unidade_da_federacao_pc <- lag.listw(lw, dfs_shp$ln_nao_naturais_da_unidade_da_federacao_pc)

fct_centAsCols <- function(polygonx, names = c("centlat", "centlng")){
  centroids <- do.call(rbind, sf::st_centroid(polygonx$geometry)) %>% 
    tibble::as_tibble() %>% stats::setNames(c(names[1],names[2])) %>% dplyr::bind_cols(polygonx, ., .name_repair = "unique")
  return(centroids)
}
dfs_shp <- fct_centAsCols(dfs_shp)

# emig_form = "ln_emig_pc ~ mean_salary + ln_popWithHigherEduc_pc + eci + sg_region"
dfs_shp <- dfs_shp %>%
  mutate(nm_region_en = case_when(
    nm_region == "Sul" ~ "South",
    nm_region == "Sudeste" ~ "Southeast",
    nm_region == "Centro-Oeste" ~ "Midwest",
    nm_region == "Norte" ~ "North",
    nm_region == "Nordeste" ~ "Northeast",
    TRUE ~ NA_character_ # Handle unexpected values
  ))


dfs_shp=data.frame(
  cd_micro                = dfs_shp$cd_micro,
  ln_emig_pc              = dfs_shp$ln_emig_total_pc,
  w_ln_emig_pc            = dfs_shp$Wy_ln_emig_total_pc,
  ln_imig_pc              = dfs_shp$ln_immigrants_pc,
  w_ln_imig_pc            = dfs_shp$Wy_ln_immigrants_pc,
  ln_intraMig_pc          = dfs_shp$ln_nao_naturais_da_unidade_da_federacao_pc,
  w_ln_intraMig_pc        = dfs_shp$Wy_ln_nao_naturais_da_unidade_da_federacao_pc,
  mean_salary             = dfs_shp$mean_salary_total,
  ln_higherEduc_pc        = dfs_shp$ln_inhabitants_with_higherEducation_total_pc,
  ECI                     = dfs_shp$eci_subn,
  is_coastal              = dfs_shp$is_coastal_10km,
  region_                 = dfs_shp$nm_region_en,
  centlat                 = dfs_shp$centlat,
  centlng                 = dfs_shp$centlng
)

sdp <- sp::SpatialPointsDataFrame(
  dfs_shp,
  coords        = cbind(dfs_shp$centlng, dfs_shp$centlat)
)






