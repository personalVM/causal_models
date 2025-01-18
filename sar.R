library(HSAR)
library(spatialreg)
# ?HSAR::sar()
# install.packages("spdep")
library(spdep)


shp <- sf::st_read(paste0("volume/data/clean_data/micro/shp/")) %>%
  janitor::clean_names() %>%
  mutate(across(where(is.numeric), as.character)) %>%
  select(cd_micro) %>% 
  sf::st_set_crs(4326)
dfs_shp <- left_join(df, shp) %>% 
  filter(cd_micro != 26019) %>% 
  sf::st_sf()

nb <- poly2nb(dfs_shp, row.names = dfs_shp$cd_micro)
lw <- nb2listw(nb)
print("I am Ready!")

emig_form = as.formula(paste0("ln_emig_total_pc  ~ ", indv))
y = dfs_shp$ln_emig_total_pc
imig_form = as.formula(paste0("ln_immigrants_pc  ~ ", indv))
y = dfs_shp$ln_immigrants_pc
internal_form = as.formula(paste0("ln_nao_naturais_da_unidade_da_federacao_pc  ~ ", indv))
y = dfs_shp$ln_nao_naturais_da_unidade_da_federacao_pc


library(spatialreg)
(sar_model <- spatialreg::spautolm(emig_form, data = dfs_shp, listw = lw, family = "SAR"))
(sar_model <- spatialreg::spautolm(imig_form, data = dfs_shp, listw = lw, family = "SAR"))
(sar_model <- spatialreg::spautolm(internal_form, data = dfs_shp, listw = lw, family = "SAR"))
summary(sar_model)

fitted_values <- fitted(sar_model)
residuals <- residuals(sar_model)
TSS <- sum((y - mean(y))^2)
RSS <- sum(residuals^2)
pseudo_r2 <- 1 - (RSS / TSS)
pseudo_r2

residuals <- residuals(sar_model)
tss <- sum((dfs_shp$ln_emig_total_pc - mean(dfs_shp$ln_emig_total_pc))^2)
tss <- sum((dfs_shp$ln_immigrants_pc - mean(dfs_shp$ln_emig_total_pc))^2)
rss <- sum(residuals^2)
r_squared <- 1 - (rss / tss)
n <- length(residuals)  # Number of observations
k <- length(sar_model$coefficients) - 1  # Number of predictors (excluding the intercept)
adjusted_r_squared <- 1 - ((1 - r_squared) * (n - 1)) / (n - k - 1)

cat("Adjusted R-squared:", adjusted_r_squared, "\n")




