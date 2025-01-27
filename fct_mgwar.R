# R-script gwar.R

# References --------------------------------------------------------------

# A function to model GWAR

# Setup -------------------------------------------------------------------

# rm(list = ls())
gc()
source("volume/etl/util_loadPackages.R")
library(spgwr)
library(sp)
library(spdep)

# devtools::install_github("steibelj/gwaR", build_vignettes = TRUE)
# gwaR::
# spgwr::


formula_vars = "ln_emig_pc ~ W_ln_emig_pc+ mean_salary + ln_higherEduc_pc + eci + is_coastal + region_"
gwar_form = "ln_emig_pc ~ W_ln_emig_pc+ mean_salary + ln_higherEduc_pc + eci + is_coastal + region_"

GWARbandwidth <- spgwr::gwr.sel(
  formula = formula_vars,
  data = sdp,
  method = "cv",
  gweight = gwr.bisquare
)

# GWR model
(gwar_model <- spgwr::gwr(
  formula = formula_vars,
  data = sdp, 
  adapt = GWARbandwidth/100,
  hatmatrix=TRUE, 
  se.fit=TRUE
))

results <- as.data.frame(gwar_model$SDF) 
# %>% dplyr::rename(dplyr::any_of(lookup))
results[["cd_micro"]] <- dfs_shp[[1]]

df_gwar <- dplyr::left_join(results, shp, by = "cd_micro") %>%
  sf::st_sf(.)

# dfn = df_gwr[, c("mean_salary_total", "geometry")] %>% sf::st_sf() %>% sf::st_set_crs(4326)
# dfn = df_gwr[, c("mean_salary_total_se", "geometry")] %>% sf::st_sf() %>% sf::st_set_crs(4326)
dfn = df_gwr %>% sf::st_sf() %>% sf::st_set_crs(4326)


# ln_emig_total_pc ~ mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + eci_subn + is_coastal_100km + sg_region
# gg <- ggplot2::ggplot(dfn, ggplot2::aes(fill = eci)) +
gg <- ggplot2::ggplot(dfn, ggplot2::aes(fill = mean_salary)) +
# gg <- ggplot2::ggplot(dfn, ggplot2::aes(fill = ln_popWithHigherEduc_pc)) +
  ggplot2::geom_sf(color = "black", size = 0.06) +
  ggplot2::scale_fill_gradient(low = "white", high = "darkblue") +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "",
    fill = "..."
  )
gg
print("gg")


# "ln_emig_pc", 
table_gwr <- function(
    vars_int = c("W_ln_emig_pc", "mean_salary", "ln_higherEduc_pc", "eci", "is_coastal", "region_Northeast", "region_North", "region_Southeast", "region_South", "(Intercept)"),
    localTable_path="regression_local.html"
){
  # names(gwr_model$SDF@data)
  res_int <- gwar_model$SDF@data[, vars_int] %>%
    rename("Intercept"="(Intercept)") %>%
    select("Intercept", everything())
  # apply(res_int, 2, summary)
  tab <- rbind(apply(res_int, 2, summary), coef(lm(formula = gwar_form, data = dfs_shp))) %>% as.data.frame()
  rownames(tab)[7] <- "Global"
  tab_local <- t(tab) %>%
    as.data.frame(.)
  tab_local_sap <- sapply(tab_local, function(x){round(x, 3)})
  rownames(tab_local_sap) <- rownames(tab_local)
  colnames(tab_local_sap) <- c("Mínimo", "1.Quartil", "Mediana", "Média", "3.Quartil", "Máximo", "Global")
  tab_local_sap <- tab_local_sap %>% as.data.frame() %>% select(Global, dplyr::everything())
  ktab_local <- knitr::kable(tab_local_sap, booktabs = T, format = 'html')
  readr::write_file(ktab_local, localTable_path)
}
# fct_localTable(localTable_path="volume/causal_models/table_local.html")




