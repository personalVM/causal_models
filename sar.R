


df

library(HSAR)

install.packages("spdep")
library(spdep)
dfs_shp
nb <- poly2nb(dfs_shp, row.names = dfs_shp$cd_micro)
lw <- nb2listw(nb)

# neighbors <- poly2nb(your_spatial_data, queen = FALSE) 
# weights <- nb2listw(neighbors, style = "W")


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


# Fit a SAR model
spdep::lag
sar_model <- lagsarlm(emig_form, data = dfs_shp, lw)
sar.res <- spdep::spautolm(emig_form,listw=lw,data=dfs_shp)
sar.res
spdep


sar_model <- spdep::spautolm(emig_form, data = dfs_shp, listw = lw, family = "SAR")
sar_model <- spautolm(ln_emig_total_pc ~ mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + ln_labor_pc + is_coastal_50km + sg_region, data = dfs_shp, listw = lw, family = "SAR")
sar_model <- spdep::splm(ln_emig_total_pc ~ mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + ln_labor_pc + is_coastal_50km + sg_region, data = dfs_shp, listw = lw, family = "SAR")

# Summary of the model
summary(sar_model)

# Check residuals for spatial autocorrelation
moran.test(residuals(sar_model), lw)







RUN curl -o /home/rstudio/HSAR_0_5_1.tar.gz https://cran.r-project.org/src/contrib/Archive/HSAR/HSAR_0.5.1.tar.gz
RUN tar -xzvf /home/rstudio/HSAR_0_5_1.tar.gz -C /home/rstudio/
  RUN sudo chmod -R 775 /home/rstudio/HSAR/ && sudo chown -R rstudio:rstudio /home/rstudio/HSAR/
  RUN R -e "install.packages('/home/rstudio/HSAR/', repos = NULL, type = 'source')"


