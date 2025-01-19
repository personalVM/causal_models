

fct_gwr = function(gwr_form, data, weights){
  
  GWRbandwidth <- spgwr::gwr.sel(
    formula = gwr_form,
    data = data,
    method = "cv",
    gweight = gwr.bisquare
  )

  gwr_model <- spgwr::gwr(
    formula = gwr_form,
    data = data, 
    adapt = GWRbandwidth/100,
    hatmatrix=TRUE, 
    se.fit=TRUE
  )

  results <- as.data.frame(gwr_model$SDF) 
  results[["cd_micro"]] <- sdp$cd_micro

  shp <- sf::st_read(paste0("volume/data/clean_data/micro/shp/")) %>%
    janitor::clean_names() %>%
    mutate(across(where(is.numeric), as.character)) %>%
    sf::st_set_crs(4326) %>% 
    filter(cd_micro != 26019) %>% 
    sf::st_sf()
  
  df_gwr <- dplyr::left_join(results, shp, by = "cd_micro") %>%
    sf::st_sf(.) %>% sf::st_set_crs(4326)

  gwr_viz <- function(var_viz = "eci") {
    gg <- ggplot(df_gwr, aes(fill = !!sym(var_viz))) +
      geom_sf(color = "black", size = 0.06) +
      scale_fill_gradient(low = "white", high = "darkblue") +
      theme_minimal() +
      labs(
        title = paste("Visualization of", var_viz),
        fill = var_viz
      )
    return(gg)
  }
  
  # "ln_emig_pc", 
    table_gwr <- function(
      vars_int = c("ln_emig_pc", "W_ln_emig_pc", "mean_salary", "ln_higherEduc_pc", "eci", "is_coastal", "region_Northeast", "region_North", "region_Southeast", "region_South", "(Intercept)"),
      localTable_path="regression_local.html"
    ){
      # names(gwr_model$SDF@data)
      res_int <- gwr_model$SDF@data[, vars_int] %>%
        rename("Intercept"="(Intercept)") %>%
        select("Intercept", everything())
      # apply(res_int, 2, summary)
      tab <- rbind(apply(res_int, 2, summary), coef(lm(formula = gwr_form, data = df))) %>% as.data.frame()
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



  return(
    list(
      model   = gwr_model,
      # summary = gwr_summary,
      viz = gwr_viz
    )
  )
}


# # -------
# fct_gwr <- function(
#     df           = df_total,
#     # formula_vars = "ICE ~ ln_HABES_pc + ln_PIB_pc + ln_EMIG_pc + ln_IMIG_pc + dist_costa + rg_"
#     formula_vars = "ln_emig_total_pc ~ mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + ln_inhabitants_pkm + sg_region"
# ){
#   
#   shp <- sf::st_read(paste0("volume/data/clean_data/micro/shp/")) %>%
#     janitor::clean_names() %>%
#     mutate(across(where(is.numeric), as.character)) %>%
#     select(cd_micro) %>% 
#     sf::st_set_crs(4326)
#   
#   # lm <- lm(df, formula_vars)
#   # df$lm_residual <- lm$residuals
#   
#   # dfs with spatial vectors attached
#   dfs_shp <- left_join(df, shp) %>% sf::st_sf()
#   
#   dfs_shp <- dfs_shp %>% filter(cd_micro != 26019) # "Fernando de Noronha"
#   
#   # # Matriz de vizinhança
#   print("Calculando matriz de vizinhança")
#   nb <- spdep::poly2nb(dfs_shp, queen=T)
#   lw <- spdep::nb2listw(nb, zero.policy = TRUE, style="W")
#   
#   # Centroids como colunas
#   # source("src/fct_polygonCentroid.R")
#   fct_centAsCols <- function(polygonx, names = c("centlat", "centlng")){
#     centroids <- do.call(rbind, sf::st_centroid(polygonx$geometry)) %>% 
#       tibble::as_tibble() %>% stats::setNames(c(names[1],names[2])) %>% dplyr::bind_cols(polygonx, ., .name_repair = "unique")
#     return(centroids)
#   }
#   dfs_shp <- fct_centAsCols(dfs_shp)
#   dfs_shp_backup <- dfs_shp
#   
#   # Spatial Point DataFrame
#   # formula_vars = " ~  +  +  + "
#   sdp <- sp::SpatialPointsDataFrame(
#     data=data.frame(
#       ln_emig_total_pc         = dfs_shp$ln_emig_total_pc,
#       mean_salary_total        = dfs_shp$mean_salary_total,
#       ln_inhabitants_with_higherEducation_total_pc = dfs_shp$ln_inhabitants_with_higherEducation_total_pc,
#       ln_inhabitants_pkm = dfs_shp$ln_inhabitants_pkm,
#       sg_region         = dfs_shp$sg_region
#     ),
#     coords        = cbind(dfs_shp$centlng, dfs_shp$centlat)
#   )
#   
#   # formula_vars = "ln_emig_total_pc ~ mean_salary_total + ln_inhabitants_with_higherEducation_total_pc + ln_inhabitants_pkm + sg_region"
#   # banda adaptativa (kernel)
#   GWRbandwidth <- spgwr::gwr.sel(
#     formula = formula_vars,
#     data=sdp, 
#     method = "cv",
#     gweight=gwr.bisquare()
#   )
#   # %>% 
#   #   suppressWarnings() %>% 
#   #   suppressMessages()
#   GWRbandwidth
#   
#   # GWR model
#   gwr_model <- spgwr::gwr(
#     formula = formula_vars,
#     data = sdp, 
#     adapt = GWRbandwidth/100,
#     hatmatrix=TRUE, 
#     se.fit=TRUE
#   )
#   
#   # table of local results
#   fct_localTable <- function(
#     # vars_int = c("mean_salary_total", "ln_inhabitants_with_higherEducation_total_pc", "ln_inhabitants_pkm", "rg_Nordeste", "rg_Norte", "rg_Sudeste", "rg_Sul", "(Intercept)"),
#     vars_int = c("ln_HABES_pc", "ln_HABEFC_pc", "ln_HABEMC_pc", "ln_PIB_pc", "ln_EMIG_pc", "ln_IMIG_pc", "dist_costa", "rg_Nordeste", "rg_Norte", "rg_Sudeste", "rg_Sul", "(Intercept)"),
#     localTable_path="regression_local.html"
#   ){
#     res_int <- gwr_model$SDF@data[, vars_int] %>% 
#       rename("Intercepto"="(Intercept)") %>% 
#       select("Intercepto", everything())
#     # apply(res_int, 2, summary)
#     tab <- rbind(apply(res_int, 2, summary), coef(lm(formula = formula_vars, data = df))) %>% as.data.frame()
#     rownames(tab)[7] <- "Global"
#     tab_local <- t(tab) %>% 
#       as.data.frame(.)
#     tab_local_sap <- sapply(tab_local, function(x){round(x, 3)})
#     rownames(tab_local_sap) <- rownames(tab_local)
#     colnames(tab_local_sap) <- c("Mínimo", "1.Quartil", "Mediana", "Média", "3.Quartil", "Máximo", "Global")
#     tab_local_sap <- tab_local_sap %>% as.data.frame() %>% select(Global, dplyr::everything())
#     ktab_local <- knitr::kable(tab_local_sap, booktabs = T, format = 'html')
#     readr::write_file(ktab_local, localTable_path)
#   }
#   # fct_localTable(localTable_path="volume/causal_models/table_local.html")
# 
#   # Change the names of scoef 
#   lookup <- c("ln_HABES_pc_scoef" = "ln_HABES_pc", 
#               "ln_HABEM_pc_scoef" = "ln_HABEM_pc", 
#               "ln_HABEF_pc_scoef" = "ln_HABEF_pc", 
#               "ln_PIB_pc_scoef"   = "ln_PIB_pc",
#               "ln_EMIG_pc_scoef"  = "ln_EMIG_pc",
#               "ln_IMIG_pc_scoef"  = "ln_IMIG_pc",
#               "dist_costa"        = "dist_costa")
#   
#   results <- as.data.frame(gwr_model$SDF) %>%
#     dplyr::rename(dplyr::any_of(lookup))
#   results[["cd_micro"]] <- dfs_shp[[1]]
#   
#   # Spatial df with the results from GWR attached to the polygons
#   # df_gwr <- dplyr::left_join(dfs_shp, results, by = "cd_micro") %>%
#   df_gwr <- dplyr::left_join(results, shp, by = "cd_micro") %>%
#     sf::st_sf(.)
#   
#   # df_gwr$
#   dfn = df_gwr[, c("mean_salary_total", "geometry")] %>% sf::st_sf() %>% sf::st_set_crs(4326)
#   gg = ggplot2::ggplot(dfn, ggplot2::aes(fill="mean_salary_total"))+
#     ggplot2::geom_sf(color="black", size=.06)+
#     ggplot2::scale_fill_gradient(low="white", high="darkblue")
#   gg
#   ggsave("volume/causal_models/my_plot.png", plot = gg)
#   
#   # Global Moran's I function
#   fct_globalMI <- function(m1var="ECI"){
#     spdep::moran.test(df_gwr[[m1var]], listw = lw)
#     m1=spdep::moran.mc(df_gwr[[m1var]], lw, nsim=599)$statistic %>% as.numeric(.)
#     return(m1)
#   }
#   # fct_globalMI(m1var = "ECI")
#   
#   # Global Geary's C function
#   fct_globalGC <- function(gcvar="eci_subn"){
#     gc = spdep::geary(
#       x = df_gwr[[gcvar]], 
#       listw = lw, 
#       n = length(df_gwr[[gcvar]]), 
#       n1 = length(df_gwr[[gcvar]])-1, 
#       S0 = sum(unlist(lw$weights)), zero.policy=NULL
#     )$C
#     return(gc)
#   }
#   # fct_globalGC(gcvar = "ECI")
#   
#   # Local Moran
#   fct_localMI <- function(varviz="ln_HABES_pc", titleviz="", legendviz="", sig_alpha=0.1){
#     
#     local <- localmoran(x = dfs_shp[[varviz]], listw = lw)
#     dfs_shp$local_mi = local %>% as.data.frame(.) %>% select(local_mi = "Ii") %>% pull()
#     dfs_shp$local_mi_pvalue = local %>% as.data.frame(.) %>% select(local_mi_pvalue="Pr(z != E(Ii))") %>% pull()
#     dfs_shp$local_moran_quadr_mean <- attr(local, "quadr")$mean
#     # dfs_shp$local_moran_quadr_median <- attr(local, "quadr")$median
#     # dfs_shp$local_moran_quadr_pysal <- attr(local, "quadr")$pysal
#     # dfs_shp$local_mi_pvalue
#     
#     dfs_shp <- dfs_shp %>% 
#       mutate(
#         local_moran_quadr_mean = as.character(local_moran_quadr_mean),
#         local_morans_i = case_when(
#           local_mi_pvalue > sig_alpha ~ "Not Significant",
#           local_mi_pvalue <= sig_alpha ~ local_moran_quadr_mean
#         )) %>% 
#       mutate(
#         morans_i_local=ifelse(local_morans_i=="Not Significant", "Não significante", 
#                               ifelse(local_morans_i=="Low-Low", "Baixo-Baixo", 
#                                      ifelse(local_morans_i=="High-High", "Alto-Alto", 
#                                             ifelse(local_morans_i=="High-Low", "Alto-Baixo", 
#                                                    ifelse(local_morans_i=="Low-High", "Baixo-Alto", local_morans_i)))))
#       )
#     
#     ggplot <- ggplot2::ggplot(dfs_shp, ggplot2::aes(fill = morans_i_local))+
#       ggplot2::geom_sf(color="black", size=.06)+
#       ggplot2::labs(title = titleviz, caption = "", y = "Latitude", x = "Longitude")+
#       ggplot2::guides(fill=ggplot2::guide_legend(title=legendviz, reverse = T))+
#       scale_fill_manual(values = c("#D9564C", "#F59292", "#838ED9", "#4C53AA", "#DDDDDD"))+
#       ggplot2::theme_void()+
#       theme(legend.position="bottom")+
#       ggplot2::theme(plot.title = element_text(size=26))
#     
#     return(ggplot)
#   }
#   
#   # Visualizations function
#   fct_GWRviz <- function(varviz="habes_pc", titleviz="", legendviz="", paletteer=1){
#     pallets=c("viridis::plasma", "scico::tokyo")
#     ggplot <- ggplot2::ggplot(df_gwr, ggplot2::aes(fill=.data[[varviz]]))+
#       ggplot2::geom_sf(color="black", size=.06)+
#       ggplot2::scale_fill_gradient(low="white", high="darkblue")+
#       ggplot2::annotate("text", x = -32, y = -23, label = paste0("MI=", round(fct_globalMI(m1var = varviz), digits = 2)), size = 4)+
#       # ggplot2::annotate("text", x = -32, y = -27, label = paste0("GC=", round(fct_globalGC(gcvar = varviz), digits = 2)), size = 4)+
#       ggplot2::labs(title = titleviz, caption = "", y = "Latitude", x = "Longitude")+
#       ggplot2::guides(fill=ggplot2::guide_legend(title=legendviz, reverse = T))+
#       paletteer::scale_fill_paletteer_c(pallets[paletteer])+
#       ggplot2::theme_void()+
#       theme(legend.position="bottom")+
#       ggplot2::theme(plot.title = element_text(size=26))
#     return(ggplot)
#   }
#   # fct_GWRviz(varviz = "ln_HABEM_pc_scoef")
#   # fct_GWRviz(varviz = "mean_salary_total")
#   
#   gwr_aicb <- gwr_model$results$AICb # GWR book's (page 61,96)
#   gwr_aich <- gwr_model$results$AICh # Brunsdon AIC version
#   gwr_aicc <- gwr_model$results$AICc # Hurvich, Simonoff and Tsai (1998, page 276)
#   gwr_mape <- MLmetrics::MAPE(df_gwr$pred, df_gwr$ICE)
#   
#   fct_scoef_sig <- function(varviz="ln_HABES_pc", titleviz="", legendviz=""){
#     
#     coef_se = gwr_model$SDF[[paste0(varviz, "_se")]]
#     coef = gwr_model$SDF[[varviz]]
#     coef_sig = vector()
#     for(i in 1:length(coef)) {
#       if(coef[i]-3.3*coef_se[i]>0){coef_sig[i]="p-valor<0.001"}else
#         if(coef[i]-2.81*coef_se[i]>0){coef_sig[i]="p-valor<0.005"}else
#           if(coef[i]-2.58*coef_se[i]>0){coef_sig[i]="p-valor<0.01"}else
#             if(coef[i]-1.96*coef_se[i]>0){coef_sig[i]="p-valor<0.05"}else
#               if(coef[i]-1.0365*coef_se[i]>0){coef_sig[i]="p-valor<0.3"}else
#                 if(coef[i]-1.64*coef_se[i]<=0){coef_sig[i]="Não significante"}
#     }
#     dfs_shp$coef_sig <- coef_sig
#     
#     group.colors <- c("p-valor<0.001" = "#009c1a", "p-valor<0.005" = "#22b600", "p-valor<0.01" ="#26cc00", "p-valor<0.05" = "#7be382", "p-valor<0.3" = "#d2f2d4", "Não significante"="#EEEEEE")
#     
#     ggplot <- ggplot2::ggplot(dfs_shp, ggplot2::aes(fill = coef_sig))+
#       ggplot2::geom_sf(color="black", size=.06)+
#       ggplot2::labs(title = titleviz, caption = "", y = "Latitude", x = "Longitude")+
#       ggplot2::guides(fill=ggplot2::guide_legend(title=legendviz, reverse = T))+
#       scale_fill_manual(values=group.colors)+
#       ggplot2::theme_void()+
#       theme(legend.position="bottom")+
#       ggplot2::theme(plot.title = element_text(size=26))
#     
#     return(ggplot)
#   }
#   # fct_scoef_sig(varviz = "ln_HABES_pc")
#   
#   
#   return(
#     list(
#       df_gwr         = df_gwr,
#       fct_globalMI   = fct_globalMI,
#       fct_globalGC   = fct_globalGC,
#       fct_localMI    = fct_localMI,
#       fct_GWRviz     = fct_GWRviz,
#       fct_scoef_sig  = fct_scoef_sig,
#       fct_localTable = fct_localTable,
#       gwr_mape       = gwr_mape,
#       gwr_aicb       = gwr_aicb,
#       gwr_aich       = gwr_aich,
#       gwr_aicc       = gwr_aicc
#     )
#   )
#   
# }



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

