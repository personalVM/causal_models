

fct_gwr = function(
    gwr_form,
    data, 
    weights
    ){
  
  gwr_form <- as.formula(gwr_form)
  
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
  
  vars_int = c("mean_salary", "ln_higherEduc_pc", "ECI", "is_coastal", "region_Northeast", "region_North", "region_Southeast", "region_South", "X.Intercept.")
  for (var in vars_int) {
    coef_se <- results[[paste0(var, "_se")]]
    coef <- results[[var]]
    coef_sig <- vector(length = length(coef))
    for (i in seq_along(coef)) {
      if (coef[i] - 3.3 * coef_se[i] > 0) {
        coef_sig[i] <- "p-value<0.001"
      } else if (coef[i] - 2.81 * coef_se[i] > 0) {
        coef_sig[i] <- "p-value<0.005"
      } else if (coef[i] - 2.58 * coef_se[i] > 0) {
        coef_sig[i] <- "p-value<0.01"
      } else if (coef[i] - 1.96 * coef_se[i] > 0) {
        coef_sig[i] <- "p-value<0.05"
      } else if (coef[i] - 1.0365 * coef_se[i] > 0) {
        coef_sig[i] <- "p-value<0.3"
      } else {
        coef_sig[i] <- "Not significant"
      }
    }
    results[[paste0(var, "_sig")]] <- coef_sig
  }
  
  prefix <- "beta_"
  varss <- c("mean_salary", "ln_higherEduc_pc", "ECI", "is_coastal", "region_Northeast", "region_North", "region_Southeast", "region_South")
  colnames(results)[colnames(results) %in% varss] <- paste0(prefix, varss)
  results$intercept = results$X.Intercept.
  results$intercept_se = results$X.Intercept._se
  
  
  shp <- sf::st_read(paste0("volume/data/clean_data/micro/shp/")) %>%
    janitor::clean_names() %>%
    mutate(across(where(is.numeric), as.character)) %>%
    sf::st_set_crs(4326) %>% 
    filter(cd_micro != 26019) %>% 
    sf::st_sf()
  
  df_gwr <- dplyr::left_join(results, shp, by = "cd_micro") %>%
    sf::st_sf(.) %>% 
    sf::st_set_crs(4326)
  
  df_gwr$residuals = gwr_model$lm$residuals
  
  viz_sig <- function(var="ECI", titleviz="", legendviz=""){
    group.colors <- c("p-value<0.001" = "#009c1a", "p-value<0.005" = "#22b600", "p-value<0.01" ="#26cc00", "p-value<0.05" = "#7be382", "p-value<0.3" = "#d2f2d4", "Not significant"="#EEEEEE")
    ggplot <- ggplot2::ggplot(df_gwr, ggplot2::aes(fill = !!sym(var)))+
      ggplot2::geom_sf(color="black", size=.06)+
      ggplot2::labs(title = titleviz, caption = "", y = "Latitude", x = "Longitude")+
      ggplot2::guides(fill=ggplot2::guide_legend(title=legendviz, reverse = T))+
      scale_fill_manual(values=group.colors)+
      ggplot2::theme_void()+
      theme(legend.position="bottom")+
      ggplot2::theme(plot.title = element_text(size=26))
    return(ggplot)
  }

  viz1 <- function(var_viz = "ECI") {
    gg <- ggplot(df_gwr, aes(fill = !!sym(var_viz))) +
      geom_sf(color = "black", size = 0.06) +
      scale_fill_gradient(low = "white", high = "darkblue") +
      theme_minimal() +
      labs(title = paste("Visualization of", var_viz), fill = var_viz)
    return(gg)
  }
  
  table_gwr <- function(
    vars_int = c("mean_salary", "ln_higherEduc_pc", "ECI", "is_coastal", "region_Northeast", "region_North", "region_Southeast", "region_South", "(Intercept)"),
    localTable_path="regression_local.html"
  ){
    res_int <- gwr_model$SDF@data[, vars_int] %>%
      rename("Intercept"="(Intercept)") %>%
      select("Intercept", everything())
    # apply(res_int, 2, summary)
    tab <- rbind(apply(res_int, 2, summary), coef(lm(formula = gwr_form, data = data))) %>% as.data.frame()
    rownames(tab)[7] <- "Global"
    tab_local <- t(tab) %>%
      as.data.frame(.)
    tab_local_sap <- sapply(tab_local, function(x){round(x, 3)})
    rownames(tab_local_sap) <- rownames(tab_local)
    colnames(tab_local_sap) <- c("Minimun", "1.Quartile", "Median", "Mean", "3.Quartile", "Maximum", "Global")
    tab_local_sap <- tab_local_sap %>% as.data.frame() %>% select(Global, dplyr::everything())
    ktab_local <- knitr::kable(tab_local_sap, booktabs = T, format = 'html')
    readr::write_file(ktab_local, localTable_path)
  }
  
  fct_globalMI <- function(var="beta_ECI"){
    mi  = spdep::moran.mc(df_gwr[[var]], lw, nsim=599)$statistic %>% as.numeric(.)
    sig = spdep::moran.mc(df_gwr[[var]], lw, nsim=599)$p.value %>% as.numeric(.)
    return(list(mi=mi, sig=sig))
  }
  
  gwr_aicb <- gwr_model$results$AICb # GWR book's (page 61,96)
  gwr_aich <- gwr_model$results$AICh # Brunsdon AIC version
  gwr_aicc <- gwr_model$results$AICc # Hurvich, Simonoff and Tsai (1998, page 276)
  gwr_mape <- MLmetrics::MAPE(df_gwr$pred, df_gwr$ICE)
  

  return(
    list(
      model = gwr_model,
      df_gwr = df_gwr, 
      # summary = gwr_summary,
      viz1   = viz1,
      viz_sig = viz_sig,
      fct_globalMI = fct_globalMI,
      table = table_gwr,
      AICb  = gwr_aicb,
      AICh  = gwr_aich,
      AICc  = gwr_aicc,
      MAPE  = gwr_mape
    )
  )
}

