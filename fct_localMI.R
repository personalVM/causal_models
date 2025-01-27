fct_localMI <- function(data, lw=lw, varviz="ECI", titleviz="", legendviz="", sig_alpha=0.1){
  
  local <- localmoran(x = data[[varviz]], listw = lw)
  data$local_mi = local %>% as.data.frame(.) %>% select(local_mi = "Ii") %>% pull()
  data$local_mi_pvalue = local %>% as.data.frame(.) %>% select(local_mi_pvalue="Pr(z != E(Ii))") %>% pull()
  data$local_moran_quadr_mean <- attr(local, "quadr")$mean
  # dfs_shp$local_moran_quadr_median <- attr(local, "quadr")$median
  # dfs_shp$local_moran_quadr_pysal <- attr(local, "quadr")$pysal
  # dfs_shp$local_mi_pvalue
  
  dfs_shp <- data %>% 
    mutate(
      local_moran_quadr_mean = as.character(local_moran_quadr_mean),
      local_morans_i = case_when(
        local_mi_pvalue > sig_alpha ~ "Not Significant",
        local_mi_pvalue <= sig_alpha ~ local_moran_quadr_mean
      )) %>%
    mutate(
      morans_i_local=ifelse(local_morans_i=="Not Significant", "Not Significant",
                            ifelse(local_morans_i=="Low-Low", "Low-Low",
                                   ifelse(local_morans_i=="High-High", "High-High",
                                          ifelse(local_morans_i=="High-Low", "High-Low",
                                                 ifelse(local_morans_i=="Low-High", "Low-High", local_morans_i)))))
    )
  # mutate(
  #   morans_i_local=ifelse(local_morans_i=="Not Significant", "Não significante",
  #                         ifelse(local_morans_i=="Low-Low", "Baixo-Baixo",
  #                                ifelse(local_morans_i=="High-High", "Alto-Alto",
  #                                       ifelse(local_morans_i=="High-Low", "Alto-Baixo",
  #                                              ifelse(local_morans_i=="Low-High", "Baixo-Alto", local_morans_i)))))
  # )
  
  ggplot <- ggplot2::ggplot(dfs_shp, ggplot2::aes(fill = morans_i_local))+
    ggplot2::geom_sf(color="black", size=.06)+
    ggplot2::labs(title = titleviz, caption = "", y = "Latitude", x = "Longitude")+
    ggplot2::guides(fill=ggplot2::guide_legend(title=legendviz, reverse = T))+
    scale_fill_manual(values = c("#D9564C", "#F59292", "#838ED9", "#4C53AA", "#DDDDDD"))+
    ggplot2::theme_void()+
    theme(legend.position="bottom")+
    ggplot2::theme(plot.title = element_text(size=26))
  
  return(ggplot)
}

