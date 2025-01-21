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
