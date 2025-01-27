viz_map2 <- function(data, varviz="ln_emig_pc", titleviz="", legendviz="", paletteer=1){
  pallets=c("viridis::plasma", "scico::tokyo")
  ggplot <- ggplot2::ggplot(data, ggplot2::aes(fill=.data[[varviz]]))+
    ggplot2::geom_sf(color="black", size=.06)+
    ggplot2::scale_fill_gradient(low="white", high="darkblue")+
    ggplot2::labs(title = titleviz, caption = "", y = "Latitude", x = "Longitude")+
    ggplot2::guides(fill=ggplot2::guide_legend(title=legendviz, reverse = T))+
    paletteer::scale_fill_paletteer_c(pallets[paletteer])+
    ggplot2::theme_void()+
    theme(legend.position="bottom")+
    ggplot2::theme(plot.title = element_text(size=26))
  return(ggplot)
}

