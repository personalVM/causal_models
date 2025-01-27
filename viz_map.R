viz_map <- function(data, var_viz = "eci", color="blue") {
  gg <- ggplot(data %>% sf::st_as_sf(), aes(fill = !!sym(var_viz))) +
    geom_sf(color = "black", size = 0.06) +
    scale_fill_gradient(low = "white", high = color) +
    # theme_minimal() +
    theme_void() +
    labs(
      title = paste("Visualization of", var_viz),
      fill = var_viz
    )
  return(gg)
}


