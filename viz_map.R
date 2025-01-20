viz_map <- function(data, var_viz = "eci") {
  gg <- ggplot(data, aes(fill = !!sym(var_viz))) +
    geom_sf(color = "black", size = 0.06) +
    scale_fill_gradient(low = "white", high = "darkblue") +
    theme_minimal() +
    labs(
      title = paste("Visualization of", var_viz),
      fill = var_viz
    )
  return(gg)
}
