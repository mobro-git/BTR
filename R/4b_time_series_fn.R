
time_series_wrap_fn <- function(df, data_list, mapping_list) {
  p <- ggplot(df, aes(x = .data[[data_list$x]], y = .data[[data_list$y]], color = .data[[data_list$color]])) +
    geom_line(size = 1, aes(group = interaction(model,scenario))) +
    facet_wrap(vars(.data[[data_list$facet]]), ncol = 4, scales = mapping_list$scales) +
    labs(title = mapping_list$title,
         x = "",
         y = mapping_list$ylab,
         color = "") +
    scale_subpalette(mapping_list$palettes, mapping_list$model_color_palette) +
    scale_y_continuous(labels = scales::comma)  +
    theme_emf() +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    theme(panel.spacing.x = unit(4, "mm"))

  return(p)
}

time_series_grid_fn <- function(df, data_list, mapping_list) {
  p <- ggplot(df, aes(x = .data[[data_list$x]], y = .data[[data_list$y]],
                      color = .data[[data_list$color]])) +
    geom_line(size = 1, aes(group = interaction(model,scenario))) +
    facet_grid(rows = vars(.data[[data_list$facet1]]), cols = vars(.data[[data_list$facet2]]),
               scales = mapping_list$scales) +
    labs(title = mapping_list$title,
         x = "",
         y = mapping_list$ylab,
         color = "") +
    scale_subpalette(mapping_list$palettes, mapping_list$model_color_palette) +
    scale_y_continuous(labels = scales::comma)  +
    theme_emf() +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    theme(panel.spacing.x = unit(4, "mm"))

  return(p)
}


time_series_single_fn <- function(df, data_list, mapping_list) {
  if (!("alpha" %in% colnames(df))) {
    df$alpha = 1
  }
  p <- ggplot(df, aes(x = .data[[data_list$x]], y = .data[[data_list$y]],
                      color = .data[[data_list$color]])) +
    geom_line(size = 1, aes(alpha = alpha, group = interaction(model,scenario))) +
    labs(title = mapping_list$title,
         x = "",
         y = mapping_list$ylab,
         color = "") +
    scale_subpalette(mapping_list$palettes, mapping_list$model_color_palette) +
    scale_y_continuous(labels = scales::comma)  +
    theme_emf() +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    theme(panel.spacing.x = unit(4, "mm"))

  return(p)
}

