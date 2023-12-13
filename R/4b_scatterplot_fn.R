
scatterplot_wrap_fn <- function(df, data_list, mapping_list) {
  p <- ggplot(df, aes(x = .data[[data_list$x]], y = .data[[data_list$y]],
                      color = .data[[data_list$color]], shape = .data[[data_list$shape]])) +
    geom_point(size = 2) +
    facet_wrap(vars(.data[[data_list$facet]]), ncol = 4, scales = mapping_list$scales) +
    labs(title = mapping_list$title,
         x = "",
         y = mapping_list$ylab,
         color = "") +
    #scale_subpalette(mapping_list$palettes, mapping_list$model_color_palette) +
    scale_y_continuous(labels = scales::comma)  +
    theme_emf() +
    theme(panel.spacing.x = unit(4, "mm"))

  return(p)
}

scatterplot_grid_fn <- function(df, data_list, mapping_list) {
  p <- ggplot(df, aes(x = .data[[data_list$x]], y = .data[[data_list$y]],
                      color = .data[[data_list$color]], shape = .data[[data_list$shape]])) +
    geom_point(size = 2) +
    facet_grid(rows = vars(.data[[data_list$facet1]]), cols = vars(.data[[data_list$facet2]]),
               scales = mapping_list$scales) +
    labs(title = mapping_list$title,
         x = "",
         y = mapping_list$ylab,
         color = "") +
    #scale_subpalette(mapping_list$palettes, mapping_list$model_color_palette) +
    scale_y_continuous(labels = scales::comma)  +
    theme_emf() +
    theme(panel.spacing.x = unit(4, "mm"))

  return(p)
}


scatterplot_single_fn <- function(df, data_list, mapping_list) {
  if (mapping_list$text) {
    p <- ggplot(df, aes(x = .data[[data_list$x]], y = .data[[data_list$y]])) +
      geom_point(aes(color = .data[[data_list$color]], shape = .data[[data_list$shape]]),
                 size = 3) +
      geom_text_repel(aes(label = .data[[data_list$label]]),
                      size = 4,
                      force = 3,
                      force_pull = 2,
                      max.overlaps = 20,
                      parse = TRUE,
                      direction = mapping_list$text_direction,
                      fontface = "bold") +
      scale_shape_discrete(name = "scenario") +
      scale_color_discrete(name = "scenario") +
      labs(x = mapping_list$xlab,
           y = mapping_list$ylab,
           color = "",
           shape = mapping_list$shape,
           title = mapping_list$title) +
      # scale_subpalette(mapping_list$palettes, mapping_list$model_color_palette) +
      theme_emf() +
      theme(panel.spacing.x = unit(4, "mm"))
  }
  else {
    p <- ggplot(df, aes(x = .data[[data_list$x]], y = .data[[data_list$y]],
                        color = .data[[data_list$color]], shape = .data[[data_list$shape]])) +
      geom_point(size = 4) +
      scale_shape_discrete(name = "scenario") +
      scale_color_discrete(name = "scenario") +
      labs(x = mapping_list$xlab,
           y = mapping_list$ylab,
           color = "",
           shape = mapping_list$shape,
           title = mapping_list$title) +
      # scale_subpalette(mapping_list$palettes, mapping_list$model_color_palette) +
      scale_y_continuous(labels = scales::comma)  +
      theme_emf() +
      theme(panel.spacing.x = unit(4, "mm"))
  }

  return(p)
}

