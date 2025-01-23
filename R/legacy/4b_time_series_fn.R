
time_series_wrap_fn <- function(df, data_list, mapping_list) {
  
  ylab = mapping_list$ylab
  if(ylab == "Mt CO2/yr"){ylab = expression(paste("Mt C", O[2]))}
  
  if (data_list$linetype == FALSE) {
  p <- ggplot(df, aes(x = .data[[data_list$x]], y = .data[[data_list$y]], color = .data[[data_list$color]])) +
    geom_line(size = 1, aes(group = interaction(model,scenario,variable_rename))) +
    facet_wrap(vars(.data[[data_list$facet1]], .data[[data_list$facet2]]), ncol = 4, scales = mapping_list$scales) +
    labs(title = mapping_list$title,
         x = "",
         y = mapping_list$ylab,
         color = "") +
    scale_subpalette(mapping_list$palettes, mapping_list$model_color_palette) +
    scale_y_continuous(labels = scales::comma)  +
    theme_custom() +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    theme(panel.spacing.x = unit(4, "mm"))

  return(p)
  }
  
  if (!data_list$linetype == FALSE) {
    p <- ggplot(df, aes(x = .data[[data_list$x]], y = .data[[data_list$y]], color = .data[[data_list$color]])) +
      geom_line(size = 1, aes(group = interaction(model,scenario,variable_rename), linetype = .data[[data_list$linetype]])) +
      facet_wrap(vars(.data[[data_list$facet1]], .data[[data_list$facet2]]), ncol = 4, scales = mapping_list$scales) +
      labs(title = mapping_list$title,
           x = "",
           y = mapping_list$ylab,
           color = "") +
      scale_subpalette(mapping_list$palettes, mapping_list$model_color_palette) +
      scale_y_continuous(labels = scales::comma)  +
      theme_custom() +
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      theme(panel.spacing.x = unit(4, "mm"))
    
    return(p)
  }
}

time_series_grid_fn <- function(df, data_list, mapping_list) {
  
  ylab = mapping_list$ylab
  if(ylab == "Mt CO2/yr"){ylab = expression(paste("Mt C", O[2]))}
  
  if (data_list$linetype == FALSE) {
  p <- ggplot(df, aes(x = .data[[data_list$x]], y = .data[[data_list$y]],
                      color = .data[[data_list$color]])) +
    geom_line(size = 1, aes(group = interaction(model,scenario,variable_rename))) +
    facet_grid(rows = vars(.data[[data_list$facet1]]), cols = vars(.data[[data_list$facet2]]),
               scales = mapping_list$scales) +
    labs(title = mapping_list$title,
         x = "",
         y = ylab,
         color = "") +
    scale_subpalette(mapping_list$palettes, mapping_list$model_color_palette) +
    scale_y_continuous(labels = scales::comma)  +
    theme_custom() +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    theme(panel.spacing.x = unit(4, "mm"))
  }
  
  if (!data_list$linetype == FALSE) {
    p <- ggplot(df, aes(x = .data[[data_list$x]], y = .data[[data_list$y]],
                        color = .data[[data_list$color]])) +
      geom_line(size = 1, aes(group = interaction(model,scenario,variable_rename), linetype = .data[[data_list$linetype]])) +
      facet_grid(rows = vars(.data[[data_list$facet1]]), cols = vars(.data[[data_list$facet2]]),
                 scales = mapping_list$scales) +
      labs(title = mapping_list$title,
           x = "",
           y = ylab,
           color = "") +
      scale_subpalette(mapping_list$palettes, mapping_list$model_color_palette) +
      scale_y_continuous(labels = scales::comma)  +
      theme_custom() +
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      theme(panel.spacing.x = unit(4, "mm"))
  }

  return(p)
}


time_series_single_fn <- function(df, data_list, mapping_list) {
  if (!("alpha" %in% colnames(df))) {
    df$alpha = 1
  }
  p <- ggplot(df, aes(x = .data[[data_list$x]], y = .data[[data_list$y]],
                      color = .data[[data_list$color]])) +
    geom_line(size = 1, aes(alpha = alpha, group = interaction(model,scenario,variable_rename))) +
    labs(title = mapping_list$title,
         x = "",
         y = mapping_list$ylab,
         color = "") +
    scale_subpalette(mapping_list$palettes, mapping_list$model_color_palette) +
    scale_y_continuous(labels = scales::comma)  +
    theme_custom() +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    theme(panel.spacing.x = unit(4, "mm"))

  return(p)
}

