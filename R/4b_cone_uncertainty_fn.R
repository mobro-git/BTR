# function to get the info about upper and lower bound to make the ribbon
ribbonize <- function(df, range) {
  if (length(unique(df$range)) > 1) {
    sen_ribbon = df %>%
      group_by(year, range, variable_rename, unit)

  } else if (range == "scenario") {
    sen_ribbon = df %>%
      group_by(year, variable, unit)

  } else if (range == "model") {
    sen_ribbon = df %>%
      group_by(year, scenario, variable, unit)
  }

  sen_ribbon = sen_ribbon %>%
    summarize(catmin = min(value, na.rm = T),
              catmax = max(value, na.rm = T),
              variable_rename = variable_rename,
              model = model,
              scenario = scenario,
              region = region,
              color = color,
              .groups = "drop") %>%
    ungroup()
  return(sen_ribbon)
}

band_grid_fn <- function(df, data_list, mapping_list) {

  # get the ribbon info for band
  sen_ribbon = ribbonize(df, data_list$range)

  p <- ggplot() +
    geom_line(df, mapping = aes(x = .data[[data_list$x]], y = .data[[data_list$y]],
                                color = .data[[data_list$color]],
                                group = interaction(model, scenario, .data[[data_list$color]])),
              size = 1, alpha = 0.1) +
    facet_grid(rows = vars(.data[[data_list$facet2]]), cols = vars(.data[[data_list$facet1]]),
               scales = mapping_list$scales) +
    geom_ribbon(data = sen_ribbon,
                mapping = aes(x= .data[[data_list$x]],
                              ymin= catmin, ymax= catmax,
                              fill= .data[[data_list$color]], color= .data[[data_list$color]]),
                alpha = 0.1) +
    labs(title = mapping_list$title,
         x = mapping_list$xlab,
         y = mapping_list$ylab,
         fill = "",
         color = "") +
    scale_subpalette(mapping_list$palettes, mapping_list$model_color_palette) +
    theme_emf() +
    scale_y_continuous(labels = scales::comma)  +
    theme(panel.spacing.x = unit(4, "mm"))

  return(p)
}

band_wrap_fn <- function(df, data_list, mapping_list) {

  # get the ribbon info for band
  sen_ribbon = ribbonize(df, data_list$range)

  p <- ggplot() +
    geom_line(df, mapping = aes(x = .data[[data_list$x]], y = .data[[data_list$y]],
                                color = .data[[data_list$color]],
                                group = interaction(model, scenario, .data[[data_list$color]])),
              size = 1, alpha = 0.1) +
    facet_wrap(vars(.data[[data_list$facet]]), ncol = 4, scales = mapping_list$scales) +
    geom_ribbon(data = sen_ribbon,
                mapping = aes(x= .data[[data_list$x]],
                              ymin= catmin, ymax= catmax,
                              fill= .data[[data_list$color]], color= .data[[data_list$color]]),
                alpha = 0.1) +
    labs(title = mapping_list$title,
         x = mapping_list$xlab,
         y = mapping_list$ylab,
         fill = "",
         color = "") +
    scale_subpalette(mapping_list$palettes, mapping_list$model_color_palette) +
    theme_emf() +
    scale_y_continuous(labels = scales::comma)  +
    theme(panel.spacing.x = unit(4, "mm"))

  return(p)
}


cone_uncertainty_single_fn <- function(df, data_list, mapping_list) {

  # get the ribbon info
  sen_ribbon = ribbonize(df, data_list$range)

  p <- ggplot() +
    geom_line(df, mapping = aes(x = .data[[data_list$x]], y = .data[[data_list$y]],
                      color = .data[[data_list$color]],
                      group = interaction(.data[[data_list$range]], .data[[data_list$color]], model)),
              size = 1, alpha = 0.1) +
    geom_ribbon(data = sen_ribbon,
                mapping = aes(x= .data[[data_list$x]],
                              ymin= catmin, ymax= catmax,
                              fill= .data[[data_list$color]], color= .data[[data_list$color]]),
                alpha = 0.1) +
    labs(title = mapping_list$title,
         x = mapping_list$xlab,
         y = mapping_list$ylab,
         fill = "",
         color = "") +
    scale_subpalette(mapping_list$palettes, mapping_list$model_color_palette) +
    theme_emf() +
    theme(panel.spacing.x = unit(4, "mm")) +
    scale_y_continuous(labels = scales::comma)

  return(p)
}

cone_uncertainty_grid_fn <- function(df, data_list, mapping_list) {

  # get the ribbon info
  sen_ribbon = ribbonize(df, data_list$range)

  p <- ggplot() +
    geom_line(df, mapping = aes(x = .data[[data_list$x]], y = .data[[data_list$y]],
                                color = .data[[data_list$color]],
                                group = interaction(.data[[data_list$range]], .data[[data_list$color]], model)),
              size = 0.5, alpha = 0.1) +
    facet_grid(rows = vars(.data[[data_list$facet1]]), cols = vars(.data[[data_list$facet2]]),
               scales = mapping_list$scales) +
    geom_ribbon(data = sen_ribbon,
                mapping = aes(x= .data[[data_list$x]],
                              ymin= catmin, ymax= catmax,
                              fill= .data[[data_list$color]], color= .data[[data_list$color]]),
                alpha = 0.1) +
    labs(title = mapping_list$title,
         x = mapping_list$xlab,
         y = mapping_list$ylab,
         fill = "",
         color = "") +
    scale_subpalette(mapping_list$palettes, mapping_list$model_color_palette) +
    theme_emf() +
    scale_y_continuous(labels = scales::comma)  +
    theme(panel.spacing.x = unit(4, "mm"))

  return(p)
}


cone_uncertainty_wrap_fn <- function(df, data_list, mapping_list) {

  # get the ribbon info
  sen_ribbon = ribbonize(df, data_list$range)

  p <- ggplot() +
    geom_line(df, mapping = aes(x = .data[[data_list$x]], y = .data[[data_list$y]],
                                color = .data[[data_list$color]],
                                group = interaction(.data[[data_list$range]], .data[[data_list$color]])),
              size = 1, alpha = 0.1) +
    facet_wrap(vars(.data[[data_list$facet]]), ncol = 4, scales = mapping_list$scales) +
    geom_ribbon(data = sen_ribbon,
                mapping = aes(x = .data[[data_list$x]],
                              ymin = catmin, ymax = catmax,
                              fill = .data[[data_list$color]], color = .data[[data_list$color]]),
                alpha = 0.1) +
    labs(title = mapping_list$title,
         x = mapping_list$xlab,
         y = mapping_list$ylab,
         fill = "",
         color = "") +
    scale_subpalette(mapping_list$palettes, mapping_list$model_color_palette) +
    theme_emf() +
    scale_y_continuous(labels = scales::comma)  +
    theme(panel.spacing.x = unit(4, "mm"))

  return(p)
}

# ribbonize <- function(df, var, scenarios, models) {
#   sen_ribbon = df %>%
#     filter(scenario %in% scenarios,
#            model %in% models) %>%
#     group_by(year, scenario, variable, unit) %>%
#     summarize(catmin = min(value, na.rm = T),
#               catmax = max(value, na.rm = T),
#               .groups = "drop") %>%
#     ungroup()
#   return(sen_ribbon)
# }


# coneofuncertainty <- function(data, var, title_name, scenarios, models) {
#
#   # get the ribbon info
#   sen_ribbon = ribbonize(data, var, scenarios, models)
#
#   # make the figures
#   figure <- ggplot() +
#       geom_line(data = df_list,
#                 aes(x = year,y = value,
#                     color = model)) +
#       scale_shape_manual(values=model_shapes) +
#       scale_colour_manual(values=model_color_palette) +
#       theme(legend.position="bottom", aspect.ratio = 4) +
#       scale_y_continuous(labels = scales::comma) +
#       facet_wrap(vars(scenario), ncol = 4) +
#       geom_ribbon(data = sen_ribbon,
#                   mapping = aes(x=year,
#                                 ymin=catmin, ymax=catmax,
#                                 fill=variable, color=variable),
#                    alpha = 0.2) +
#       labs(
#         title = title_name,
#         x = "Year",
#         y = unique(df_list$unit)[1]
#       )
#   figure
# }

