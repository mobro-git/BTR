
## Part 1. Function for targeted Net-Zero Stacked Bar w/ assigned colors

ref_df <-

stacked_bar_single_fn <- function(df, data_list, mapping_list) {
  p <- ggplot(df, aes(x = .data[[data_list$x]], y = .data[[data_list$y]])) +
    geom_col(aes(fill = .data[[data_list$color]])) +
    labs(title = mapping_list$title,
         #x = mapping_list$xlab,
         x = "",
         y = mapping_list$ylab,
         fill = "") +
    theme_emf() +
    theme(axis.text.x = element_text(angle = 90, hjust=1)) +
    scale_subpalette(mapping_list$palettes, mapping_list$model_color_palette)

  return(p)
}

# stacked_bar_wrap_fn <- function(df, data_list, mapping_list) {
#   p <- ggplot(df, aes(x = .data[[data_list$x]], y = .data[[data_list$y]])) +
#     geom_col(aes(fill = .data[[data_list$color]]), width = 0.7) +
#     facet_wrap(vars(.data[[data_list$facet]]), ncol = 4, drop = FALSE, scales = mapping_list$scales) +
#     labs(title = mapping_list$title,
#          #x = mapping_list$xlab,
#          x = "",
#          y = mapping_list$ylab,
#          fill = "") +
#     theme_emf() +
#     theme(axis.text.x = element_text(angle = 90, hjust=1)) +
#     scale_subpalette(mapping_list$palettes, mapping_list$model_color_palette)
#
#   return(p)
# }
#
# stacked_bar_grid_fn <- function(df, data_list, mapping_list) {
#   p <- ggplot(df, aes(x = .data[[data_list$x]], y = .data[[data_list$y]])) +
#     geom_col(aes(fill = .data[[data_list$color]])) +
#     facet_grid(rows = vars(.data[[data_list$facet1]]), cols = vars(.data[[data_list$facet2]]),
#                space = "free_x", scales = mapping_list$scales) +
#     labs(title = mapping_list$title,
#          #x = mapping_list$xlab,
#          x = "",
#          y = mapping_list$ylab,
#          fill = "") +
#     theme_emf() +
#     theme(axis.text.x = element_text(angle = 90, hjust=1)) +
#     scale_subpalette(mapping_list$palettes, mapping_list$model_color_palette)
#
#   return(p)
# }
#

