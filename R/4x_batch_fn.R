
## Part 1. Functions for Stacked Bar Batch Plots

# regular, x, y, color, 1 facet
stacked_bar_batch_plot_fn <- function(df, x, y, color, facet, mapping_list) {
  p <- ggplot(df, aes(x = .data[[x]], y = .data[[y]])) +
    geom_col(aes(fill = .data[[color]]), width = 0.7) +
    facet_wrap(vars(.data[[facet]]), ncol = 4) +
    theme(legend.position="right",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(title = mapping_list$title,
         x = mapping_list$xlab,
         y = mapping_list$ylab) +
    theme_bw()
  return(p)
}

# facet grid: x, y, color, 2 facets
stacked_bar_batch_grid_plot_fn1 <- function(df, x, y, color, facet1, facet2, mapping_list) {
  p <- ggplot(df, aes(x = .data[[x]], y = .data[[y]])) +
    geom_col(aes(fill = .data[[color]])) +
    facet_grid(rows = vars(.data[[facet1]]), cols = vars(.data[[facet2]]),
               space = "free_x") +
    theme(legend.position="right") +
    labs(title = mapping_list$title,
         x = mapping_list$xlab,
         y = mapping_list$ylab,
         fill = "") +
    theme(strip.text.x = element_text(size = 14)) +
    theme(strip.text.y = element_text(size = 14)) +
    theme_bw()
  return(p)
}


# x interation: 2x's, y, color, 1 facet
stacked_bar_batch_grid_plot_fn2 <- function(df, x1, x2, y, color, facet, mapping_list) {
  p <- ggplot(df, aes(x = interaction(.data[[x1]], .data[[x2]]), y = .data[[y]])) +
    geom_col(aes(fill = .data[[color]])) +
    facet_wrap(vars(.data[[facet]]), ncol = 4) +
    theme(legend.position="right") +
    labs(title = mapping_list$title,
         x = mapping_list$xlab,
         y = mapping_list$ylab,
         fill = "") +
    theme_bw()
  return(p)
}

