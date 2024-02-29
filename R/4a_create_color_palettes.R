
create_subpalettes <- function(plot_list, config) {
  figure_titles = unique(plot_list$title_name)

  for (figure_title in figure_titles) {
    temp = plot_list %>% filter(title_name == figure_title)

    if(unique(temp$color) == "model")
    {var_palette = config[[unique(temp$models)]]}
    else if(unique(temp$color) == "variable_rename")
    {var_palette = unique(temp$variable_rename)}
    else if(unique(temp$color) == "scenario")
    {var_palette = config[[unique(temp$scenarios)]]}

    names(var_palette) = var_palette
    sub_palettes[[figure_title]] = var_palette
    sub_palettes = sub_palettes %>%
      map(~find_color(.x, color_map))
  }
  sub_palettes
}



