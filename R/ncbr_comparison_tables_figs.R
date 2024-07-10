
ncbr_comparison_figure <- function(ncbr_comp_data_hist, btr_highlow_df) {
  
  ncbr_hist_long <- ncbr_comp_data_hist %>% 
    pivot_longer(cols = 2:20, names_to = "year", values_to = "value")
  
  btr_long <- btr_highlow_df %>% 
    pivot_longer(cols = 2:10, names_to = "year", values_to = "value")
  
  proj_range <- btr_long %>%
    group_by(Projection, year  ) %>%
    summarise(ymax = max(value),
              ymin = min(value)) %>%
    mutate(year = as.numeric(year))
  
  var_palette = c(unique(ncbr_hist_long$Projection),unique(btr_long$Projection))
  
  figure <- ggplot() +
    geom_line(ncbr_hist_long, mapping = aes(year,value, group = Projection, color = Projection),size = 0.7) +
    scale_subpalette_single(var_palette) +
    theme_btr()
  
  figure
    
}