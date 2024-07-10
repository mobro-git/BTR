
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
  
  palette <- c('2022 NC' = 'red',
               '2021 NC' = 'blue',
               '2016 BR' = 'darkgreen',
               '2014 NC' = 'purple',
               '2010 NC' = 'yellow',
               '2006 NC' = 'orange')
  
  figure <- ggplot() +
    geom_line(ncbr_hist_long, mapping = aes(year,value, group = Projection, color = Projection),size = 0.7) +
    scale_color_manual(values = palette) +
    theme_btr()
  
  figure
    
}