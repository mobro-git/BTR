
ncbr_comparison_figure <- function(ncbr_comp_data_hist, tge_long) {
  
  ncbr_hist_long <- ncbr_comp_data_hist %>% 
    pivot_longer(cols = 2:20, names_to = "year", values_to = "value") %>%
    mutate(year = as.numeric(year))
  
  # btr_long <- btr_highlow_df %>% 
  #   pivot_longer(cols = 2:10, names_to = "year", values_to = "value")
  
  proj_range_ncbr_comp <- tge_long  %>%
    mutate(year = as.numeric(year)) %>% 
    group_by(year) %>%
    summarise(ymax = max(value),
              ymin = min(value),
              med = median(value)) %>%
    mutate(Projection = '2024 BTR')
  
  var_palette = c(unique(ncbr_hist_long$Projection),unique(btr_long$Projection))
  
  figure <- ggplot() +
    geom_line(ncbr_hist_long, mapping = aes(year,value, group = Projection, color = Projection),size = 0.7) +
    geom_ribbon(proj_range_ncbr_comp, mapping = aes(x = year,ymax = ymax, ymin = ymin,
                                                  fill = Projection, 
                                                  color = Projection),
                alpha = 0.4 ,
                size = 0.7) +
    scale_subpalette_single(var_palette) +
    theme_btr()
  
  figure
    
}