
ncbr_comparison_figure <- function(ncbr_comp_data_hist, tge_all_long, config) {
  
  ncbr_hist_long <- ncbr_comp_data_hist %>% 
    pivot_longer(cols = 2:20, names_to = "year", values_to = "value") %>%
    mutate(year = as.numeric(year))
  
  # btr_long <- btr_highlow_df %>% 
  #   pivot_longer(cols = 2:10, names_to = "year", values_to = "value")
  
  proj_range_ncbr_comp <- tge_all_long  %>%
    mutate(year = as.numeric(year)) %>% 
    group_by(year, Projection) %>%
    summarise(ymax = max(value),
              ymin = min(value),
              med = median(value)) %>%
    mutate(Projection = '2024 BTR')
  
  var_palette = c(unique(ncbr_hist_long$Projection),unique(proj_range_ncbr_comp$Projection))
  
  #TODO: FLIP LEGEND ORDER, new to old desc
  figure <- ggplot() +
    geom_line(ncbr_hist_long, mapping = aes(year,value, group = Projection, color = Projection),size = 0.7) +
    geom_ribbon(proj_range_ncbr_comp, mapping = aes(x = year,ymax = ymax, ymin = ymin,
                                                  fill = Projection, 
                                                  color = Projection),
                alpha = 0.4 ,
                size = 0.7) +
    geom_vline(xintercept = config$base_year,
               linetype = 'dashed',
               color = "black",
               # size = 0.4,
               alpha = 0.5) +
    labs(x = 'Year',
         y = expression(paste("Total Gross GHG Emissions (MMt ", CO[2], "e)", sep = ""))) +
    scale_subpalette_single(var_palette) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = c(2005, 2010, 2015, 2020, 2022, 2025, 2030, 2035, 2040), expand = c(0,0)) +
    theme_btr() +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.15, 0.2))
  
  figure
    
}
