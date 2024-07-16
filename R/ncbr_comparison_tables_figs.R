
ncbr_comparison_figure <- function(ncbr_comp_data_hist, tge_all_long, config) {
  
  ncbr_hist_long <- ncbr_comp_data_hist %>% 
    pivot_longer(cols = 2:20, names_to = "year", values_to = "value") %>%
    mutate(year = as.numeric(year))
  ncbr_hist_btr <- tge_all_long %>% filter(Projection == '2024 BTR',
                                           year <= 2022)
  
  proj_range_ncbr_comp <- tge_all_long %>%
    group_by(Projection, year) %>%
    summarise(ymax = max(value),
              ymin = min(value),
              med = median(value)) %>%
    filter(Projection == '2024 BTR',
           year >= config$base_year)
  
  var_palette = c(unique(ncbr_hist_long$Projection),unique(proj_range_ncbr_comp$Projection))
  
  #TODO: FLIP LEGEND ORDER, new to old desc
  figure <- ggplot() +
    geom_line(ncbr_hist_long, mapping = aes(year,value, group = Projection, color = Projection),size = 0.7) +
    geom_line(ncbr_hist_btr,mapping = aes(year,value, group = Projection, color = Projection),size = 0.7 ) +
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


ncbr_comp_fig_1990 <- function(tge_90_long, tge_all_long, config) {
  
  ncbr_hist_btr <- tge_all_long %>% filter(Projection == '2024 BTR',
                                           year <= 2022)
  
  proj_range_ncbr_comp <- tge_all_long %>%
    group_by(Projection, year) %>%
    summarise(ymax = max(value),
              ymin = min(value),
              med = median(value)) %>%
    filter(Projection == '2024 BTR',
           year >= config$base_year)
  
  var_palette = c(unique(tge_90_long$Projection),unique(proj_range_ncbr_comp$Projection))
  
  figure <- ggplot() +
    geom_line(tge_90_long, mapping = aes(year,value, group = Projection, color = Projection),size = 0.7) +
    geom_line(ncbr_hist_btr,mapping = aes(year,value, group = Projection, color = Projection),size = 0.7 ) +
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
    scale_x_continuous(breaks = c(seq(1990,2020,by=5), 2022, seq(2025,2040,by=5)), expand = c(0,0)) +
    theme_btr() +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.15, 0.2))
  
  figure
}

