
ncbr_comparison_figure <- function(ncbr_comp_ribbon, tge_all_long, settings, config) {
  
  ncbr_hist_btr <- tge_all_long %>% filter(Report == 'btr_2024',
                                           Year <= 2022,
                                           Year %in% config$fives)
  
  ncbr_hist_long <- tge_all_long %>% filter(!Report == 'btr_2024',
                                            Year <= 2022,
                                            Year %in% config$fives)
  
  ncbr_ribbon_clean <- ncbr_comp_ribbon %>% 
    filter(Year %in% config$fives)
  
  # tge_ends <- tge_all_long %>%
  #   filter(Year == max(Year),
  #          proj_name %in% c('usrr_wm_hiseq',
  #                           'gcam_wm_hiseq',
  #                           'nems_wm_hiseq'))
  
  var_palette = c(unique(tge_all_long$Report))
  
  #TODO: FLIP LEGEND ORDER, new to old desc
  figure <- ggplot() +
    geom_line(ncbr_hist_long, mapping = aes(Year,Value, group = Report, color = Report),size = 0.7) +
    geom_line(ncbr_hist_btr,mapping = aes(Year,Value, group = Report, color = Report),size = 0.7 ) +
    #geom_text_repel(tge_ends, mapping = aes(Year,Value, label = proj_name)) +
    geom_ribbon(ncbr_ribbon_clean, mapping = aes(x = Year,ymax = max, ymin = min,
                                                  fill = Report, 
                                                  color = Report),
                alpha = 0.4 ,
                size = 0.7) +
    geom_vline(xintercept = settings$base_year,
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


ncbr_comp_fig_1990 <- function(ncbr_comp_ribbon, tge_all_long, settings, config) {
  
  ncbr_hist_btr <- tge_all_long %>% filter(Report == 'btr_2024',
                                           Year <= 2022,
                                           Year %in% config$annual_1990_fives)
  
  ncbr_hist_long <- tge_all_long %>% filter(!Report == 'btr_2024',
                                            Year <= 2022,
                                            Year %in% config$annual_1990_fives)
  
  ncbr_ribbon_clean <- ncbr_comp_ribbon %>% 
    filter(Year %in% config$annual_1990_fives)
  
  # tge_ends <- tge_all_long %>%
  #   filter(Year == max(Year),
  #          proj_name %in% c('usrr_wm_hiseq',
  #                           'gcam_wm_hiseq',
  #                           'nems_wm_hiseq'))
  
  var_palette = c(unique(tge_all_long$Report))
  
  #TODO: FLIP LEGEND ORDER, new to old desc
  figure <- ggplot() +
    geom_line(ncbr_hist_long, mapping = aes(Year,Value, group = Report, color = Report),size = 0.7) +
    geom_line(ncbr_hist_btr,mapping = aes(Year,Value, group = Report, color = Report),size = 0.7 ) +
    #geom_text_repel(tge_ends, mapping = aes(Year,Value, label = proj_name)) +
    geom_ribbon(ncbr_ribbon_clean, mapping = aes(x = Year,ymax = max, ymin = min,
                                                 fill = Report, 
                                                 color = Report),
                alpha = 0.4 ,
                size = 0.7) +
    geom_vline(xintercept = settings$base_year,
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
ncbr_comp_table <- function(tge_btr){}

