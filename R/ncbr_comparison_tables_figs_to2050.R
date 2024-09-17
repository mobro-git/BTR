
ncbr_comparison_figure50 <- function(ncbr_comp_ribbon, tge_all_long, settings, config, brvs = FALSE, brvs_name = NULL) {
  
  ncbr_hist_btr <- tge_all_long %>% filter(Report == '2024 BTR',
                                           Year <= 2022,
                                           Year %in% config$fives)
  
  if(brvs == TRUE){exclude = c('2024 BTR','2023 BR Voluntary Supplement')}
  if(brvs == FALSE){exclude = '2024 BTR'}
  
  ncbr_hist_long <- tge_all_long %>% filter(!Report %in% exclude,
                                            # Year <= 2035,
                                            Year %in% config$fives50)
  
  ncbr_ribbon_clean <- ncbr_comp_ribbon %>% 
    filter(Year %in% config$fives50,
           Report == '2024 BTR')
  
  if(brvs == TRUE){
    ncbr_brvs_ribbon_clean <- ncbr_comp_ribbon %>% 
      filter(Year %in% config$fives50,
             Report == "2023 BR Voluntary Supplement") %>%
      mutate(Report = brvs_name)
  }
  
  # tge_ends <- tge_all_long %>%
  #   filter(Year == max(Year),
  #          proj_name %in% c('usrr_wm_hiseq',
  #                           'gcam_wm_hiseq',
  #                           'nems_wm_hiseq'))
  var_palette = sort((unique(tge_all_long$Report)), decreasing = TRUE)
  
  if(!is.null(brvs_name)) {
    tge_all_long_rename <- tge_all_long %>%
      mutate(Report = case_when(
        Report == "2023 BR Voluntary Supplement" ~ brvs_name,
        TRUE ~ Report
      ))
    var_palette = sort(unique(tge_all_long_rename$Report), decreasing = TRUE)
  }
  #TODO: FLIP LEGEND ORDER, new to old desc
  if(brvs == FALSE){
    
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
      scale_x_continuous(breaks = c(2005, 2010, 2015, 2020, 2022, 2025, 2030, 2035, 2040, 2045, 2050), expand = c(0,0)) +
      theme_btr() +
      theme(
        legend.position = "inside",
        legend.position.inside = c(0.15, 0.2))
  }
  
  if(brvs == TRUE) {
    figure <- ggplot() +
      geom_line(ncbr_hist_long, mapping = aes(Year,Value, group = Report, color = Report),size = 0.7) +
      geom_line(ncbr_hist_btr,mapping = aes(Year,Value, group = Report, color = Report),size = 0.7 ) +
      #geom_text_repel(tge_ends, mapping = aes(Year,Value, label = proj_name)) +
      geom_ribbon(ncbr_ribbon_clean, mapping = aes(x = Year,ymax = max, ymin = min,
                                                   fill = Report, 
                                                   color = Report),
                  alpha = 0.4 ,
                  size = 0.7) +
      geom_ribbon(ncbr_brvs_ribbon_clean, mapping = aes(x = Year,ymax = max, ymin = min,
                                                        fill = Report, 
                                                        color = Report),
                  alpha = 0.4 ,
                  size = 0.7) +
      geom_vline(xintercept = settings$base_year,
                 linetype = 'dashed',
                 color = "black",
                 # size = 0.4,
                 alpha = 0.5) +
      scale_subpalette_single(var_palette) +
      labs(x = 'Year',
           y = expression(paste("Total Gross GHG Emissions (MMt ", CO[2], "e)", sep = ""))) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = c(2005, 2010, 2015, 2020, 2022, 2025, 2030, 2035, 2040, 2045, 2050), expand = c(0,0)) +
      theme_btr() +
      theme(
        legend.position = "inside",
        legend.position.inside = c(0.23, 0.2)) 
  }
  
  figure
  
}


ncbr_comp_fig_1990_50 <- function(ncbr_comp_ribbon, tge_all_long, settings, config) {
  
  ncbr_hist_btr <- tge_all_long %>% filter(Report == '2024 BTR',
                                           Year <= 2022,
                                           Year %in% config$annual_1990_fives50)
  
  ncbr_hist_long <- tge_all_long %>% filter(!Report == '2024 BTR',
                                           # Year <= 2035,
                                            Year %in% config$annual_1990_fives50)
  
  ncbr_ribbon_clean <- ncbr_comp_ribbon %>% 
    filter(Year %in% config$annual_1990_fives50,
           Report == '2024 BTR')
  
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
    scale_x_continuous(breaks = c(1990,1995,2000,2005, 2010, 2015, 2020, 2022, 2025, 2030, 2035, 2040, 2045, 2050), expand = c(0,0)) +
    theme_btr() +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.15, 0.2))
  
  figure
  
}
