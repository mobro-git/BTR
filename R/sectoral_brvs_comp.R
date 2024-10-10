
sector_brvs <- function(comp_df, var_choice, config, btr_models = TRUE) {
  
  if(btr_models == TRUE){
    
  plot_data <- comp_df %>%
    filter(model %in% config$model_wm_hist,
           variable == var_choice,
           year %in% config$table) %>%
    group_by(scenario,year,variable) %>%
    summarise(max = max(value),
              min = min(value))
  }
  
  if(btr_models == FALSE){
    
    plot_data <- comp_df %>%
      filter(variable == var_choice,
             year %in% config$table) %>%
      group_by(scenario,year,variable) %>%
      summarise(max = max(value),
                min = min(value))
  }
  
  var_palette <- unique(plot_data$scenario)
  
  plot <- ggplot(plot_data) +
    geom_ribbon(aes(x = year,
                    ymax = max,
                    ymin = min,
                    fill = scenario,
                    color = scenario),
                alpha = .6) +
    labs(y = "Mt CO2") +
    scale_x_continuous(breaks = config$table, expand = c(0,0)) +
    scale_subpalette_single(var_palette) +
    theme_btr()
}
