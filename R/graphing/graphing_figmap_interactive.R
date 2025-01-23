
# allows use of pipeline graphing to print one figure and return a ggplot object that can then be edited

data_from_graph <- function(plot_type, config, emf_data_long, figmap, figure_num, reg,
                            scenario_rename = FALSE,level_var = NULL, level_scen = NULL, level_mod = NULL) {

  # select the key variables, flip values, and merge with specific figure requests
  df <- preliminary_data_processing_for_plotting(emf_data_long, figmap)

  # full processing based on figure requests + print plot

  if(approved_plot_type(plot_type)) {

    data_processing_fn = get(paste(plot_type, "_figure_specific_data_processing", sep = ""))

    dat = df %>%
      filter(figure_no == figure_num & region == reg) %>%
      data_processing_fn(config)

    figure = unique(dat$title_name)
    selected = reg

    if(scenario_rename) {
      dat = dat %>%
        mutate(scenario = case_when(
          scenario == "wm" ~ "With Measures",
          scenario == "wam" ~ "With Additional Measures",
          TRUE~scenario))
    }

      if(!is.null(level_var)){
        dat <- dat %>%
          mutate(variable_rename = factor(variable_rename, levels = level_var))
      }
      if(!is.null(level_scen)){
        dat <- dat %>%
          mutate(scenario = factor(scenario, levels = level_scen))
      }
      if(!is.null(level_mod)){
        dat <- dat %>%
          mutate(model = factor(model, levels = level_mod))
      }

    dat

  }}


# allows use of pipeline graphing to print one figure and return a ggplot object that can then be edited

print_graph <- function(plot_type, config, data_long_clean, figmap, figure_num, reg = "United States",
                        scenario_rename = FALSE,level_var = NULL, level_scen = NULL, level_mod = NULL) {

  # select the key variables, flip values, and merge with specific figure requests
  df <- preliminary_data_processing_for_plotting(data_long_clean, figmap)

  # assign color palettes
  subpalettes = create_subpalettes(figmap, config)

  # full processing based on figure requests + print plot

  if(approved_plot_type(plot_type)) {

    data_processing_fn = get(paste(plot_type, "_figure_specific_data_processing", sep = ""))

    dat = df %>%
      filter(figure_no == figure_num & region == reg) %>%
      data_processing_fn(config)

    figure = unique(dat$title_name)
    #print(figure)
    selected = reg
    
    scen_factor <- c("With Measures",
                     "Low Fuel Cost",
                     "Advanced Technology",
                     "High Fuel Cost",
                     "High Fuel Cost, Low Cost Renewables",
                     "With Additional Measures",
                     "LEEP"
                     )

    if(scenario_rename) {
      dat = dat %>%
        mutate(scenario = case_when(
          scenario == "wm" ~ "With Measures",
          scenario == "wam" ~ "With Additional Measures",
          scenario == "leep_IRA" ~ "LEEP",
          scenario == "wm_adv" ~ "Advanced Technology",
          scenario == "wm_highfuelcost" ~ "High Fuel Cost",
          scenario == "wm_lowfuelcost" ~ "Low Fuel Cost",
          scenario == "wm_lowcostre_highfuelcost" ~ "High Fuel Cost, Low Cost Renewables",
          TRUE~scenario)) %>%
        mutate(scenario = factor(scenario, scen_factor))
    }

    if(!is.null(level_var)){
      dat <- dat %>%
        mutate(variable_rename = factor(variable_rename, levels = level_var))
    }
    if(!is.null(level_scen)){
      dat <- dat %>%
        mutate(scenario = factor(scenario, levels = level_scen))
    }
    if(!is.null(level_mod)){
      dat <- dat %>%
        mutate(model = factor(model, levels = level_mod))
    }

    dat

    if (approved_facet_type(dat) & check_dat_before_plotting(dat)) {
      plot_fn = get_plot_fn(plot_type, unique(dat$type))
      plot = call_plot_fn(dat, figure, selected, subpalettes, plot_type, plot_fn)
    }

    plot

  }
  }

# allows use of pipeline graphing to print one figure and return a ggplot object that can then be edited

print_graph_for_save <- function(plot_type, config, data_long_clean, figmap, figure_num, reg = "United States",
                        scenario_rename = TRUE,level_var = NULL, level_scen = NULL, level_mod = NULL) {
  
  # select the key variables, flip values, and merge with specific figure requests
  df <- preliminary_data_processing_for_plotting(data_long_clean, figmap)
  
  # assign color palettes
  subpalettes = create_subpalettes(figmap, config)
  
  # full processing based on figure requests + print plot
  
  if(approved_plot_type(plot_type)) {
    
    data_processing_fn = get(paste(plot_type, "_figure_specific_data_processing", sep = ""))
    
    dat = df %>%
      filter(figure_no == figure_num & region == reg) %>%
      data_processing_fn(config)
    
    figure = unique(dat$title_name)
    #print(figure)
    selected = reg
    
    scen_factor <- c("Reference",
                     "Low Fuel Cost",
                     "Advanced Technology",
                     "High Fuel Cost",
                     "High Fuel Cost, Low Cost Renewables"
    )
    
    if(scenario_rename) {
      dat = dat %>%
        mutate(scenario = case_when(
          scenario == "wm" ~ "Reference",
          scenario == "wm_adv" ~ "Advanced Technology",
          scenario == "wm_highfuelcost" ~ "High Fuel Cost",
          scenario == "wm_lowfuelcost" ~ "Low Fuel Cost",
          scenario == "wm_lowcostre_highfuelcost" ~ "High Fuel Cost, Low Cost Renewables",
          
          TRUE~scenario)) %>%
        mutate(scenario = factor(scenario, scen_factor))
    }
    
    if(!is.null(level_var)){
      dat <- dat %>%
        mutate(variable_rename = factor(variable_rename, levels = level_var))
    }
    if(!is.null(level_scen)){
      dat <- dat %>%
        mutate(scenario = factor(scenario, levels = level_scen))
    }
    if(!is.null(level_mod)){
      dat <- dat %>%
        mutate(model = factor(model, levels = level_mod))
    }
    
    dat
    
    if (approved_facet_type(dat) & check_dat_before_plotting(dat)) {
      plot_fn = get_plot_fn(plot_type, unique(dat$type))
      plot = call_plot_fn(dat, figure, selected, subpalettes, plot_type, plot_fn)
    }
    
    plot
    
    if(scenario_rename & unique(dat$color)=="scenario"){
      dat = dat %>% mutate(scenario = as.character(scenario))
      var_palette = unique(dat$scenario)
      plot = plot + 
        scale_subpalette_single(var_palette) +
        scale_linetype_manual(values = c("GCAM" = "solid", "OP-NEMS" = "dashed", "USREP-ReEDS" = "dotted"))
    }
    
    plot
    
  }}
