
## Function to ggsave figures with defaults and file format specified once for all figures
savefig = function(figure, fig_no, format = c("svg","png"), wd = 7, ht = 5, unit = "in") {

  for(item in format) {
  ggsave(filename = paste("output/final_figures/",item,"/Fig",fig_no,".",item, sep = ""),
         plot = figure,
         width = wd,
         height = ht,
         units = unit)
  }
}

savedata = function(df, fig_no) {

  write.csv(df, file = paste("output/final_figures/data/Fig",fig_no,".csv",sep=""), row.names = FALSE)

}

saveall = function(figure, df, fig_no, format = c("svg","png"), wd = 7, ht = 5, unit = "in") {

  for(item in format) {
    ggsave(filename = paste("output/final_figures/",item,"/Fig",fig_no,".",item, sep = ""),
           plot = figure,
           width = wd,
           height = ht,
           units = unit)
  }
  if(nrow(df) > 0) {
    savedata(df, fig_no)
  }

}


# This function does a bunch of stuff
# Calls the main functions to print a spagetti plot
# Then makes some dot plots to show the distributions: one for 2030, one for 2035
# The inputs are the same as for the print_graph() function, plus some new ones:
#   ymin and ymax: to manually set the y axis limits so that the axes line up across plots
#   plot_title: manually set the plot title of the combined plot
#   metric: what centrality metric to show on the dot plots: "mean" or "median"
dot_plots_sens = function(plot_type, config, emf_data_long, figmap, figure_num, reg, ymin, ymax, plot_title, metric, labels = FALSE, historic_coord = c(0,0), iralow_coord = c(0,0), irahigh_coord = c(0,0)) {
  subpalettes = create_subpalettes(figmap, config)

  # make the spagetti plot
  fig = print_graph(plot_type, config, emf_data_long, figmap, figure_num, reg)
  # update the axis range
  fig = fig + scale_y_continuous(limits = c(ymin, ymax)) +
    #scale_x_continuous(breaks = c(2005, 2021, 2025, 2030, 2035), labels = c(2005, 2021, 2025, 2030, 2035)) +
    scale_alpha(range = c(0.6, 1), guide = "none") +
    annotate("text", x = historic_coord[1], y = historic_coord[2], label = "Historic", color = "black", alpha = 1) +
    annotate("text", x = iralow_coord[1], y = iralow_coord[2], label = "IRA.Low", color = "#A1A1A1", alpha = 1) +
    annotate("text", x = irahigh_coord[1], y = irahigh_coord[2], label = "IRA.High", color = "#363636", alpha = 1) +
    theme(legend.position = "none", plot.title = element_blank())

  if (labels) {
    label_data = data_from_graph(plot_type, config, emf_data_long, figmap, figure_num, reg) %>%
      filter(year == 2035)
    print(unique(label_data$model))
    fig = fig +
      geom_text(data = label_data, aes(x = year + 0.25, y = value, label = model, color = scenario), size = 2, hjust = 0) +
      #geom_text_repel(data = label_data, aes(label = model), size = 2, hjust = 0) +
      scale_x_continuous(limits = c(NA,2037))
  }

  # pull the data from the graph and filter/summarize for 2030/2035
  df_30 = data_from_graph(plot_type, config, emf_data_long, figmap, figure_num, reg) %>%
    filter(year %in% c(2030))
  df_stat_30 = df_30 %>%
    group_by(scenario,year,variable_rename) %>%
    summarise(mean = mean(value, na.rm = TRUE),
              median = median(value, na.rm = TRUE))
  df_35 = data_from_graph(plot_type, config, emf_data_long, figmap, figure_num, reg) %>%
    filter(year %in% c(2035))
  df_stat_35 = df_35 %>%
    group_by(scenario,year,variable_rename) %>%
    summarise(mean = mean(value, na.rm = TRUE),
              median = median(value, na.rm = TRUE))

  stats = 0
  # these are the summary stats we'll eventually want to overlay on the plot to highlight the efficacy of the IRA
  # preira_mean_30 = df_stat_30[df_stat_30$scenario == "No IRA","mean"]$mean
  # ira_mean_30 = df_stat_30[df_stat_30$scenario == "IRA","mean"]$mean
  # preira_mean_35 = df_stat_35[df_stat_35$scenario == "No IRA","mean"]$mean
  # ira_mean_35 = df_stat_35[df_stat_35$scenario == "IRA","mean"]$mean
  # preira_med_30 = df_stat_30[df_stat_30$scenario == "No IRA","median"]$median
  # ira_med_30 = df_stat_30[df_stat_30$scenario == "IRA","median"]$median
  # preira_med_35 = df_stat_35[df_stat_35$scenario == "No IRA","median"]$median
  # ira_med_35 = df_stat_35[df_stat_35$scenario == "IRA","median"]$median
  #
  # # calculate raw differences
  # mean_diff_30 = preira_mean_30 - ira_mean_30
  # mean_diff_35 = preira_mean_35 - ira_mean_35
  # med_diff_30 = preira_med_30 - ira_med_30
  # med_diff_35 = preira_med_35 - ira_med_35
  #
  # # calculate percent differences
  # mean_pctdiff_30 = mean_diff_30 / preira_mean_30
  # mean_pctdiff_35 = mean_diff_35 / preira_mean_35
  # med_pctdiff_30 = med_diff_30 / preira_med_30
  # med_pctdiff_35 = med_diff_35 / preira_med_35
  #
  # # stick em all in a data frame
  # # stats = 0
  # stats = data.frame("year" = c(2030,2035,2030,2035), "metric" = c("mean","mean","median","median"),
  #                    "No IRA mean" = c(preira_mean_30, preira_mean_35, preira_med_30, preira_med_35),
  #                    "IRA mean" = c(ira_mean_30, ira_mean_35, ira_med_30, ira_med_35),
  #                    "difference" = c(mean_diff_30, mean_diff_35, med_diff_30, med_diff_35),
  #                    "percent difference" = c(mean_pctdiff_30, mean_pctdiff_35, med_pctdiff_30, med_pctdiff_35))

  # still not entirely sure what this does...
  # subpalettes = create_subpalettes(figmap_leep_timeseries, config)

  # a few of the ggplot lines of code vary based on the year or on the metric chosen, so conditionally choose the right one here
  if (metric == "mean") {
    segment_code_30 = geom_segment(data = df_stat_30,
                                   aes(x = year - 0.04, xend = year + 0.04, y = mean, yend = mean, color = scenario),
                                   size = 1)
    segment_code_35 = geom_segment(data = df_stat_35,
                                   aes(x = year - 0.04, xend = year + 0.04, y = mean, yend = mean, color = scenario),
                                   size = 1)
    text_code = geom_text(data = df_stat_30, aes(x = year - 0.08, y = mean, color = scenario),
                          label = "Avg", size = 3, hjust = 0.5, angle = 90, position = position_dodge2(width = 0.05))
  } else if (metric == "median") {
    segment_code_30 = geom_segment(data = df_stat_30,
                                   aes(x = year - 0.04, xend = year + 0.04, y = median, yend = median, color = scenario),
                                   size = 1)
    segment_code_35 = geom_segment(data = df_stat_35,
                                   aes(x = year - 0.04, xend = year + 0.04, y = median, yend = median, color = scenario),
                                   size = 1)
    text_code = geom_text(data = df_stat_30, aes(x = year - 0.08, y = median, color = scenario),
                          label = "Med", size = 3, hjust = 0.5, angle = 90, position = position_dodge2(width = 0.05))
  }

  # dot plot for 2030
  dots_30 = ggplot() +
    geom_point(data = df_30, aes(x = year, y = value, color = scenario), shape = 1, size = 1.5, position = position_dodge(width = 0.05)) +
    segment_code_30 +
    #text_code +
    scale_x_continuous(breaks = c(2030), labels = c("2030"), limits = c(2029.9, 2030.1)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    theme_emf() +
    scale_subpalette(subpalettes, "Emissions Sensitivity") +
    theme(panel.grid = element_blank(), plot.title = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(), axis.title.x = element_blank(),
          axis.ticks = element_blank(), panel.border = element_blank(),
          legend.position = "none", plot.margin = margin(0,0,0,0))

  # dot plot for 2035
  dots_35 = ggplot() +
    geom_point(data = df_35, aes(x = year, y = value, color = scenario), shape = 1, size = 1.5, position = position_dodge(width = 0.05)) +
    segment_code_35 +
    scale_x_continuous(breaks = c(2035), labels = c("2035"), limits = c(2034.9, 2035.1)) +
    scale_y_continuous(limits = c(ymin,ymax)) +
    theme_emf() +
    scale_subpalette(subpalettes, "Emissions Sensitivity") +
    theme(panel.grid = element_blank(), plot.title = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(), axis.title.x = element_blank(),
          axis.ticks = element_line(color = "black"),
          axis.ticks.length = unit(-0.15, "cm"), panel.border = element_blank(),
          legend.position = "none", plot.margin = margin(0,0,0,0))

  # combine all the plots
  final = fig + dots_30 + dots_35 +
    plot_annotation(title = plot_title, theme = theme(plot.title = element_blank())) +
    plot_layout(widths = c(10,1,1)) +
    theme(plot.margin = margin(0,0,0,0))

  # return the full plot + the components + the summary stats
  return(list(
    full = final,
    line = fig,
    dot1 = dots_30,
    dot2 = dots_35,
    stats = stats
  ))
}

# p = dot_plots("time_series", config, clean_data, figmap_leep_timeseries, 4, "United States", 0, 2500, "Emissions|CO2|Energy|Demand|Electricity", metric = "median")
# print(p$full)
# print(p$stats)
sens_dot_plot = function(dta, title, figmap, config, far_left = FALSE, single = FALSE, ps = FALSE, ymin = 0, ymax = 0,
                         ira_coord = c(0,0), low_coord = c(0,0), high_coord = c(0,0)) {
  subpalettes = create_subpalettes(figmap, config)
  if (far_left) {
    point_code = geom_point(aes(x = year + stagger, y = value, color = scenario, shape = as.factor(stagger)), size = 2)
  } else {
    #point_code = geom_point(aes(x = year + stagger, y = value, color = model), shape = 1, size = 2)
    point_code = geom_point(aes(x = year + stagger, y = value, color = scenario, shape = as.factor(stagger)), size = 2)
    #point_code = geom_point(aes(x = year + stagger, y = value, shape = as.factor(stagger)), size = 2, color = "black")
  }

  if (far_left) {
    stats = dta %>%
      mutate(scenario = case_when(scenario == "Mod" ~ "IRA", TRUE ~ scenario)) %>%
      group_by(year, stagger, scenario) %>%
      summarize(median = median(value))
    if (single) {
      width = 0.2
      left_margin = 11.5
    } else {
      width = 0.6
      left_margin = 0.5
    }
    segment_code = geom_segment(data = stats,
                                aes(x = year + stagger - width, xend = year + stagger + width,
                                    y = median, yend = median, color = scenario),
                                linewidth = 0.5)
  } else {
    if (!("stagger") %in% colnames(dta)) {
      dta$stagger = 0
    }
    stats = dta %>%
      mutate(scenario = case_when(scenario == "Mod" ~ "IRA", TRUE ~ scenario)) %>%
      group_by(year, stagger) %>%
      summarize(median = median(value))
    segment_code = geom_segment(aes(x = year, xend = year, y = -5000, yend = -5000))
    left_margin = 0.5
  }

  stats = dta %>%
    mutate(scenario = case_when(scenario == "Core" ~ "IRA", TRUE ~ scenario)) %>%
    group_by(year, stagger, scenario) %>%
    summarize(median = median(value))

  p = dta %>% ggplot() +
    point_code +
    segment_code +
    scale_x_continuous(breaks = c(2030, 2035), labels = c(2030, 2035), limits = c(2028, 2037)) +
    scale_y_continuous(limits = c(ymin, ymax), labels = scales::comma) +
    #scale_subpalette(subpalettes,"ZZZZZ") +
    theme_emf() +
    ggtitle(title) +
    theme(axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 8),
          plot.margin = margin(0.7,1,0.7,left_margin), legend.position = "none")
  ylabel = expression(paste("Electricity Sector Emissions (Mt C", O[2], "/yr)"))
  if (!ps) {
    ylabel = expression(paste("Economy-Wide Emissions (Mt C", O[2], "/yr)"))
  }
  #     labs(y = expression(paste("Sectoral Direct + Indirect Emissions (Mt C", O[2], "/yr)")), title = title) +
  if (!far_left) {
    p = p +
      geom_text_repel(aes(x = year + stagger, y = value, label = scenario, segment.alpha = 0.6, segment.size = 0.2), point.padding = 0.2,
                      size = 1.5, hjust = 0, vjust = 0, max.time = 6, force_pull = 1, seed = 42,
                      min.segment.length = 0.0) +
      coord_cartesian(clip = "off") +
      theme(axis.text.y = element_blank(), axis.title.y = element_blank(),)
    opt_label = "Optimistic Emis"
    pes_label = "Pessimistic Emis"
  } else {
    p = p +
      labs(y = ylabel) +
      #scale_subpalette(subpalettes, "Sensitivity Dots") +
      scale_color_manual(values = c("IRA" = "black", "IRA.Low" = "#6a959b", "IRA.High" = "#0083ca"), guide = "none")
    opt_label = "Optimistic"
    pes_label = "Pessimistic"
  }

  if(sum(ira_coord) != 0) {
    p = p +
      annotate("text", x = ira_coord[1], y = ira_coord[2], size = 2,
               label = "Moderate", color = "black", alpha = 1, hjust = -0.2)
  }
  if(sum(low_coord) != 0) {
    p = p +
      annotate("text", x = low_coord[1], y = low_coord[2], size = 2,
               label = pes_label, color = "#6a959b", alpha = 1, hjust = -0.2)
  }
  if(sum(high_coord) != 0) {
    p = p +
      annotate("text", x = high_coord[1], y = high_coord[2], size = 2,
               label = opt_label, color = "#0083ca", alpha = 1, hjust = -0.2)
  }

  return(list(
    "plot" = p,
    "stats" = stats
  ))
}


# function for spaghetti plots + dot plots
dot_plots = function(plot_type, config, emf_data_long, figmap, figure_num, reg, ymin, ymax, plot_title, metric, labels = FALSE, hist_year = 2021, historic_coord = c(0,0), preira_coord = c(0,0), ira_coord = c(0,0), ylab = "") {
  subpalettes = create_subpalettes(figmap, config)

  # make the spagetti plot
  fig = print_graph(plot_type, config, emf_data_long, figmap, figure_num, reg)
  # update the axis range
  fig = fig + scale_y_continuous(limits = c(ymin, ymax)) +
    scale_x_continuous(breaks = c(2005,2010, hist_year, 2025, 2030, 2035), labels = c(2005, 2010, hist_year, 2025, 2030, 2035)) +
    scale_alpha(range = c(0.6, 1), guide = "none") +
    labs(y = ylab) +
    annotate("text", x = historic_coord[1], y = historic_coord[2], label = "Historical", color = "black", alpha = 1) +
    annotate("text", x = preira_coord[1], y = preira_coord[2], label = "No IRA", color = "#F28063", alpha = 1) +
    annotate("text", x = ira_coord[1], y = ira_coord[2], label = "IRA", color = "#0388B3", alpha = 1) +
    theme(legend.position = "none", plot.title = element_blank())

  if (labels) {
    label_data = data_from_graph(plot_type, config, emf_data_long, figmap, figure_num, reg) %>%
      filter(year == 2035)
    print(unique(label_data$model))
    fig = fig +
      geom_text(data = label_data, aes(x = year + 0.25, y = value, label = model, color = scenario), size = 2, hjust = 0) +
      #geom_text_repel(data = label_data, aes(label = model), size = 2, hjust = 0) +
      scale_x_continuous(limits = c(NA,2037))
  }

  # pull the data from the graph and filter/summarize for 2030/2035
  df_30 = data_from_graph(plot_type, config, emf_data_long, figmap, figure_num, reg) %>%
    filter(year %in% c(2030))
  df_stat_30 = df_30 %>%
    group_by(scenario,year,variable_rename) %>%
    summarise(mean = mean(value, na.rm = TRUE),
              median = median(value, na.rm = TRUE))
  df_35 = data_from_graph(plot_type, config, emf_data_long, figmap, figure_num, reg) %>%
    filter(year %in% c(2035))
  df_stat_35 = df_35 %>%
    group_by(scenario,year,variable_rename) %>%
    summarise(mean = mean(value, na.rm = TRUE),
              median = median(value, na.rm = TRUE))

  # these are the summary stats we'll eventually want to overlay on the plot to highlight the efficacy of the IRA
  preira_mean_30 = df_stat_30[df_stat_30$scenario == "No IRA","mean"]$mean
  ira_mean_30 = df_stat_30[df_stat_30$scenario == "IRA","mean"]$mean
  preira_mean_35 = df_stat_35[df_stat_35$scenario == "No IRA","mean"]$mean
  ira_mean_35 = df_stat_35[df_stat_35$scenario == "IRA","mean"]$mean
  preira_med_30 = df_stat_30[df_stat_30$scenario == "No IRA","median"]$median
  ira_med_30 = df_stat_30[df_stat_30$scenario == "IRA","median"]$median
  preira_med_35 = df_stat_35[df_stat_35$scenario == "No IRA","median"]$median
  ira_med_35 = df_stat_35[df_stat_35$scenario == "IRA","median"]$median

  # calculate raw differences
  mean_diff_30 = preira_mean_30 - ira_mean_30
  mean_diff_35 = preira_mean_35 - ira_mean_35
  med_diff_30 = preira_med_30 - ira_med_30
  med_diff_35 = preira_med_35 - ira_med_35

  # calculate percent differences
  mean_pctdiff_30 = mean_diff_30 / preira_mean_30
  mean_pctdiff_35 = mean_diff_35 / preira_mean_35
  med_pctdiff_30 = med_diff_30 / preira_med_30
  med_pctdiff_35 = med_diff_35 / preira_med_35

  # stick em all in a data frame
  stats = 0
  # stats = data.frame("year" = c(2030,2035,2030,2035), "metric" = c("mean","mean","median","median"),
  #                    "No IRA mean" = c(preira_mean_30, preira_mean_35, preira_med_30, preira_med_35),
  #                    "IRA mean" = c(ira_mean_30, ira_mean_35, ira_med_30, ira_med_35),
  #                    "difference" = c(mean_diff_30, mean_diff_35, med_diff_30, med_diff_35),
  #                    "percent difference" = c(mean_pctdiff_30, mean_pctdiff_35, med_pctdiff_30, med_pctdiff_35))

  # still not entirely sure what this does...
  subpalettes = create_subpalettes(figmap, config)

  # a few of the ggplot lines of code vary based on the year or on the metric chosen, so conditionally choose the right one here
  if (metric == "mean") {
    segment_code_30 = geom_segment(data = df_stat_30,
                                   aes(x = year - 0.04, xend = year + 0.04, y = mean, yend = mean, color = scenario),
                                   size = 1)
    segment_code_35 = geom_segment(data = df_stat_35,
                                   aes(x = year - 0.04, xend = year + 0.04, y = mean, yend = mean, color = scenario),
                                   size = 1)
    text_code = geom_text(data = df_stat_30, aes(x = year - 0.08, y = mean, color = scenario),
                          label = "Avg", size = 3, hjust = 0.5, angle = 90, position = position_dodge2(width = 0.05))
  } else if (metric == "median") {
    segment_code_30 = geom_segment(data = df_stat_30,
                                   aes(x = year - 0.04, xend = year + 0.04, y = median, yend = median, color = scenario),
                                   size = 1)
    segment_code_35 = geom_segment(data = df_stat_35,
                                   aes(x = year - 0.04, xend = year + 0.04, y = median, yend = median, color = scenario),
                                   size = 1)
    text_code = geom_text(data = df_stat_30, aes(x = year - 0.08, y = median, color = scenario),
                          label = "Med", size = 3, hjust = 0.5, angle = 90, position = position_dodge2(width = 0.05))
  }

  # dot plot for 2030
  dots_30 = ggplot() +
    geom_point(data = df_30, aes(x = year, y = value, color = scenario), shape = 1, size = 1.5, position = position_dodge(width = 0.05)) +
    segment_code_30 +
    #text_code +
    scale_x_continuous(breaks = c(2030), labels = c("2030"), limits = c(2029.9, 2030.1)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    theme_emf() +
    scale_subpalette(subpalettes, "Emissions|CO2|Energy|Demand|Industry") +
    theme(panel.grid = element_blank(), plot.title = element_blank(),
          axis.text.y = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.y = element_blank(), axis.title.x = element_blank(),
          axis.ticks = element_blank(), panel.border = element_blank(),
          legend.position = "none", plot.margin = margin(0,0,0,0))

  # grapes
  # dot plot for 2035
  dots_35 = ggplot() +
    geom_point(data = df_35, aes(x = year, y = value, color = scenario), shape = 1, size = 1.5, position = position_dodge(width = 0.05)) +
    segment_code_35 +
    scale_x_continuous(breaks = c(2035), labels = c("2035"), limits = c(2034.9, 2035.1)) +
    scale_y_continuous(limits = c(ymin,ymax)) +
    theme_emf() +
    scale_subpalette(subpalettes, "Emissions|CO2|Energy|Demand|Industry") +
    theme(panel.grid = element_blank(), plot.title = element_blank(),
          axis.text.y = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.y = element_blank(), axis.title.x = element_blank(),
          axis.ticks = element_blank(), panel.border = element_blank(),
          legend.position = "none", plot.margin = margin(0,0,0,0))

  # combine all the plots
  final = fig + dots_30 + dots_35 +
    plot_annotation(title = plot_title, theme = theme(plot.title = element_blank())) +
    plot_layout(widths = c(10,1,1)) +
    theme(plot.margin = margin(0,0,0,0))

  # return the full plot + the components + the summary stats
  return(list(
    full = final,
    line = fig,
    dot1 = dots_30,
    dot2 = dots_35,
    stats = stats
  ))
}


duplicate_historical_data <- function(data_long, hist_model, hist_year, criteria = FALSE) {
  # short circuit
  if (hist_model == "none") {
    return(data_long)
  }

  # this is the data we'll stamp for every model
  copy_data = data_long %>% filter(model == hist_model, year == hist_year)

  # this is all the historical data, don't lose it
  hist_data = data_long %>% filter(model == hist_model & year <= hist_year)
  # temporary data_long which will be appended many times
  data_in = data_long %>% filter(year >= hist_year & model != hist_model)

  if (criteria) {
    data_25 = data_long %>% filter(model != hist_model, year == 2025) %>%
      mutate(year = hist_year)
    data_in = bind_rows(data_in, data_25)
  }

  data_wide = data_in %>% left_join(select(copy_data,c(value,year,variable,region)), by = c("year", "variable","region")) %>%
    mutate(value = case_when(
      year == hist_year & !is.na(value.y) ~ value.y,
      TRUE ~ value.x
    )) %>%
    select(-c(value.x,value.y))

  data_final = bind_rows(data_wide, hist_data)

  # the end
  return(data_final)
}

# remove all historical data and replace with EIA-LTS 2020 data for connectivity purposes
# clean_data2 = clean_data %>% duplicate_historical_data("EIA-LTS",2020)

# function for coneplot with dots overlaid
lts_coneplot_with_dots = function(config, data, figmap, lts_fignum, leep_fignum, region, ymin, ymax, title, titlesize = 8, metric = "mean", facet = FALSE) {
  # update the historical data (is this even necessary anymore?)
  data = data %>% duplicate_historical_data("EIA-LTS",2020) %>% filter(year <= 2035)

  # make the LTS cone plot
  fig1 = print_graph("cone_uncertainty", config, data, figmap, lts_fignum, region) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    # scale_x_continuous(breaks = c(2005, 2021, 2030, 2035, 2050), labels = c(2005, 2021, 2030, 2035, 2050)) +
    scale_x_continuous(breaks = c(2005, 2021, 2030, 2035), labels = c(2005, 2021, 2030, 2035)) +
    #again
    theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5, size = titlesize))


  # extract points and summary statistics for 2030 and 2035
  df_30 = data_from_graph("cone_uncertainty", config, data, figmap, leep_fignum, region) %>%
    filter(year %in% c(2030))
  df_stat_30 = df_30 %>%
    group_by(scenario,year,variable_rename) %>%
    summarise(mean = mean(value),
              median = median(value))

  df_35 = data_from_graph("cone_uncertainty", config, data, figmap, leep_fignum, region) %>%
    filter(year %in% c(2035))
  df_stat_35 = df_35 %>%
    group_by(scenario,year,variable_rename) %>%
    summarise(mean = mean(value),
              median = median(value))

  if (facet) {
    dash_len = 2.0
  } else {
    dash_len = 0.5
  }

  if (metric == "mean") {
    segment_code_30 = geom_segment(data = df_stat_30, aes(x = year - dash_len, xend = year + dash_len, y = mean, yend = mean,
                                                          color = variable_rename), position = position_dodge2(width = 0.5), linewidth = 1)
    segment_code_35 = geom_segment(data = df_stat_35, aes(x = year - dash_len, xend = year + dash_len, y = mean, yend = mean,
                                                          color = variable_rename), position = position_dodge2(width = 0.5), linewidth = 1)
  } else if (metric == "median") {
    segment_code_30 = geom_segment(data = df_stat_30, aes(x = year - dash_len, xend = year + dash_len, y = median, yend = median,
                                                          color = variable_rename), position = position_dodge2(width = 0.5), linewidth = 1)
    segment_code_35 = geom_segment(data = df_stat_35, aes(x = year - dash_len, xend = year + dash_len, y = median, yend = median,
                                                          color = variable_rename), position = position_dodge2(width = 0.5), linewidth = 1)
  }


  fig2 = fig1 +
    geom_point(data = df_30, aes(x = year, y = value, color = variable_rename),
               shape = 1, size = 2, position = position_dodge(width = 1)) +
    segment_code_30 +
    geom_point(data = df_35, aes(x = year, y = value, color = variable_rename),
               shape = 1, size = 2, position = position_dodge(width = 1)) +
    segment_code_35 +
    ggtitle(title) +
    theme(legend.position = "none", axis.title.x = element_blank())
  #bottom1
  return(fig2)

}

medians_table = function(plot_type, config, data, figmap, plot_num, region, models_to_exclude = c()) {
  med_dta = data_from_graph(plot_type, config, data, figmap, plot_num, region)
  if (length(models_to_exclude) > 0) {
    med_dta = med_dta %>% filter(!(model %in% models_to_exclude))
  }

  # calculate difference in medians
  diff_med = med_dta %>%
    filter(year %in% c(2030, 2035)) %>%
    mutate(scenario = case_when(
      scenario == "No IRA" ~ "No_IRA",
      TRUE ~ scenario
    )) %>%
    group_by(year, scenario, variable, unit) %>%
    summarize(median = median(value)) %>%
    data.frame() %>%
    pivot_wider(names_from = scenario, values_from = median, names_prefix = "median_") %>%
    mutate(absolute_diff_of_medians = median_IRA - median_No_IRA,
           percent_diff_of_medians = (median_IRA - median_No_IRA) / median_No_IRA)

  # calculate median of differences
  med_diff = med_dta %>%
    data.frame() %>%
    select(model, scenario, variable, unit, year, value) %>%
    filter(year %in% c(2030, 2035)) %>%
    mutate(scenario = case_when(
      scenario == "No IRA" ~ "No_IRA",
      TRUE ~ scenario
    )) %>%
    pivot_wider(names_from = scenario, values_from = value, names_prefix = "value_") %>%
    mutate(absolute_diff = value_IRA - value_No_IRA,
           percent_diff = (value_IRA - value_No_IRA) / value_No_IRA) %>%
    group_by(variable, unit, year) %>%
    summarize(median_of_abs_diffs = median(absolute_diff),
              median_of_pct_diffs = median(percent_diff)) %>%
    data.frame()

  # stick 'em together
  full_diffs = diff_med %>% left_join(
    select(med_diff, c(year, variable, median_of_abs_diffs, median_of_pct_diffs)),
    by = c("year","variable")
  ) %>%
    arrange(variable, year)

  return(full_diffs)
}

#medians_table("time_series", config, clean_data, figmap_leep_timeseries, 9, "United States")

emis_stack = function(dta, title, figmap, config, econwide = FALSE) {
  subpalettes = create_subpalettes(figmap, config)
  totals = dta %>% group_by(year) %>%
    summarize(value = sum(value))

  dta = dta %>% group_by(year, variable_rename) %>%
    summarize(value = sum(value))

  if (econwide) {
    bar_code = geom_bar(aes(x = year, y = value, fill = variable_rename), position = position_stack(), stat = "identity", color = "white", linewidth = 0.25)
  } else {
    bar_code = geom_bar(aes(x = year, y = value, fill = variable_rename,
                            color = variable_rename, alpha = variable_rename), position = position_stack(reverse = TRUE), stat = "identity")
  }

  p = dta %>% ggplot() +
    bar_code +
    #geom_text(aes(x = year, y = value, label = round(value, 0), fill = variable_rename), size = 2.5, position = position_stack(vjust = 0.5, reverse = FALSE)) +
    #geom_text(data = totals, aes(x = year, y = value, label = round(value, 0)), vjust = -0.4, stat = "identity", size = 2.5) +
    scale_x_continuous(breaks = c(2005, 2021), labels = c(2005, 2021)) +
    scale_y_continuous(expand = c(0,0), limits = c(0,6200),labels = scales::comma) +
    labs(y = expression(paste("End-Use Emissions (Mt C", O[2], "/yr)")), title = title) +
    # expression(paste("Economy-Wide C", O[2])),
    scale_subpalette(subpalettes, "Emissions Stack") +
    theme_emf() +
    theme(axis.ticks.x = element_blank(), axis.ticks.y = element_line(color = "black"), axis.ticks.length = unit(-0.15, "cm"), axis.title.x = element_blank(), plot.title = element_blank()) +
    bottom1

  return(p)
}


# A function to write all csvs in the final_figures/data/folder to a single excel document
# need to have installed package "openxlsx"
compile_all_data = function() {
  all_data = list.files("./output/final_figures/data", full.names = FALSE)
  #print(all_data)

  data_list = list()
  for (file in all_data) {
    fignum = strsplit(file, "_")[[1]][1]
    if(grepl(".csv",fignum)) {
      fignum = substr(fignum,1,nchar(fignum) - 4)
    }
    #print(fignum)
    if (substr(fignum,1,3) == "Fig") {
      df = read.csv(paste0("./output/final_figures/data/",file))
      data_list[[fignum]] = df
    }
  }

  openxlsx::write.xlsx(data_list, "./output/final_figures/data/ALL_DATA.xlsx")
  return(data_list)
}

# trim down the data extracts
clean_supplemental_data = function(df, fig_no) {
  df_clean = df %>% data.frame() %>%
    mutate(figure_num = fig_no) %>%
    rename("label" = "variable_rename") %>%
    select(figure_num, model, scenario, variable, unit, year , label, value)
  return(df_clean)
}

dot_plots_but_with_arrows = function(plot_type, config, emf_data_long, figmap, figure_num, reg, ymin, ymax, plot_title, metric, labels = FALSE, hist_year = 2021, historic_coord = c(0,0), preira_coord = c(0,0), ira_coord = c(0,0)) {
  subpalettes = create_subpalettes(figmap, config)

  # make the spagetti plot
  fig = print_graph(plot_type, config, emf_data_long, figmap, figure_num, reg)
  # update the axis range
  fig = fig + scale_y_continuous(limits = c(ymin, ymax)) +
    scale_x_continuous(breaks = c(2005,2010, hist_year, 2025, 2030, 2035), labels = c(2005, 2010, hist_year, 2025, 2030, 2035)) +
    scale_alpha(range = c(0.6, 1), guide = "none") +
    annotate("text", x = historic_coord[1], y = historic_coord[2], label = "Historic", color = "black", alpha = 1) +
    annotate("text", x = preira_coord[1], y = preira_coord[2], label = "No IRA", color = "#F28063", alpha = 1) +
    annotate("text", x = ira_coord[1], y = ira_coord[2], label = "IRA", color = "#0388B3", alpha = 1) +
    theme(legend.position = "none", plot.title = element_blank())

  if (labels) {
    label_data = data_from_graph(plot_type, config, emf_data_long, figmap, figure_num, reg) %>%
      filter(year == 2035)
    print(unique(label_data$model))
    fig = fig +
      geom_text(data = label_data, aes(x = year + 0.25, y = value, label = model, color = scenario), size = 2, hjust = 0) +
      #geom_text_repel(data = label_data, aes(label = model), size = 2, hjust = 0) +
      scale_x_continuous(limits = c(NA,2037))
  }

  # pull the data from the graph and filter/summarize for 2030/2035
  df_30 = data_from_graph(plot_type, config, emf_data_long, figmap, figure_num, reg) %>%
    filter(year %in% c(2030))
  df_stat_30 = df_30 %>%
    group_by(scenario,year,variable_rename) %>%
    summarise(mean = mean(value, na.rm = TRUE),
              median = median(value, na.rm = TRUE))
  df_35 = data_from_graph(plot_type, config, emf_data_long, figmap, figure_num, reg) %>%
    filter(year %in% c(2035))
  df_stat_35 = df_35 %>%
    group_by(scenario,year,variable_rename) %>%
    summarise(mean = mean(value, na.rm = TRUE),
              median = median(value, na.rm = TRUE))

  df_lines_30 = data_from_graph(plot_type, config, emf_data_long, figmap, figure_num, reg) %>%
    filter(year %in% c(2030)) %>%
    group_by(model, year) %>%
    summarize(min = min(value),
              max = max(value)) %>%
    mutate(diff = max - min) %>%
    mutate(median = median(diff)) %>%
    arrange(desc(max)) %>%
    data.frame() %>%
    mutate(stagger = row_number()) %>%
    mutate(stagger = stagger - 1) %>%
    mutate(stagger = stagger / max(stagger)) %>%
    mutate(stagger = stagger - 0.5)

  df_arrow_30 = data_from_graph(plot_type, config, emf_data_long, figmap, figure_num, reg) %>%
    filter(year %in% c(2030), scenario == "IRA") %>%
    select(model, value) %>%
    rename(IRA_value = value)

  df_lines_30 = df_lines_30 %>% left_join(df_arrow_30, by = "model") %>%
    mutate(arrow_direction = case_when(
      max == IRA_value ~ "up",
      min == IRA_value ~ "down",
      TRUE ~ "uh-oh"
    )) %>%
    mutate(arrow_end = case_when(arrow_direction == "up" ~ max, TRUE ~ min),
           arrow_start = case_when(arrow_direction == "up" ~ min, TRUE ~ max))

  #View(df_lines_30)

  df_lines_35 = data_from_graph(plot_type, config, emf_data_long, figmap, figure_num, reg) %>%
    filter(year %in% c(2035)) %>%
    group_by(model, year) %>%
    summarize(min = min(value),
              max = max(value)) %>%
    mutate(diff = max - min) %>%
    mutate(median = median(diff)) %>%
    arrange(desc(max)) %>%
    data.frame() %>%
    mutate(stagger = row_number()) %>%
    mutate(stagger = stagger - 1) %>%
    mutate(stagger = stagger / max(stagger)) %>%
    mutate(stagger = stagger - 0.5)
  # View(df_lines_35)

  df_arrow_35 = data_from_graph(plot_type, config, emf_data_long, figmap, figure_num, reg) %>%
    filter(year %in% c(2035), scenario == "IRA") %>%
    select(model, value) %>%
    rename(IRA_value = value)

  df_lines_35 = df_lines_35 %>% left_join(df_arrow_35, by = "model") %>%
    mutate(arrow_direction = case_when(
      max == IRA_value ~ "up",
      min == IRA_value ~ "down",
      TRUE ~ "uh-oh"
    )) %>%
    mutate(arrow_end = case_when(arrow_direction == "up" ~ max, TRUE ~ min),
           arrow_start = case_when(arrow_direction == "up" ~ min, TRUE ~ max))

  # still not entirely sure what this does...
  subpalettes = create_subpalettes(figmap_leep_timeseries, config)

  # dot plot for 2030
  dots_30 = ggplot() +
    # geom_point(data = df_30, aes(x = year, y = value, color = scenario),
    #            shape = 1, size = 1.5, position = position_dodge(width = 0.05)) +
    geom_segment(data = df_lines_30, aes(x = year + stagger, xend = year + stagger, y = arrow_start, yend = arrow_end), color = "black",
                 size = 0.5, arrow = arrow(length = unit(0.2, "cm"), type = "closed", angle = 25)) +
    # geom_segment(data = df_lines_30, aes(x = year - 0.5, xend = year + 0.5, y = median, yend = median), color = "black",
    #              size = 1) +
    # #segment_code_30 +
    #text_code +
    scale_x_continuous(breaks = c(2030), labels = c("2030"), limits = c(2029, 2031)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    theme_emf() +
    scale_subpalette(subpalettes, "Emissions|CO2|Energy|Demand|Industry") +
    theme(panel.grid = element_blank(), plot.title = element_blank(),
          axis.text.y = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.y = element_blank(), axis.title.x = element_blank(),
          axis.ticks = element_blank(), panel.border = element_blank(),
          legend.position = "none", plot.margin = margin(0,0,0,0))

  # grapes
  # dot plot for 2035
  dots_35 = ggplot() +
    # geom_point(data = df_35, aes(x = year, y = value, color = scenario),
    #            shape = 1, size = 1.5, position = position_dodge(width = 0.05)) +
    geom_segment(data = df_lines_35, aes(x = year + stagger, xend = year + stagger, y = arrow_start, yend = arrow_end), color = "black",
                 size = 0.5, arrow = arrow(length = unit(0.2, "cm"), type = "closed", angle = 25)) +
    # geom_segment(data = df_lines_35, aes(x = year - 0.5, xend = year + 0.5, y = median, yend = median), color = "black",
    #              size = 1) +
    # segment_code_35 +
    scale_x_continuous(breaks = c(2035), labels = c("2035"), limits = c(2034, 2036)) +
    scale_y_continuous(limits = c(ymin,ymax)) +
    theme_emf() +
    scale_subpalette(subpalettes, "Emissions|CO2|Energy|Demand|Industry") +
    theme(panel.grid = element_blank(), plot.title = element_blank(),
          axis.text.y = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.y = element_blank(), axis.title.x = element_blank(),
          axis.ticks = element_blank(), panel.border = element_blank(),
          legend.position = "none", plot.margin = margin(0,0,0,0))

  # combine all the plots
  final = fig + dots_30 + dots_35 +
    plot_annotation(title = plot_title, theme = theme(plot.title = element_blank())) +
    plot_layout(widths = c(10,1,1)) +
    theme(plot.margin = margin(0,0,0,0))

  # return the full plot + the components + the summary stats
  return(list(
    full = final,
    line = fig,
    dot1 = dots_30,
    dot2 = dots_35,
    stats = stats
  ))
}





tableit = function(table, table_name, col_names) {

  ft_table = flextable(table) %>%
    add_header_row(values = col_names, colwidths = c(1,3,3)) %>%
    add_header_lines(values = table_name) %>%
    theme_booktabs(bold_header = TRUE) %>%
    align(align = "center", part = "all") %>%
    vline(j = c(1,4), border = fp_border_default()) %>%
    border_outer() %>%
    border_inner() %>%
    set_table_properties(width = .25, layout = "autofit")
  ft_table

}

tablegt = function(table, table_name, col_names, num_decimals) {
  table_gt = table %>% gt() %>%
    tab_header(
      title = gt::html(table_name)
    ) %>%
    tab_spanner(label = col_names[2], columns = c(Min, Median, Max)) %>%
    tab_spanner(label = col_names[3], columns = c(`Min\r`, `Median\r`, `Max\r`)) %>%
    cols_label() %>%
    tab_style(
      style = list(
        cell_borders(sides = "all", color = "#F2F2F2"),
        cell_fill(color = "#F2F2F2", alpha = NULL),
        cell_text(align = "center")
      ), locations = cells_body()
    ) %>% tab_style(
      style = list(
        cell_fill(color = "black"),
        cell_text(color = "white", weight = "normal", align = "center")
      ), locations = cells_column_labels()
    ) %>% tab_style(
      style = list(
        cell_fill(color = "black"),
        cell_text(color = "white", weight = "bold")
      ), locations = cells_column_spanners()
    ) %>% tab_style(
      style = list(
        cell_borders(sides = "right", color = "#DCDCDC", weight = "2px")
      ), locations = cells_body(columns = c(1,4))
    ) %>% tab_style(
      style = list(
        cell_text(weight = "bold")
      ), locations = cells_body(columns = c(1))
    ) %>% fmt_number(decimals = num_decimals, use_seps = TRUE, columns = 2:7)

  return(table_gt)
}

# TODO: change to per_reduction

summary_tables = function(table_no, var, suffix, drop_mod = NULL, drop_datasrc = NULL, short_circuit = FALSE, data, config) {

  var_df = clean_data %>% filter(variable == var)

  baseline_2005 = (var_df %>% filter(year == 2005 & model == "EPA-GHGI"))$value
  baseline_2021 = (var_df %>% filter(year == 2021 & model == "EPA-GHGI"))$value

  var_unit = unique(var_df$unit)

  mod = clean_data %>%
    filter(
      variable == var &
        model %in% config$models_leep &
        !model %in% drop_mod &
        !datasrc %in% drop_datasrc &
        year >= 2024 &
        scenario %in% c("IRA", "No IRA"))

  dup_2021 = mod %>%
    select(model,scenario,unit,region,variable) %>%
    distinct() %>%
    mutate(
      year = 2021,
      datasrc = "EPA-GHGI",
      value = baseline_2021
    ) %>%
    relocate_standard_col_order()

  df = rbind(mod, dup_2021)

  year_range <- seq.int(from = 2021, to = 2035)

  interpolation = df %>%
    select(-variable, -datasrc, -region, -unit) %>%
    filter(!is.na(value)) %>%
    complete(nesting(model, scenario),
             year = year_range) %>%
    arrange(model, scenario, year) %>%
    nest(data = c(year, value)) %>%
    mutate(int_pts = map(data,
                         .f = ~approx(x=.$year, y=.$value, xout = .$year) %>% as_tibble() )) %>%
    unnest(cols = c(data, int_pts)) %>%
    mutate(value = y) %>%
    select(-x, -y) %>%
    filter(year >= 2024 & year <= 2035)

  raw = interpolation %>%
    filter(year >= 2024) %>%
    mutate(diff_2005 = baseline_2005 - value,
           per_diff_2005 = (diff_2005 / baseline_2005) * 100) %>%
    mutate(diff_2021 = baseline_2021 - value,
           per_diff_2021 = (diff_2021 / baseline_2021) * 100) %>%
    mutate_if(is.numeric, round, 1) %>%
    mutate(year = as.character(year))

  stats_all = raw %>%
    group_by(year, scenario) %>%
    summarise(
      min_ab = round(min(value),0),
      max_ab = round(max(value),0),
      median_ab = round(median(value),0),
      min_diff_2005 = round(min(per_diff_2005),1),
      max_diff_2005 = round(max(per_diff_2005),1),
      median_diff_2005 = round(median(per_diff_2005),1),
      min_diff_2021 = round(min(per_diff_2021),1),
      max_diff_2021 = round(max(per_diff_2021),1),
      median_diff_2021 = round(median(per_diff_2021),1)
    ) %>%
    ungroup()

  stats_noira = stats_all %>%
    filter(scenario == "No IRA")

  stats_ira = stats_all %>%
    filter(scenario == "IRA")

  if (short_circuit) {
    return(list(
      "IRA" = stats_ira,
      "noIRA" = stats_noira
    ))
  }

  diff_scenarios = raw %>%
    select(model, scenario, year, value) %>%
    pivot_wider(names_from = "scenario", values_from = "value") %>%
    mutate(diff_noira = `No IRA` - `IRA`,
           per_diff_noira = (diff_noira / `No IRA`) * 100) %>%
    mutate_if(is.numeric, round, 1) %>%
    group_by(year) %>%
    summarise(
      median_diff = median(diff_noira),
      min_diff = min(diff_noira),
      max_diff = max(diff_noira),
      median_per = median(per_diff_noira),
      min_per = min(per_diff_noira),
      max_per = max(per_diff_noira)
    )

  colnames = c("Year","Median","Min","Max","Median\r","Min\r","Max\r")

  table1 = cbind(stats_noira %>% select(year, median_ab, min_ab, max_ab),
                 stats_ira %>% select(median_ab, min_ab, max_ab))
  colnames(table1) = colnames

  table2 = diff_scenarios
  colnames(table2) = colnames

  table3 = stats_noira %>%
    select(year, median_diff_2005, min_diff_2005, max_diff_2005, median_diff_2021, min_diff_2021, max_diff_2021)
  colnames(table3) = colnames

  table4 = stats_ira %>%
    select(year, median_diff_2005, min_diff_2005, max_diff_2005, median_diff_2021, min_diff_2021, max_diff_2021)
  colnames(table4) = colnames

  # ft_table1 = tableit(
  #   table = table1,
  #   table_name = as_paragraph(suffix," (",var_unit,")"),
  #   col_names = c("", "No IRA", "IRA")
  # )
  #print(as_paragraph(suffix," (",var_unit,")"))
  gt_table1 = tablegt(
    table = table1,
    table_name = paste0(suffix, " (", var_unit, ")"),
    col_names = c("", "No IRA", "IRA"),
    num_decimals = 0
  )
  gt_table1

  # cols_table2 = as_paragraph(as_chunk(c("",
  #                                       paste("Absolute Difference (",var_unit,")",sep=""),
  #                                       "% Difference")))
  cols_table2 = c("",paste0("Absolute Difference (", var_unit, ")"), "% Difference")
  # ft_table2 = tableit(
  #   table = table2,
  #   table_name = as_paragraph("Difference between No IRA and IRA ",suffix),
  #   col_names = cols_table2)
  gt_table2 = tablegt(
    table = table2,
    table_name = paste0("Difference between No IRA and IRA: ", suffix),
    col_names = cols_table2,
    num_decimals = 1
  )
  gt_table2

  # ft_table3 = tableit(
  #   table = table3,
  #   table_name = as_paragraph("% Difference in ",suffix," from 2005 and 2021 for the No IRA scenario"),
  #   col_names = c("","2005","2021")
  # )
  # ft_table3
  gt_table3 = tablegt(
    table = table3,
    table_name = paste0("Difference in ", suffix, " from 2005 and 2021 for the No IRA scenario"),
    col_names = c("","2005","2021"),
    num_decimals = 1
  )
  gt_table3

  # ft_table4 = tableit(
  #   table = table4,
  #   table_name = as_paragraph("% Difference in ",suffix," from 2005 and 2021 for the IRA scenario"),
  #   col_names = c("","2005","2021")
  # )
  # ft_table4

  gt_table4 = tablegt(
    table = table4,
    table_name = paste0("Difference in ", suffix, " from 2005 and 2021 for the IRA scenario"),
    col_names = c("","2005","2021"),
    num_decimals = 1
  )
  gt_table4

# save_as_html(
#   gt_table1,
#   gt_table2,
#   gt_table3,
#   gt_table4,
#   path = paste("output/final_figures/data/Table",table_no,".html",sep="")
# )

  gt_table1 %>% gtsave(paste0("./output/final_figures/display_tables/Table",table_no,".1.html"))
  gt_table2 %>% gtsave(paste0("./output/final_figures/display_tables/Table",table_no,".2.html"))
  gt_table3 %>% gtsave(paste0("./output/final_figures/display_tables/Table",table_no,".3.html"))
  gt_table4 %>% gtsave(paste0("./output/final_figures/display_tables/Table",table_no,".4.html"))

  gt_table1 %>% gtsave(paste0("./output/final_figures/display_tables/Table",table_no,".1.png"))
  gt_table2 %>% gtsave(paste0("./output/final_figures/display_tables/Table",table_no,".2.png"))
  gt_table3 %>% gtsave(paste0("./output/final_figures/display_tables/Table",table_no,".3.png"))
  gt_table4 %>% gtsave(paste0("./output/final_figures/display_tables/Table",table_no,".4.png"))


return(list(table1, gt_table1,
            table2, gt_table2,
            table3, gt_table3,
            table4, gt_table4))

}


write_sheet = function(data, wb, sheetname) {

  addWorksheet(wb, sheetName = sheetname)
  writeData(wb, sheet = sheetname, data)

}

gt_elec_table = function(raw_data, tab_title, percent = FALSE, footnote_text = "", footnote_column = NA, footnote_row = NA) {
  my_table = raw_data %>% gt() %>%
    tab_header(
      title = gt::html(tab_title)
    ) %>%
    tab_spanner(label = "No IRA", columns = c("2030", "2035")) %>%
    tab_spanner(label = "IRA", columns = c("2030\r", "2035\r")) %>%
    tab_style(
      style = list(
        cell_borders(sides = "all", color = "#F2F2F2"),
        cell_fill(color = "#F2F2F2", alpha = NULL),
        cell_text(align = "center")
      ), locations = cells_body()
    ) %>% tab_style(
      style = list(
        cell_fill(color = "black"),
        cell_text(color = "white", weight = "normal", align = "center")
      ), locations = cells_column_labels()
    ) %>% tab_style(
      style = list(
        cell_fill(color = "black"),
        cell_text(color = "white", weight = "bold")
      ), locations = cells_column_spanners()
    ) %>% tab_style(
      style = list(
        cell_borders(sides = "right", color = "#DCDCDC", weight = "2px"),
        cell_text(weight = "normal", align = "left")
      ), locations = cells_body(columns = c(1))
    ) %>% tab_style(
      style = list(
        cell_text(weight = "normal", color = "black", align = "left"),
        cell_fill(color = "#C9C9C9")
      ), locations = cells_row_groups()
    ) %>%
    sub_missing(columns = 2:6, missing_text = "-")

  if (percent) {
    my_table = my_table %>% fmt_percent(decimals = 0, use_seps = TRUE, columns = 2:6)
  } else {
    my_table = my_table %>% fmt_number(decimals = 0, use_seps = TRUE, columns = 2:6)
  }

  if (footnote_text != "") {
    my_table = my_table %>%
      tab_footnote(
        footnote = footnote_text,
        locations = cells_body(
          columns = footnote_column,
          rows = footnote_row
        )
      )
  }

  return(my_table)
}

indicator_small = function(all_data, summary_data, filter_variable, subtitle = "", top = FALSE, bottom = FALSE) {
  data_small = all_data %>% filter(sector == filter_variable)
  summ_small = summary_data %>% filter(sector == filter_variable)
  p = data_small %>%
    ggplot() +
    geom_point(aes(x = pct_diff, y = scenario, color = scenario, shape = scenario), alpha = 0.8, size = 1) +
    geom_segment(data = summ_small,
                 aes(x = min, xend = max, y = scenario, yend = scenario, color = scenario),
                 alpha = 0.3, linewidth = 3) +
    geom_point(data = summ_small, aes(x = min, y = scenario, color = scenario, shape = scenario), size = 3) +
    geom_point(data = summ_small, aes(x = max, y = scenario, color = scenario, shape = scenario), size = 3) +
    geom_point(data = summ_small, aes(x = median, y = scenario, color = scenario),
               shape = "|", size = 1.5, stroke = 3.5) +
    geom_vline(xintercept = 50, color = "black", linewidth = 0.1, alpha = 0.4) +
    # scale_y_discrete(name = filter_variable) +
    ggtitle(subtitle) +
    scale_color_manual(values = c(subpalettes$`Economy-Wide Emissions`[['No IRA']],
                                  subpalettes$`Economy-Wide Emissions`[['IRA']]),
                       breaks = c("No IRA", "IRA")) +
    scale_shape_manual(values = c("No IRA" = 17, "IRA" = 16),
                       breaks = c("No IRA", "IRA")) +
    facet_grid(year ~ ., switch = "y") +
    theme_emf() +
    theme(axis.ticks = element_blank(), axis.text.y = element_blank(),
          #axis.title.y = element_text(size = 8),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 10, hjust = 0, face = "bold",
                                    margin = margin(0,0,1,0)),
          strip.text.y.left = element_text(size = 6, face = "plain", angle = 0),
          strip.text.y.right = element_text(size = 6, face = "plain", angle = 0),
          axis.text.x = element_text(face = "plain", size = 6),
          axis.title.x = element_text(face = "plain", size = 8),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          #axis.line.x = element_line(color = "#595959", linewidth = 0.25),
          axis.line.x = element_line(color = "white", linewidth = 0.25),
          plot.margin = margin(0,0,5,0))

  if (!(bottom)){
    p = p + theme(axis.title.x = element_blank()) +
      scale_x_continuous(limits = c(0,100), expand = c(0,0), position = "top",
                         name = expression("Percent Reduction from 2005 Levels (%)"))
  }
  if (!(top) & !(bottom)) {
    p = p + theme(axis.text.x = element_blank()) +
      scale_x_continuous(limits = c(0,100), expand = c(0,0), position = "bottom",
                         name = expression("Percent Reduction from 2005 Levels (%)"))
  }
  if (bottom) {
    p = p + scale_x_continuous(limits = c(0,100), expand = c(0,0), position = "bottom",
                               name = expression("Percent Reduction from 2005 Levels (%)"))
  }

  return(p)
}

indicator_smallX = function(all_data, summary_data, filter_variable, subtitle = "", top = FALSE, bottom = FALSE) {
  data_small = all_data %>% filter(sector == filter_variable)
  summ_small = summary_data %>% filter(sector == filter_variable)
  p = data_small %>%
    ggplot() +
    geom_point(aes(x = pct_diff, y = scenario, color = scenario, shape = scenario), alpha = 0.8, size = 1) +
    geom_segment(data = summ_small,
                 aes(x = min, xend = max, y = scenario, yend = scenario, color = scenario),
                 alpha = 0.3, linewidth = 3) +
    geom_point(data = summ_small, aes(x = min, y = scenario, color = scenario, shape = scenario), size = 3) +
    geom_point(data = summ_small, aes(x = max, y = scenario, color = scenario, shape = scenario), size = 3) +
    geom_point(data = summ_small, aes(x = median, y = scenario, color = scenario),
               shape = "|", size = 1.5, stroke = 3.5) +
    geom_vline(xintercept = 50, color = "black", linewidth = 0.1, alpha = 0.4) +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.1, alpha = 0.4) +
    # scale_y_discrete(name = filter_variable) +
    ggtitle(subtitle) +
    scale_color_manual(values = c(subpalettes$`Economy-Wide Emissions`[['No IRA']],
                                  subpalettes$`Economy-Wide Emissions`[['IRA']]),
                       breaks = c("No IRA", "IRA")) +
    scale_shape_manual(values = c("No IRA" = 17, "IRA" = 16),
                       breaks = c("No IRA", "IRA")) +
    facet_grid(year ~ ., switch = "y") +
    theme_emf() +
    theme(axis.ticks = element_blank(), axis.text.y = element_blank(),
          #axis.title.y = element_text(size = 8),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 10, hjust = 0, face = "bold",
                                    margin = margin(0,0,1,0)),
          strip.text.y.left = element_text(size = 6, face = "plain", angle = 0),
          strip.text.y.right = element_text(size = 6, face = "plain", angle = 0),
          axis.text.x = element_text(face = "plain", size = 6),
          axis.title.x = element_text(face = "plain", size = 8),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          #axis.line.x = element_line(color = "#595959", linewidth = 0.25),
          axis.line.x = element_line(color = "white", linewidth = 0.25),
          plot.margin = margin(0,0,5,0),
          legend.position = "none")

  if (!(bottom)){
    p = p + theme(axis.title.x = element_blank()) +
      scale_x_continuous(limits = c(-5,100), expand = c(0,0), position = "top",
                         name = expression("Percent Reduction from No IRA Scenario (%)"))
  }
  if (!(top) & !(bottom)) {
    p = p + theme(axis.text.x = element_blank()) +
      scale_x_continuous(limits = c(-5,100), expand = c(0,0), position = "bottom",
                         name = expression("Percent Reduction from No IRA Scenario (%)"))
  }
  if (bottom) {
    p = p + scale_x_continuous(limits = c(-5,100), expand = c(0,0), position = "bottom",
                               name = expression("Percent Reduction from No IRA Scenario (%)"))
  }

  return(p)
}

filter_sensitivities = function(clean_data) {
  filtered = clean_data  %>%
    mutate(region = "United States") %>%
    mutate(model = case_when(
      datasrc == "ReEDS_compiled.csv" ~ "ReEDS-NRELr",
      TRUE~model)) %>%
    mutate(region = "United States") %>%
    # remove bistline models with incorrect indirect emissions for end-use sectors
    # MARKAL-NETL sensitivities included here, but we exclude from LEEP Report sensitivity analysis: should decide what to do
    filter(!(model %in% c("GCAM-CGS","REGEN-EPRI") &
               scenario %in% c("IRA.Low","IRA.High") &
               variable %in% c("Emissions|CO2|Energy|Demand|Buildings|Total",
                               "Emissions|CO2|Energy|Demand|Industry and Fuel Production|Total",
                               "Emissions|CO2|Energy|Demand|Transportation|Total"))) %>%
    filter(!(model %in% c("MARKAL-NETL") & scenario %in% c("IRA.Low","IRA.High")))

  filtered
}

drop_warn = function(drop) {
  if(drop == "") {
    print("No models have been dropped from this figure.")
  } else {
    paste("The model(s)", paste(drop, collapse = " & "), "have been dropped from this figure.")
  }
}

spg_clean = function(ts_map_ID, drop, histsrc, config, clean_data, figmap_leep_timeseries) { #add in parameters from other functions being called in

  df = data_from_graph(
    "time_series",
    config,
    clean_data,
    figmap_leep_timeseries,
    ts_map_ID,
    "United States"
  ) %>%
    filter(!model %in% drop) %>%
    filter(!datasrc %in% drop) %>%
    mutate(
      alpha = case_when(
        model == "USREP-ReEDS" ~ 1,
        model == "GCAM-PNNL" ~ 1,
        model == "IPM-EPA" ~ 1,
        scenario == "Historic" ~ 1,
        T ~ 1
      )
    ) %>%
    filter(year <= 2021 &
             model == histsrc |
             year >= 2021 & scenario != "Historic") %>%
    pivot_wider(names_from = year, values_from = value) %>%
    mutate(`2021` = case_when(T ~ `2021`[model == histsrc])) %>%
    pivot_longer(cols = starts_with("20"),
                 names_to = "year",
                 values_to = "value") %>%
    filter(!is.na(value)) %>%
    mutate(year = as.numeric(year))

  return(data = df)
}

spg2 = function(df, title, yname, gd, ymin, ymax, ybreaks, yax_format, annotate, historic_coord = c(0,0), preira_coord = c(0,0), ira_coord = c(0,0), config, figmap_leep_timeseries) {

  hist = df %>%
    filter(scenario == "Historic")
  strt = df %>%
    filter(scenario != "Historic") %>%
    group_by(model, scenario) %>%
    filter(year == min(year))
  df = df %>%
    filter(year %in% c(2025, 2030, 2035))

  df = rbind(hist, strt, df)

  subpalettes = create_subpalettes(figmap_leep_timeseries, config)
  subpalettes$`Emissions|CO2|Energy|Demand|Industry`[1] = "black"
  annoclr = subpalettes$`Emissions|CO2|Energy|Demand|Industry`
  histclr = annoclr[1]
  noIRAclr = annoclr[2]
  IRAclr = annoclr[3]

  figure = ggplot(df, aes(year,value, color = scenario, group = interaction(model, scenario))) +
    geom_line(aes(alpha = alpha, linetype = scenario), size = 0.5) +
    scale_subpalette(subpalettes, "Emissions|CO2|Energy|Demand|Industry") +
    theme_emf() +
    scale_linetype_manual(values = c("Historic" = "solid", "IRA" = "solid", "No IRA" = "longdash"))+
    scale_x_continuous(breaks = c(2005, 2021, 2025, 2030, 2035)) +
    scale_y_continuous(limits = c(ymin, ymax), breaks = ybreaks, labels = yax_format) +
    scale_alpha(range = c(1, 1), guide = F) +
    labs(title = title,
         x = element_blank(),
         y = yname) +
    theme(legend.position = gd,
          axis.ticks = element_line(color = "black"),
          axis.ticks.length = unit(-0.15, "cm")) +
    annotate("text", x = historic_coord[1], y = historic_coord[2], label = "Historical", color = histclr, alpha = annotate) +
    annotate("text", x = preira_coord[1], y = preira_coord[2], label = "No IRA", color = noIRAclr, alpha = annotate) +
    annotate("text", x = ira_coord[1], y = ira_coord[2], label = "IRA", color = IRAclr, alpha = annotate)

  return(list(figure = figure, data = df))
}

spg2_2010 = function(df, title, yname, gd, ymin, ymax, ybreaks, yax_format, annotate, historic_coord = c(0,0), preira_coord = c(0,0), ira_coord = c(0,0), config, figmap_leep_timeseries) {

  hist = df %>%
    filter(scenario == "Historic")
  strt = df %>%
    filter(scenario != "Historic") %>%
    group_by(model, scenario) %>%
    filter(year == min(year))
  df = df %>%
    filter(year %in% c(2025, 2030, 2035))

  df = rbind(hist, strt, df)

  subpalettes = create_subpalettes(figmap_leep_timeseries, config)
  subpalettes$`Emissions|CO2|Energy|Demand|Industry`[1] = "black"
  annoclr = subpalettes$`Emissions|CO2|Energy|Demand|Industry`
  histclr = annoclr[1]
  noIRAclr = annoclr[2]
  IRAclr = annoclr[3]

  figure = ggplot(df, aes(year,value, color = scenario, group = interaction(model, scenario))) +
    geom_line(aes(alpha = alpha, linetype = scenario), size = 0.5) +
    scale_subpalette(subpalettes, "Emissions|CO2|Energy|Demand|Industry") +
    theme_emf() +
    scale_linetype_manual(values = c("Historic" = "solid", "IRA" = "solid", "No IRA" = "longdash"))+
    scale_x_continuous(breaks = c(2010, 2021, 2025, 2030, 2035)) +
    scale_y_continuous(limits = c(ymin, ymax), breaks = ybreaks, labels = yax_format) +
    scale_alpha(range = c(1, 1), guide = F) +
    labs(title = title,
         x = element_blank(),
         y = yname) +
    theme(legend.position = gd,
          axis.ticks = element_line(color = "black"),
          axis.ticks.length = unit(-0.15, "cm")) +
    annotate("text", x = historic_coord[1], y = historic_coord[2], label = "Historical", color = histclr, alpha = annotate) +
    annotate("text", x = preira_coord[1], y = preira_coord[2], label = "No IRA", color = noIRAclr, alpha = annotate) +
    annotate("text", x = ira_coord[1], y = ira_coord[2], label = "IRA", color = IRAclr, alpha = annotate)

  return(list(figure = figure, data = df))
}

spg3 = function(df, title, yname, gd, ymin, ymax, ybreaks, yax_format, annotate, historic_coord = c(0,0), preira_coord = c(0,0), ira_coord = c(0,0), config, figmap_leep_timeseries) {

  hist = df %>%
    filter(scenario == "Historic")
  strt = df %>%
    filter(scenario != "Historic") %>%
    group_by(model, scenario) %>%
    filter(year == min(year))
  df = df %>%
    filter(year %in% c(2025, 2030, 2035))

  df = rbind(hist, strt, df)

  subpalettes = create_subpalettes(figmap_leep_timeseries, config)
  subpalettes$`Emissions|CO2|Energy|Demand|Industry`[1] = "black"
  annoclr = subpalettes$`Emissions|CO2|Energy|Demand|Industry`
  histclr = annoclr[1]
  noIRAclr = annoclr[2]
  IRAclr = annoclr[3]

  figure = ggplot(df, aes(year,value, color = scenario, group = interaction(model, scenario))) +
    geom_line(aes(alpha = alpha, linetype = scenario), size = 0.5) +
    scale_subpalette(subpalettes, "Emissions|CO2|Energy|Demand|Industry") +
    theme_emf() +
    scale_linetype_manual(values = c("Historic" = "solid", "IRA" = "solid", "No IRA" = "longdash"))+
    scale_x_continuous(breaks = c(2020, 2025, 2030, 2035)) +
    scale_y_continuous(limits = c(ymin, ymax), breaks = ybreaks, labels = yax_format) +
    scale_alpha(range = c(1, 1), guide = F) +
    labs(title = title,
         x = element_blank(),
         y = yname) +
    theme(legend.position = gd,
          axis.ticks = element_line(color = "black"),
          axis.ticks.length = unit(-0.15, "cm")) +
    annotate("text", x = historic_coord[1], y = historic_coord[2], label = "Historical", color = histclr, alpha = annotate) +
    annotate("text", x = preira_coord[1], y = preira_coord[2], label = "No IRA", color = noIRAclr, alpha = annotate) +
    annotate("text", x = ira_coord[1], y = ira_coord[2], label = "IRA", color = IRAclr, alpha = annotate)

  return(list(figure = figure, data = df))
}

# spg = function(ts_map_ID, histsrc, title, yname, gd, drop, ymin, ymax, ybreaks, yax, config, figmap_leep_timeseries) {
#
#   subpalettes = create_subpalettes(figmap_leep_timeseries, config)
#
#   df = data_from_graph(
#     "time_series",
#     config,
#     clean_data,
#     figmap_leep_timeseries,
#     ts_map_ID,
#     "United States"
#   ) %>%
#     filter(!model %in% drop) %>%
#     filter(!datasrc %in% drop) %>%
#     mutate(
#       alpha = case_when(
#         model == "USREP-ReEDS" ~ 1,
#         model == "GCAM-PNNL" ~ 1,
#         model == "IPM-EPA" ~ 1,
#         scenario == "Historic" ~ 1,
#         T ~ 1
#       )
#     ) %>%
#     filter(year <= 2021 &
#              model == histsrc |
#              year >= 2021 & scenario != "Historic") %>%
#     pivot_wider(names_from = year, values_from = value) %>%
#     mutate(`2021` = case_when(T ~ `2021`[model == histsrc])) %>%
#     pivot_longer(cols = starts_with("20"),
#                  names_to = "year",
#                  values_to = "value") %>%
#     filter(!is.na(value)) %>%
#     mutate(year = as.numeric(year))
#
#   figure = ggplot(df, aes(year,value, color = scenario, group = interaction(model, scenario))) + geom_line(aes(alpha = alpha), size = 0.5) +
#     scale_subpalette(subpalettes, "Emissions|CO2|Energy|Demand|Industry") +
#     theme_emf() +
#     scale_x_continuous(breaks = c(2005, 2021, 2025, 2030, 2035)) +
#     scale_y_continuous(limits = c(ymin, ymax), breaks = ybreaks, labels = yax) +
#     scale_alpha(range = c(1, 1), guide = F) +
#     labs(title = title,
#          x = element_blank(),
#          y = yname) +
#     theme(legend.position = gd,
#           axis.text.x = element_text(angle = 45, hjust = 1))
#
#   return(figure)
# }



dotted = function(df, spg, metric, ymin, ymax, config, figmap_leep_timeseries, ddge = 0.2) {

  subpalettes = create_subpalettes(figmap_leep_timeseries, config)

  df_30 = df %>%
    filter(year %in% c(2030))
  df_stat_30 = df_30 %>%
    group_by(scenario,year,variable_rename) %>%
    summarise(mean = mean(value),
              median = median(value))

  df_35 = df %>%
    filter(year %in% c(2035))
  df_stat_35 = df_35 %>%
    group_by(scenario,year,variable_rename) %>%
    summarise(mean = mean(value),
              median = median(value))

  mean_diff_30 = df_stat_30[df_stat_30$scenario == "No IRA","mean"]$mean - df_stat_30[df_stat_30$scenario == "IRA","mean"]$mean
  mean_diff_35 = df_stat_35[df_stat_35$scenario == "No IRA","mean"]$mean - df_stat_35[df_stat_35$scenario == "IRA","mean"]$mean
  median_diff_30 = df_stat_30[df_stat_30$scenario == "No IRA","median"]$median - df_stat_30[df_stat_30$scenario == "IRA","median"]$median
  median_diff_35 = df_stat_35[df_stat_35$scenario == "No IRA","median"]$median - df_stat_35[df_stat_35$scenario == "IRA","median"]$median
  # stick em all in a data frame
  stats = data.frame("year" = c(2030,2035,2030,2035), "metric" = c("mean","mean","median","median"),
                     "difference" = c(mean_diff_30, mean_diff_35, median_diff_30, median_diff_35))

  # a few of the ggplot lines of code vary based on the year or on the metric chosen, so conditionally choose the right one here
  if (metric == "mean") {
    segment_code_30 = geom_segment(data = df_stat_30, aes(x = year - 0.1, xend = year + 0.1, y = mean, yend = mean, color = scenario),
                                   size = 1)
    segment_code_35 = geom_segment(data = df_stat_35, aes(x = year - 0.1, xend = year + 0.1, y = mean, yend = mean, color = scenario),
                                   size = 1)
  } else if (metric == "median") {
    segment_code_30 = geom_segment(data = df_stat_30, aes(x = year - 0.1, xend = year + 0.1, y = median, yend = median, color = scenario),
                                   size = 1)
    segment_code_35 = geom_segment(data = df_stat_35, aes(x = year - 0.1, xend = year + 0.1, y = median, yend = median, color = scenario),
                                   size = 1)
  }



  dots_30 = ggplot() +
    geom_point(data = df_30, aes(x = year, y = value, color = scenario, alpha = alpha, shape = scenario),size = 1.5, position = position_dodge(width = ddge)) +
    segment_code_30 +
    scale_x_continuous(breaks = c(2030), labels = c("2030"), limits = c(2029.9, 2030.1)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    theme_emf() +
    scale_subpalette(subpalettes, "Emissions|CO2|Energy|Demand|Industry") +
    scale_shape_manual(values = c("IRA" = 1, "No IRA" = 2)) +
    theme(panel.grid = element_blank(), plot.title = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(), axis.title.x = element_blank(),
          axis.ticks = element_blank(), panel.border = element_blank(),
          legend.position = "none", plot.margin = margin(0,1,0,1))+
    scale_alpha(range = c(1, 1), guide = F)

  # dot plot for 2035
  dots_35 = ggplot() +
    geom_point(data = df_35, aes(x = year, y = value, color = scenario, alpha = alpha, shape = scenario), size = 1.5, position = position_dodge(width = ddge)) +
    segment_code_35 +
    scale_x_continuous(breaks = c(2035), labels = c("2035"), limits = c(2034.9, 2035.1)) +
    scale_y_continuous(limits = c(ymin,ymax)) +
    theme_emf() +
    scale_subpalette(subpalettes, "Emissions|CO2|Energy|Demand|Industry") +
    scale_shape_manual(values = c("IRA" = 1, "No IRA" = 2)) +
    theme(panel.grid = element_blank(), plot.title = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(), axis.title.x = element_blank(),
          axis.ticks = element_blank(), panel.border = element_blank(),
          legend.position = "none", plot.margin = margin(0,1,0,1))+
    scale_alpha(range = c(1, 1), guide = F)

  figure = spg + dots_30 + dots_35 +
    plot_layout(widths = c(10,1,1)) +
    theme(plot.margin = margin(0,0,0,0))

  return(figure)
}



#This is an outdated version of the html call that creates scenario-consistent outputs
# html = function(df, title) {
#   base = df %>%
#     filter(year == 2005)
#
#   baseline = base$value[1]
#
#   base <- as.data.frame(base) %>%
#     select(Year = year,
#            `Absolute Level (Mt CO2)` = value) %>%
#     mutate(Scenario = "Baseline",
#            `% Reductions from 2005` = NA,
#            `% Reductions from No IRA` = NA,
#            `Reductions from No IRA (Mt CO2)` = NA)  %>%
#     select(Year, Scenario, `Absolute Level (Mt CO2)`, `% Reductions from 2005`, `% Reductions from No IRA`, `Reductions from No IRA (Mt CO2)`)
#
#   NoIRA <- df %>%
#     filter(year == 2030 | year == 2035) %>%
#     filter(scenario == "No IRA") %>%
#     group_by(year) %>%
#     summarize(`No IRA Median` = median(value),
#               `No IRA Min` = min(value),
#               `No IRA Max` =  max(value))%>%
#     pivot_longer(cols = !year, names_to = "Scenario", values_to = "Absolute Level (Mt CO2)") %>%
#     mutate(`2005_perc_red` = ((baseline-`Absolute Level (Mt CO2)`)/baseline) * 100) %>%
#     mutate(
#       Year = year,
#       `% Reductions from 2005` = `2005_perc_red`,
#       `% Reductions from No IRA` = NA,
#       `Reductions from No IRA (Mt CO2)` = NA
#     ) %>%
#     select(!year & !`2005_perc_red`)
#
#   IRA <- df %>%
#     filter(year == 2030 | year == 2035) %>%
#     filter(scenario == "IRA") %>%
#     select(!scenario) %>%
#     group_by(year) %>%
#     summarize(
#       `IRA Max` = max(value),
#       `IRA Min` = min(value),
#       `IRA Median` = median(value)) %>%
#     pivot_longer(cols = !year, names_to = "Scenario", values_to = "Absolute Level (Mt CO2)") %>%
#     mutate(
#       `% Reductions from 2005` = ((baseline-`Absolute Level (Mt CO2)`)/baseline) * 100
#     )%>%
#     mutate(
#       `% Reductions from No IRA` = case_when(
#         year == 2030 & Scenario == "IRA Max" ~
#           ((NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Max"]-`Absolute Level (Mt CO2)`)/NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Max"]) * 100,
#
#         year == 2030 & Scenario == "IRA Median" ~
#           ((NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Median"]-`Absolute Level (Mt CO2)`)/NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Median"]) * 100,
#
#         year == 2030 & Scenario == "IRA Min" ~
#           ((NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Min"]-`Absolute Level (Mt CO2)`)/NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Min"]) * 100,
#
#         year == 2035 & Scenario == "IRA Max" ~
#           ((NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Max"]-`Absolute Level (Mt CO2)`)/NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Max"]) * 100,
#
#         year == 2035 & Scenario == "IRA Median" ~
#           ((NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Median"]-`Absolute Level (Mt CO2)`)/NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Median"]) * 100,
#
#         year == 2035 & Scenario == "IRA Min" ~
#           ((NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Min"]-`Absolute Level (Mt CO2)`)/NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Min"]) * 100
#       ),
#
#       `Reductions from No IRA (Mt CO2)` = case_when(
#         year == 2030 & Scenario == "IRA Max" ~
#           (NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Max"]-`Absolute Level (Mt CO2)`),
#
#         year == 2030 & Scenario == "IRA Median" ~
#           (NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Median"]-`Absolute Level (Mt CO2)`),
#
#         year == 2030 & Scenario == "IRA Min" ~
#           (NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Min"]-`Absolute Level (Mt CO2)`),
#
#         year == 2035 & Scenario == "IRA Max" ~
#           (NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Max"]-`Absolute Level (Mt CO2)`),
#
#         year == 2035 & Scenario == "IRA Median" ~
#           (NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Median"]-`Absolute Level (Mt CO2)`),
#
#         year == 2035 & Scenario == "IRA Min" ~
#           (NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Min"]-`Absolute Level (Mt CO2)`)
#       )
#     ) %>%
#     rename(Year = year)
#
#   IRA = rbind(NoIRA, IRA)
#
#   IRA <- IRA %>%
#     arrange(desc(IRA)) %>%
#     arrange(Year)
#
#   IRA = rbind(base, IRA)
#
#   IRA$`Absolute Level (Mt CO2)` = round(IRA$`Absolute Level (Mt CO2)`, 3)
#   IRA$`% Reductions from 2005` = round(IRA$`% Reductions from 2005`, 3)
#   IRA$`% Reductions from No IRA` = round(IRA$`% Reductions from No IRA`, 3)
#   IRA$`Reductions from No IRA (Mt CO2)` = round(IRA$`Reductions from No IRA (Mt CO2)`,3)
#
#   tab = IRA %>%
#     tableHTML(caption = paste(title), rownames = F)
#
#   write.csv(IRA, paste("./output/final_figures/stats_tables/", fig_no, "SummaryTable ",title,".csv",sep=""))
#
#   return(tab)
# }

#Updated version of the html function that calculates % differences from No IRA a bit diffrently
html = function(df, title) {
  base = df %>%
    filter(year == 2005)

  baseline = base$value[1]

  base <- as.data.frame(base) %>%
    select(Year = year,
           `Absolute Level (Mt CO2)` = value) %>%
    mutate(Scenario = "Baseline",
           `% Reductions from 2005` = NA,
           `% Reductions from No IRA` = NA,
           `Reductions from No IRA (Mt CO2)` = NA)  %>%
    select(Year, Scenario, `Absolute Level (Mt CO2)`, `% Reductions from 2005`, `% Reductions from No IRA`, `Reductions from No IRA (Mt CO2)`)

  NoIRA <- df %>%
    filter(year == 2030 | year == 2035) %>%
    filter(scenario == "No IRA") %>%
    group_by(year) %>%
    summarize(`No IRA Median` = median(value),
              `No IRA Min` = min(value),
              `No IRA Max` =  max(value))%>%
    pivot_longer(cols = !year, names_to = "Scenario", values_to = "Absolute Level (Mt CO2)") %>%
    mutate(`2005_perc_red` = ((baseline-`Absolute Level (Mt CO2)`)/baseline) * 100) %>%
    mutate(
      Year = year,
      `% Reductions from 2005` = `2005_perc_red`,
      `% Reductions from No IRA` = NA,
      `Reductions from No IRA (Mt CO2)` = NA
    ) %>%
    select(!year & !`2005_perc_red`)

  IRA <- df %>%
    filter(year == 2030 | year == 2035) %>%
    filter(scenario == "IRA") %>%
    select(!scenario) %>%
    group_by(year) %>%
    summarize(
      `IRA Max` = max(value),
      `IRA Min` = min(value),
      `IRA Median` = median(value)) %>%
    pivot_longer(cols = !year, names_to = "Scenario", values_to = "Absolute Level (Mt CO2)") %>%
    mutate(
      `% Reductions from 2005` = ((baseline-`Absolute Level (Mt CO2)`)/baseline) * 100
    )

  pd_30 = df %>%
    filter(year == 2030) %>%
    select(year, model, value, scenario) %>%
    pivot_wider(names_from = scenario, values_from = value) %>%
    mutate(pd = ((`No IRA` - IRA) / `No IRA`)*100)

  pd_35 = df %>%
    filter(year == 2035) %>%
    select(year, model, value, scenario) %>%
    pivot_wider(names_from = scenario, values_from = value) %>%
    mutate(pd = ((`No IRA` - IRA) / `No IRA`)*100)

  IRA = IRA %>%
    mutate(
      `% Reductions from No IRA` = case_when(
        year == 2030 & Scenario == "IRA Max" ~ max(pd_30$pd),

        year == 2030 & Scenario == "IRA Median" ~ median(pd_30$pd),

        year == 2030 & Scenario == "IRA Min" ~ min(pd_30$pd),

        year == 2035 & Scenario == "IRA Max" ~max(pd_35$pd),

        year == 2035 & Scenario == "IRA Median" ~ median(pd_35$pd),

        year == 2035 & Scenario == "IRA Min" ~ min(pd_35$pd)
      ),

      `Reductions from No IRA (Mt CO2)` = case_when(
        year == 2030 & Scenario == "IRA Max" ~
          (NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Max"]-`Absolute Level (Mt CO2)`),

        year == 2030 & Scenario == "IRA Median" ~
          (NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Median"]-`Absolute Level (Mt CO2)`),

        year == 2030 & Scenario == "IRA Min" ~
          (NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Min"]-`Absolute Level (Mt CO2)`),

        year == 2035 & Scenario == "IRA Max" ~
          (NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Max"]-`Absolute Level (Mt CO2)`),

        year == 2035 & Scenario == "IRA Median" ~
          (NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Median"]-`Absolute Level (Mt CO2)`),

        year == 2035 & Scenario == "IRA Min" ~
          (NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Min"]-`Absolute Level (Mt CO2)`)
      )
    ) %>%
    rename(Year = year)

  IRA = rbind(NoIRA, IRA)

  IRA <- IRA %>%
    arrange(desc(IRA)) %>%
    arrange(Year)

  IRA = rbind(base, IRA)

  IRA$`Absolute Level (Mt CO2)` = round(IRA$`Absolute Level (Mt CO2)`, 2)
  IRA$`% Reductions from 2005` = round(IRA$`% Reductions from 2005`, 2)
  IRA$`% Reductions from No IRA` = round(IRA$`% Reductions from No IRA`, 2)
  IRA$`Reductions from No IRA (Mt CO2)` = round(IRA$`Reductions from No IRA (Mt CO2)`, 2)

  tab = IRA %>%
    tableHTML(caption = paste(title), rownames = F)

  write.csv(IRA, paste("./output/final_figures/stats_tables/", fig_no, "SummaryTable ",title,".csv",sep=""))

  return(tab)
}



#Function to create standard percent difference figs
pd = function(ts_map_ID, title, yname, gd, drop) {
  #Take data from leep_timeseries map based on ID.
  df = data_from_graph(
    "time_series",
    config,
    clean_data,
    figmap_leep_timeseries,
    ts_map_ID,
    "United States"
  ) %>%
    filter(!scenario == "No IRA") %>%
    filter(year > 2021) %>%
    filter(!model %in% drop) %>%
    group_by(model, year, scenario, variable_rename) %>%
    summarize(value= sum(value)) %>%
    filter(year == 2025 | year == 2030 | year == 2035)

  medians = df %>%
    filter(year %in% c(2030, 2035)) %>%
    filter(value != 0) %>%
    group_by(year) %>%
    summarize(median = median(value))

  #Set 2021 value to zero for all models
  hist <- df %>%
    group_by(model) %>%
    slice(1) %>%
    mutate(scenario = "Historic",
           value = 0,
           year = 2021)

  #Stitch it back together with main data
  df <- rbind(df, hist)

  test2 = c("EPS-EI" = "#DD614A",
            "GCAM-CGS" = "#6a959b",
            "GCAM-PNNL" = "#2A546C",
            "Haiku-RFF" = "#AE7F2C",
            "IPM-EPA" = "#8B507E",
            "IPM-NRDC" = "#488C49",
            "MARKAL-NETL" = "#af499b",
            "NEMS-EIA" = NA,
            "NEMS-OP" = "#11897d",
            "NEMS-RHG" = "#25266b",
            "ReEDS-NREL" = "#b72467",
            "REGEN-EPRI" = "#f15a22",
            "RIO-REPEAT" = "#524fa1",
            "USREP-ReEDS" = "#0083ca")

  #Plot standard figure
  figure = ggplot() +
    geom_line(data = df, aes(
      x = year,
      y = value,
      group = model,
      color = model,
      linetype = model),
      size = 0.75) +
    geom_point(aes(x = 2021, y = 0), color = "black") +
    geom_point(
      data = medians,
      aes(x = year, y = median),
      color = "black",
      shape = 16,
      size = 2
    ) +
    scale_linetype_manual(values = c("EPS-EI" = "solid",
                                     "GCAM-CGS" = "solid",
                                     "GCAM-PNNL" = "solid",
                                     "Haiku-RFF" = "solid",
                                     "IPM-EPA" = "solid",
                                     "IPM-NRDC" = "solid",
                                     "MARKAL-NETL" = "solid",
                                     "NEMS-EIA" = "dotdash",
                                     "NEMS-OP" = "twodash",
                                     "NEMS-RHG" = "solid",
                                     "ReEDS-NREL" = "longdash",
                                     "REGEN-EPRI" = "dashed",
                                     "RIO-REPEAT" = "dotted",
                                     "USREP-ReEDS" = "solid"),

                          breaks = c("EPS-EI",
                                     "GCAM-CGS",
                                     "GCAM-PNNL",
                                     "Haiku-RFF",
                                     "IPM-EPA",
                                     "IPM-NRDC",
                                     "MARKAL-NETL",
                                     "NEMS-EIA",
                                     "NEMS-OP",
                                     "NEMS-RHG",
                                     "ReEDS-NREL",
                                     "REGEN-EPRI",
                                     "RIO-REPEAT",
                                     "USREP-ReEDS"))+
    scale_color_manual(values = test2, breaks = c("EPS-EI",
                                                  "GCAM-CGS",
                                                  "GCAM-PNNL",
                                                  "Haiku-RFF",
                                                  "IPM-EPA",
                                                  "IPM-NRDC",
                                                  "MARKAL-NETL",
                                                  "NEMS-EIA",
                                                  "NEMS-OP",
                                                  "NEMS-RHG",
                                                  "ReEDS-NREL",
                                                  "REGEN-EPRI",
                                                  "RIO-REPEAT",
                                                  "USREP-ReEDS")) +
    labs(title = title,
         x = "",
         y = yname) +
    theme_emf() +
    theme( axis.ticks = element_line(color = "black"),
           axis.ticks.length = unit(-0.15, "cm"),
           axis.text.x = element_text(angle = 45, hjust = 1),
           panel.spacing.x = unit(4, "mm"),
           legend.position = gd
    ) +
    scale_x_continuous(breaks = c(2021, 2025, 2030, 2035))
  return(list(figure = figure,
              pd_df = df))
}

ad = function(diff_ID, title, metric, gd, drop) {

  if (metric == "Generation") {
    clean_data = clean_data %>%
      mutate(value = case_when(unit == "Quads" ~ value * 293.07, TRUE ~ value),
             unit = case_when(unit == "Quads" ~ "TWh", TRUE ~ unit))
  }

  test2 = c("EPS-EI" = "#DD614A",
            "GCAM-CGS" = "#6a959b",
            "GCAM-PNNL" = "#2A546C",
            "Haiku-RFF" = "#AE7F2C",
            "IPM-EPA" = "#8B507E",
            "IPM-NRDC" = "#488C49",
            "MARKAL-NETL" = "#af499b",
            "NEMS-EIA" = NA,
            "NEMS-OP" = "#11897d",
            "NEMS-RHG" = "#25266b",
            "ReEDS-NREL" = "#b72467",
            "REGEN-EPRI" = "#f15a22",
            "RIO-REPEAT" = "#524fa1",
            "USREP-ReEDS" = "#0083ca")

  df = data_from_graph("diff_bar",
                       config,
                       clean_data,
                       figmap_leep_diffbar,
                       diff_ID,
                       "United States") %>%
    filter(!model %in% drop)%>%
    filter(year > 2021) %>%
    group_by(model, year, scenario, variable_rename) %>%
    summarize(diff = sum(diff))%>%
    filter(year == 2025 | year == 2030 | year == 2035)

  medians = df %>%
    filter(year %in% c(2030, 2035)) %>%
    group_by(year) %>%
    summarize(median = median(diff))

  hist <- df %>%
    group_by(model) %>%
    slice(1) %>%
    mutate(scenario = "Historic",
           diff = 0,
           year = 2021)

  df <- rbind(df, hist)

  if (metric == "Generation") {
    figure = ggplot() +
      geom_line(data = df,
                aes(
                  x = year,
                  y = diff,
                  group = model,
                  color = model,
                  linetype = model
                ),
                size = 0.75) +
      geom_point(aes(x = 2021, y = 0), color = "black") +
      geom_point(
        data = medians,
        aes(x = year, y = median),
        color = "black",
        shape = 16,
        size = 2
      ) +
      scale_color_manual(values = test2, breaks = c("EPS-EI",
                                                    "GCAM-CGS",
                                                    "GCAM-PNNL",
                                                    "Haiku-RFF",
                                                    "IPM-EPA",
                                                    "IPM-NRDC",
                                                    "MARKAL-NETL",
                                                    "NEMS-EIA",
                                                    "NEMS-OP",
                                                    "NEMS-RHG",
                                                    "ReEDS-NREL",
                                                    "REGEN-EPRI",
                                                    "RIO-REPEAT",
                                                    "USREP-ReEDS")) +
      scale_linetype_manual(values = c("EPS-EI" = "solid",
                                       "GCAM-CGS" = "solid",
                                       "GCAM-PNNL" = "solid",
                                       "Haiku-RFF" = "solid",
                                       "IPM-EPA" = "solid",
                                       "IPM-NRDC" = "solid",
                                       "MARKAL-NETL" = "solid",
                                       "NEMS-EIA" = "dotdash",
                                       "NEMS-OP" = "twodash",
                                       "NEMS-RHG" = "solid",
                                       "ReEDS-NREL" = "longdash",
                                       "REGEN-EPRI" = "dashed",
                                       "RIO-REPEAT" = "dotted",
                                       "USREP-ReEDS" = "solid"),

                            breaks = c("EPS-EI",
                                       "GCAM-CGS",
                                       "GCAM-PNNL",
                                       "Haiku-RFF",
                                       "IPM-EPA",
                                       "IPM-NRDC",
                                       "MARKAL-NETL",
                                       "NEMS-EIA",
                                       "NEMS-OP",
                                       "NEMS-RHG",
                                       "ReEDS-NREL",
                                       "REGEN-EPRI",
                                       "RIO-REPEAT",
                                       "USREP-ReEDS"))+
      labs(title = title,
           x = "",
           y = expression(paste("Absoltue Difference (TWh)"))) +
      theme_emf() +
      theme( axis.ticks = element_line(color = "black"),
             axis.ticks.length = unit(-0.15, "cm"),
             axis.text.x = element_text(angle = 45, hjust = 1),
             panel.spacing.x = unit(4, "mm"),
             legend.position = gd
      ) +
      scale_x_continuous(breaks = c(2021, 2025, 2030, 2035)) +
      scale_y_continuous(labels = scales::comma)
  } else {
    figure = ggplot() +
      geom_line(data = df,
                aes(
                  x = year,
                  y = diff,
                  group = model,
                  color = model,
                  linetype = model
                ),
                size = 0.75) +
      geom_point(aes(x = 2021, y = 0), color = "black") +
      geom_point(
        data = medians,
        aes(x = year, y = median),
        color = "black",
        shape = 16,
        size = 2
      ) +
      scale_subpalette(subpalettes, "Emissions|CO2|Percent difference from No IRA") +
      scale_linetype_manual(values = c("EPS-EI" = "solid",
                                       "GCAM-CGS" = "solid",
                                       "GCAM-PNNL" = "solid",
                                       "Haiku-RFF" = "solid",
                                       "IPM-EPA" = "solid",
                                       "IPM-NRDC" = "solid",
                                       "MARKAL-NETL" = "solid",
                                       "NEMS-EIA" = "dotdash",
                                       "NEMS-OP" = "twodash",
                                       "NEMS-RHG" = "solid",
                                       "ReEDS-NREL" = "longdash",
                                       "REGEN-EPRI" = "dashed",
                                       "RIO-REPEAT" = "dotted",
                                       "USREP-ReEDS" = "solid"),

                            breaks = c("EPS-EI",
                                       "GCAM-CGS",
                                       "GCAM-PNNL",
                                       "Haiku-RFF",
                                       "IPM-EPA",
                                       "IPM-NRDC",
                                       "MARKAL-NETL",
                                       "NEMS-EIA",
                                       "NEMS-OP",
                                       "NEMS-RHG",
                                       "ReEDS-NREL",
                                       "REGEN-EPRI",
                                       "RIO-REPEAT",
                                       "USREP-ReEDS"))+
      labs(title = title,
           x = "",
           y = expression(paste("Absolute Difference (Mt C", O[2], "/yr)"))) +
      theme_emf() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing.x = unit(4, "mm"),
        legend.position = gd
      ) +
      scale_x_continuous(breaks = c(2021, 2025, 2030, 2035))+
      scale_y_continuous(labels = scales::comma)
  }
  return(list(figure = figure,
              ad_df = df))
}

delta = function(pd_map_ID, drop, config, clean_data, figmap_leep_timeseries) {

  subpalettes = create_subpalettes(figmap_leep_timeseries, config)


  df = data_from_graph(
    "time_series",
    config,
    clean_data,
    figmap_leep_timeseries,
    pd_map_ID,
    "United States"
  ) %>%
    filter(!scenario == "No IRA") %>%
    filter(year > 2021) %>%
    filter(!model %in% drop) %>%
    group_by(model, year, scenario, variable_rename) %>%
    summarize(value= sum(value))

  medians = df %>%
    group_by(year) %>%
    summarize(median = median(value))
  med_hist = medians %>%
    filter(year == 2030) %>%
    mutate(year = 2021) %>%
    mutate(median = 0)

  medians = rbind(medians, med_hist)

  medians = medians %>%
    filter(year %in% c(2021, 2025, 2030, 2035))

  #Set 2021 value to zero for all models
  hist <- df %>%
    group_by(model) %>%
    slice(1) %>%
    mutate(scenario = "IRA",
           value = 0,
           year = 2021)

  #Stitch it back together with main data
  df <- rbind(df, hist)

  df = df %>%
    filter(year %in% c(2021, 2025, 2030, 2035))

  figure = ggplot() +
    geom_hline(aes(yintercept=0), color = "#9E9E9E", size = 0.75) +
    geom_line(data = df, aes(
      x = year,
      y = value,
      group = model,
      color = scenario,
      alpha = 1),
      size = 0.75) +
    #    geom_point(aes(x = 2021, y = 0), color = "black") +
    geom_line(
      data = medians,
      aes(x = year, y = median),
      color = "black", size = 0.75
    ) +
    scale_subpalette(subpalettes, "Final Energy")+ #Standard subpallete
    labs(title = "",
         x = "",
         y = expression("% Difference from No IRA")) +
    theme_emf() +
    theme(
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(-0.15, "cm"),
      panel.spacing.x = unit(4, "mm"),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks = c(2021, 2025, 2030, 2035)) +
    scale_y_continuous(name = NULL, sec.axis = sec_axis(~., name = expression("% Difference from No IRA"))) +
    guides(y = "none")
  return(list(figure = figure, medians = medians, df = df))
}




four_corners = function(title, ts_map_ID, pd_map_ID, ad_map_ID, drop, histsrc, metric, unit, fig_no, ymin, ymax, brk) {
  if (metric == "Generation") {
    clean_data = clean_data %>%
      mutate(value = case_when(unit == "Quads" ~ value * 293.07, TRUE ~ value),
             unit = case_when(unit == "Quads" ~ "TWh", TRUE ~ unit))
  }

  test2 = c("EPS-EI" = "#DD614A",
            "GCAM-CGS" = "#6a959b",
            "GCAM-PNNL" = "#2A546C",
            "Haiku-RFF" = "#AE7F2C",
            "IPM-EPA" = "#8B507E",
            "IPM-NRDC" = "#488C49",
            "MARKAL-NETL" = "#af499b",
            "NEMS-EIA" = NA,
            "NEMS-OP" = "#11897d",
            "NEMS-RHG" = "#25266b",
            "ReEDS-NREL" = "#b72467",
            "REGEN-EPRI" = "#f15a22",
            "RIO-REPEAT" = "#524fa1",
            "USREP-ReEDS" = "#0083ca")

  #Time Series Dataframe
  ts_df = data_from_graph(
    "time_series",
    config,
    clean_data,
    figmap_leep_timeseries,
    ts_map_ID,
    "United States"
  ) %>%
    filter(!model %in% drop) %>%
    filter(!datasrc %in% drop)%>%
    filter(year >= 2021) %>%
    filter(year <= 2021 &
             model == histsrc |
             year > 2021 & scenario != "Historic") %>%
    group_by(model, scenario, year, variable_rename) %>%
    summarize(value = sum(value)) %>%
    pivot_wider(names_from = year, values_from = value)

  ts_df = ts_df %>%
    mutate(`2021` = case_when(T ~ ts_df$`2021`[ts_df$model == histsrc])) %>%
    pivot_longer(cols = starts_with("20"),
                 names_to = "year",
                 values_to = "value") %>%
    filter(!is.na(value)) %>%
    mutate(year = as.numeric(year)) %>%
    mutate(figure_num = fig_no)%>%
    filter(year == 2021 | year == 2025 | year == 2030 | year == 2035)

  noIRAmedians = ts_df[ts_df$scenario == "No IRA",] %>%
    filter(year == 2030 | year == 2035) %>%
    group_by(year) %>%
    summarize(median = median(value))

  IRAmedians = ts_df[ts_df$scenario == "IRA",] %>%
    filter(year == 2030 | year == 2035) %>%
    group_by(year) %>%
    summarize(median = median(value))


  if (metric == "Generation" | metric == "Primary Energy") {
    NoIRAfigure = ggplot(ts_df[ts_df$scenario == "No IRA", ], aes(year, value, color = model)) +
      geom_line(aes(linetype = model),size = 0.75) +
      scale_color_manual(values = test2, breaks = c("EPS-EI",
                                                    "GCAM-CGS",
                                                    "GCAM-PNNL",
                                                    "Haiku-RFF",
                                                    "IPM-EPA",
                                                    "IPM-NRDC",
                                                    "MARKAL-NETL",
                                                    "NEMS-EIA",
                                                    "NEMS-OP",
                                                    "NEMS-RHG",
                                                    "ReEDS-NREL",
                                                    "REGEN-EPRI",
                                                    "RIO-REPEAT",
                                                    "USREP-ReEDS")) +
      theme_emf() +
      scale_x_continuous(breaks = c(2021, 2025, 2030, 2035)) +
      scale_y_continuous(limits = c(ymin, ymax), breaks = brk, labels = scales::comma) +
      scale_linetype_manual(values = c("EPS-EI" = "solid",
                                       "GCAM-CGS" = "solid",
                                       "GCAM-PNNL" = "solid",
                                       "Haiku-RFF" = "solid",
                                       "IPM-EPA" = "solid",
                                       "IPM-NRDC" = "solid",
                                       "MARKAL-NETL" = "solid",
                                       "NEMS-EIA" = "dotdash",
                                       "NEMS-OP" = "twodash",
                                       "NEMS-RHG" = "solid",
                                       "ReEDS-NREL" = "longdash",
                                       "REGEN-EPRI" = "dashed",
                                       "RIO-REPEAT" = "dotted",
                                       "USREP-ReEDS" = "solid"),

                            breaks = c("EPS-EI",
                                       "GCAM-CGS",
                                       "GCAM-PNNL",
                                       "Haiku-RFF",
                                       "IPM-EPA",
                                       "IPM-NRDC",
                                       "MARKAL-NETL",
                                       "NEMS-EIA",
                                       "NEMS-OP",
                                       "NEMS-RHG",
                                       "ReEDS-NREL",
                                       "REGEN-EPRI",
                                       "RIO-REPEAT",
                                       "USREP-ReEDS"))+
      labs(title = "No IRA",
           y = expression(paste("Generation (TWh)")),
           x = element_blank()) +
      theme(  axis.ticks = element_line(color = "black"),
              axis.ticks.length = unit(-0.15, "cm"),
              legend.position = "right",
              plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_point(aes(x = 2021, y = ts_df$value[ts_df$year == 2021][1]), color = "black") +
      geom_point(
        data = noIRAmedians,
        aes(x = year, y = median),
        color = "black",
        shape = 16,
        size = 2
      )

    IRAfigure = ggplot(ts_df[ts_df$scenario == "IRA", ], aes(year, value, color = model)) +
      geom_line(aes(linetype = model),size = 0.75) +
      scale_color_manual(values = test2, breaks = c("EPS-EI",
                                                    "GCAM-CGS",
                                                    "GCAM-PNNL",
                                                    "Haiku-RFF",
                                                    "IPM-EPA",
                                                    "IPM-NRDC",
                                                    "MARKAL-NETL",
                                                    "NEMS-EIA",
                                                    "NEMS-OP",
                                                    "NEMS-RHG",
                                                    "ReEDS-NREL",
                                                    "REGEN-EPRI",
                                                    "RIO-REPEAT",
                                                    "USREP-ReEDS")) +
      theme_emf() +
      scale_x_continuous(breaks = c(2021, 2025, 2030, 2035)) +
      scale_y_continuous(limits = c(ymin, ymax), breaks = brk, labels = scales::comma) +
      labs(title = "IRA",
           y = expression(paste("Generation (TWh)")),
           x = element_blank()) +
      scale_linetype_manual(values = c("EPS-EI" = "solid",
                                       "GCAM-CGS" = "solid",
                                       "GCAM-PNNL" = "solid",
                                       "Haiku-RFF" = "solid",
                                       "IPM-EPA" = "solid",
                                       "IPM-NRDC" = "solid",
                                       "MARKAL-NETL" = "solid",
                                       "NEMS-EIA" = "dotdash",
                                       "NEMS-OP" = "twodash",
                                       "NEMS-RHG" = "solid",
                                       "ReEDS-NREL" = "longdash",
                                       "REGEN-EPRI" = "dashed",
                                       "RIO-REPEAT" = "dotted",
                                       "USREP-ReEDS" = "solid"),

                            breaks = c("EPS-EI",
                                       "GCAM-CGS",
                                       "GCAM-PNNL",
                                       "Haiku-RFF",
                                       "IPM-EPA",
                                       "IPM-NRDC",
                                       "MARKAL-NETL",
                                       "NEMS-EIA",
                                       "NEMS-OP",
                                       "NEMS-RHG",
                                       "ReEDS-NREL",
                                       "REGEN-EPRI",
                                       "RIO-REPEAT",
                                       "USREP-ReEDS"))+

      theme(  axis.ticks = element_line(color = "black"),
              axis.ticks.length = unit(-0.15, "cm"),
              legend.position = "right",
              plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_point(aes(x = 2021, y = ts_df$value[ts_df$year == 2021][1]), color = "black") +
      geom_point(
        data = IRAmedians,
        aes(x = year, y = median),
        color = "black",
        shape = 16,
        size = 2
      )
  }

  if (metric == "Emissions") {
    NoIRAfigure = ggplot(ts_df[ts_df$scenario == "No IRA", ], aes(year, value, color = model)) +
      geom_line(aes(linetype = model),size = 0.75) +
      scale_color_manual(values = test2, breaks = c("EPS-EI",
                                                    "GCAM-CGS",
                                                    "GCAM-PNNL",
                                                    "Haiku-RFF",
                                                    "IPM-EPA",
                                                    "IPM-NRDC",
                                                    "MARKAL-NETL",
                                                    "NEMS-EIA",
                                                    "NEMS-OP",
                                                    "NEMS-RHG",
                                                    "ReEDS-NREL",
                                                    "REGEN-EPRI",
                                                    "RIO-REPEAT",
                                                    "USREP-ReEDS")) +
      theme_emf() +
      scale_x_continuous(breaks = c(2021, 2025, 2030, 2035)) +
      scale_y_continuous(limits = c(ymin, ymax), breaks = brk, labels = scales::comma) +
      scale_linetype_manual(values = c("EPS-EI" = "solid",
                                       "GCAM-CGS" = "solid",
                                       "GCAM-PNNL" = "solid",
                                       "Haiku-RFF" = "solid",
                                       "IPM-EPA" = "solid",
                                       "IPM-NRDC" = "solid",
                                       "MARKAL-NETL" = "solid",
                                       "NEMS-EIA" = "dotdash",
                                       "NEMS-OP" = "twodash",
                                       "NEMS-RHG" = "solid",
                                       "ReEDS-NREL" = "longdash",
                                       "REGEN-EPRI" = "dashed",
                                       "RIO-REPEAT" = "dotted",
                                       "USREP-ReEDS" = "solid"),

                            breaks = c("EPS-EI",
                                       "GCAM-CGS",
                                       "GCAM-PNNL",
                                       "Haiku-RFF",
                                       "IPM-EPA",
                                       "IPM-NRDC",
                                       "MARKAL-NETL",
                                       "NEMS-EIA",
                                       "NEMS-OP",
                                       "NEMS-RHG",
                                       "ReEDS-NREL",
                                       "REGEN-EPRI",
                                       "RIO-REPEAT",
                                       "USREP-ReEDS"))+
      labs(title = "No IRA",
           y = expression(paste("Emissions (Mt C",O[2],"/yr)")),
           x = element_blank()) +
      theme(  axis.ticks = element_line(color = "black"),
              axis.ticks.length = unit(-0.15, "cm"),
              legend.position = "right",
              plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_point(aes(x = 2021, y = ts_df$value[ts_df$year == 2021][1]), color = "black") +
      geom_point(
        data = noIRAmedians,
        aes(x = year, y = median),
        color = "black",
        shape = 16,
        size = 2
      )

    IRAfigure = ggplot(ts_df[ts_df$scenario == "IRA", ], aes(year, value, color = model)) +
      geom_line(aes(linetype = model),size = 0.75) +
      scale_color_manual(values = test2, breaks = c("EPS-EI",
                                                    "GCAM-CGS",
                                                    "GCAM-PNNL",
                                                    "Haiku-RFF",
                                                    "IPM-EPA",
                                                    "IPM-NRDC",
                                                    "MARKAL-NETL",
                                                    "NEMS-EIA",
                                                    "NEMS-OP",
                                                    "NEMS-RHG",
                                                    "ReEDS-NREL",
                                                    "REGEN-EPRI",
                                                    "RIO-REPEAT",
                                                    "USREP-ReEDS")) +
      theme_emf() +
      scale_x_continuous(breaks = c(2021, 2025, 2030, 2035)) +
      scale_y_continuous(limits = c(ymin, ymax), breaks = brk, labels = scales::comma) +
      labs(title = "IRA",
           y = expression(paste("Emissions (Mt C",O[2],"/yr)")),
           x = element_blank()) +
      scale_linetype_manual(values = c("EPS-EI" = "solid",
                                       "GCAM-CGS" = "solid",
                                       "GCAM-PNNL" = "solid",
                                       "Haiku-RFF" = "solid",
                                       "IPM-EPA" = "solid",
                                       "IPM-NRDC" = "solid",
                                       "MARKAL-NETL" = "solid",
                                       "NEMS-EIA" = "dotdash",
                                       "NEMS-OP" = "twodash",
                                       "NEMS-RHG" = "solid",
                                       "ReEDS-NREL" = "longdash",
                                       "REGEN-EPRI" = "dashed",
                                       "RIO-REPEAT" = "dotted",
                                       "USREP-ReEDS" = "solid"),

                            breaks = c("EPS-EI",
                                       "GCAM-CGS",
                                       "GCAM-PNNL",
                                       "Haiku-RFF",
                                       "IPM-EPA",
                                       "IPM-NRDC",
                                       "MARKAL-NETL",
                                       "NEMS-EIA",
                                       "NEMS-OP",
                                       "NEMS-RHG",
                                       "ReEDS-NREL",
                                       "REGEN-EPRI",
                                       "RIO-REPEAT",
                                       "USREP-ReEDS"))+

      theme(  axis.ticks = element_line(color = "black"),
              axis.ticks.length = unit(-0.15, "cm"),
              legend.position = "right",
              plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_point(aes(x = 2021, y = ts_df$value[ts_df$year == 2021][1]), color = "black") +
      geom_point(
        data = IRAmedians,
        aes(x = year, y = median),
        color = "black",
        shape = 16,
        size = 2
      )
  }

  if(metric == "Generation") {
    NoIRAfigure = NoIRAfigure + labs(y = expression(paste("Generation (TWh)")))
    IRAfigure = IRAfigure + labs(y = expression(paste("Generation (TWh)")))
  }
  if(metric == "Primary Energy") {
    NoIRAfigure = NoIRAfigure + labs(y = expression(paste("Primary Energy (Quads)")))
    IRAfigure = IRAfigure + labs(y = expression(paste("Primary Energy (Quads)")))
  }
  if(metric == "Emissions") {
    NoIRAfigure = NoIRAfigure + labs(expression(paste("Emissions (Mt C", O[2], "/yr)")))
    IRAfigure = IRAfigure + labs(expression(paste("Emissions (Mt C", O[2], "/yr)")))
  }

  #Percent Difference
  pdfigure = pd(pd_map_ID, "", expression(paste("Percent Difference (%)")), "none", drop)
  pdfigure$pd_df = pdfigure$pd_df %>% mutate(figure_num = fig_no)

  #Absolute Difference
  adfigure = ad(diff_ID = ad_map_ID, "", metric, "none", drop)
  adfigure$ad_df = adfigure$ad_df %>% mutate(figure_num = fig_no)

  figure = (NoIRAfigure | IRAfigure) / (adfigure$figure | pdfigure$figure)+
    plot_layout(guides = "collect")

  return(list(
    figure = figure,
    ts_df = ts_df,
    ad_df = adfigure$ad_df,
    pd_df = pdfigure$pd_df
  ))

}

combine_4c_data = function(df1, df2, df3, main_metric, main_unit) {
  df1 = df1 %>%
    rename("label" = "variable_rename") %>%
    mutate(
      variable = main_metric,
      unit = main_unit)
  if (nrow(df2) > 0) {
    df2 = df2 %>%
      rename("label"= "variable_rename") %>%
      mutate(variable = "Percent Difference from No IRA",
             unit = "%")
  }
  if (nrow(df3) > 0) {
    df3 = df3 %>%
      rename("label" = "variable_rename",
             "value" = "diff") %>%
      mutate(variable = "Absolute Difference from No IRA",
             unit = main_unit)
  }

  full = bind_rows(df1,df2,df3) %>%
    select(figure_num,model,scenario,variable,unit,label,year,value)
  return(full)
}

