ranges = function(all_data, summary_data, filter_variable, subtitle = "", top = FALSE, bottom = FALSE) {
  data_small = all_data %>% filter(sector == filter_variable)
  summ_small = summary_data %>% filter(sector == filter_variable)
  p = data_small %>%
    ggplot() +
    geom_point(aes(x = pct_diff, y = scenario, color = scenario, shape = scenario), alpha = 0.6, size = 1) +
    geom_segment(data = summ_small,
                 aes(x = min, xend = max, y = scenario, yend = scenario, color = scenario),
                 alpha = 0.3, linewidth = 3) +
    geom_point(data = summ_small, aes(x = min, y = scenario, color = scenario, shape = scenario), size = 2) +
    geom_point(data = summ_small, aes(x = max, y = scenario, color = scenario, shape = scenario), size = 2) +
    geom_point(data = summ_small, aes(x = median, y = scenario, color = scenario),
               shape = "|", size = 1.5, stroke = 3.5) +
    geom_point(data = (data_small %>% filter(model %in% c("ReEDS-NRELr","ReEDS-NREL"))),
               aes(x=pct_diff, y = scenario), shape = "|", size = 3, stroke = 4, color = "green") +
    geom_point(data = (data_small %>% filter(model == "NEMS-OP")),
               aes(x=pct_diff, y = scenario),shape = "|", size = 3, stroke = 4, color = "purple") +
    geom_point(data = (data_small %>% filter(model == "IPM-EPA")),
               aes(x=pct_diff, y = scenario),shape = "|", size = 3, stroke = 4, color = "#00FFFF") +
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

spg_msb = function(df, title, yname, gd, ymin, ymax, ybreaks, yax_format, annotate, historic_coord = c(0,0), preira_coord = c(0,0), ira_coord = c(0,0), config, figmap_leep_timeseries) {

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

  figure = ggplot(df, aes(year,value, color = scenario2, group = interaction(model, scenario))) +
    geom_line(aes(alpha = alpha), size = 0.5) +
    scale_subpalette(subpalettes, "Emissions|CO2|Energy|Demand|Industry") +
    theme_emf() +
    scale_x_continuous(breaks = c(2005, 2021, 2025, 2030, 2035)) +
    scale_y_continuous(limits = c(ymin, ymax), breaks = ybreaks, labels = yax_format) +
    scale_alpha(range = c(1, 1), guide = F) +
    labs(title = title,
         x = element_blank(),
         y = yname) +
    theme(legend.position = gd,
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks = element_blank()) +
    annotate("text", x = historic_coord[1], y = historic_coord[2], label = "Historical", color = "black", alpha = annotate) +
    annotate("text", x = preira_coord[1], y = preira_coord[2], label = "No IRA", color = "#F28063", alpha = annotate) +
    annotate("text", x = ira_coord[1], y = ira_coord[2], label = "IRA", color = "#0388B3", alpha = annotate)

  return(list(figure = figure, data = df))
}

dotted_msb = function(df, spg, metric, ymin, ymax, config, figmap_leep_timeseries, ddge = 0.2) {

  subpalettes = create_subpalettes(figmap_leep_timeseries, config)

  df_30 = df %>%
    filter(year %in% c(2030))
  df_stat_30 = df_30 %>%
    group_by(scenario2,year,variable_rename) %>%
    summarise(mean = mean(value),
              median = median(value))

  df_35 = df %>%
    filter(year %in% c(2035))
  df_stat_35 = df_35 %>%
    group_by(scenario2,year,variable_rename) %>%
    summarise(mean = mean(value),
              median = median(value))

  mean_diff_30 = df_stat_30[df_stat_30$scenario2 == "No IRA","mean"]$mean - df_stat_30[df_stat_30$scenario2 == "IRA","mean"]$mean
  mean_diff_35 = df_stat_35[df_stat_35$scenario2 == "No IRA","mean"]$mean - df_stat_35[df_stat_35$scenario2 == "IRA","mean"]$mean
  median_diff_30 = df_stat_30[df_stat_30$scenario2 == "No IRA","median"]$median - df_stat_30[df_stat_30$scenario2 == "IRA","median"]$median
  median_diff_35 = df_stat_35[df_stat_35$scenario2 == "No IRA","median"]$median - df_stat_35[df_stat_35$scenario2 == "IRA","median"]$median
  # stick em all in a data frame
  stats = data.frame("year" = c(2030,2035,2030,2035), "metric" = c("mean","mean","median","median"),
                     "difference" = c(mean_diff_30, mean_diff_35, median_diff_30, median_diff_35))

  # a few of the ggplot lines of code vary based on the year or on the metric chosen, so conditionally choose the right one here
  if (metric == "mean") {
    segment_code_30 = geom_segment(data = df_stat_30, aes(x = year - 0.1, xend = year + 0.1, y = mean, yend = mean, color = scenario2),
                                   size = 1)
    segment_code_35 = geom_segment(data = df_stat_35, aes(x = year - 0.1, xend = year + 0.1, y = mean, yend = mean, color = scenario2),
                                   size = 1)
  } else if (metric == "median") {
    segment_code_30 = geom_segment(data = df_stat_30, aes(x = year - 0.1, xend = year + 0.1, y = median, yend = median, color = scenario2),
                                   size = 1)
    segment_code_35 = geom_segment(data = df_stat_35, aes(x = year - 0.1, xend = year + 0.1, y = median, yend = median, color = scenario2),
                                   size = 1)
  }



  dots_30 = ggplot() +
    geom_point(data = df_30, aes(x = year, y = value, color = scenario2, alpha = alpha, shape = scenario2),size = 1.5, position = position_dodge(width = ddge)) +
    segment_code_30 +
    scale_x_continuous(breaks = c(2030), labels = c("2030"), limits = c(2029.9, 2030.1)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    theme_emf() +
    scale_subpalette(subpalettes, "Emissions|CO2|Energy|Demand|Industry") +
    scale_shape_manual(values = c("IRA" = 1, "No IRA" = 2)) +
    theme(panel.grid = element_blank(), plot.title = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust=1),
          axis.title.y = element_blank(), axis.title.x = element_blank(),
          axis.ticks = element_blank(), panel.border = element_blank(),
          legend.position = "none", plot.margin = margin(0,0,0,0))+
    scale_alpha(range = c(1, 1), guide = F)

  # dot plot for 2035
  dots_35 = ggplot() +
    geom_point(data = df_35, aes(x = year, y = value, color = scenario2, alpha = alpha, shape = scenario2), size = 1.5, position = position_dodge(width = ddge)) +
    segment_code_35 +
    scale_x_continuous(breaks = c(2035), labels = c("2035"), limits = c(2034.9, 2035.1)) +
    scale_y_continuous(limits = c(ymin,ymax)) +
    theme_emf() +
    scale_subpalette(subpalettes, "Emissions|CO2|Energy|Demand|Industry") +
    scale_shape_manual(values = c("IRA" = 1, "No IRA" = 2)) +
    theme(panel.grid = element_blank(), plot.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust=1),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(), axis.title.x = element_blank(),
          axis.ticks = element_blank(), panel.border = element_blank(),
          legend.position = "none", plot.margin = margin(0,0,0,0))+
    scale_alpha(range = c(1, 1), guide = F)

  figure = spg + dots_30 + dots_35 +
    plot_layout(widths = c(10,1,1)) +
    theme(plot.margin = margin(0,0,0,0))

  return(figure)
}

delta_msb = function(pd_map_ID, drop, config, clean_data, figmap_leep_timeseries) {

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
      axis.ticks = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.spacing.x = unit(4, "mm"),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks = c(2021, 2025, 2030, 2035)) +
    scale_y_continuous(name = NULL, sec.axis = sec_axis(~., name = expression("% Difference from No IRA"))) +
    guides(y = "none")
  return(list(figure = figure, medians = medians, df = df))
}
