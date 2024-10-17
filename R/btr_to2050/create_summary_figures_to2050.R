
theme_btr <- function() {
  
  theme <- theme_light() +
    theme(text = element_text(size = 12),
          legend.background = element_rect(fill="white",color="gray"),
          legend.title=element_blank(),
          legend.position="right",
          legend.box = "vertical",
          legend.text = element_text(size = 8),
          legend.spacing.y = unit(0.5, "mm"),
          plot.margin = margin(8,10,8,8),
          axis.text.y = element_text(size=9),
          axis.text.x = element_text(size=9, angle = 45, hjust = 1),
          axis.ticks = element_blank(),
          axis.title.y = element_text(face="bold",vjust=2),
          axis.title.x = element_blank(),
          strip.text.x = element_text(size = 10, color = "black",face = "bold"),
          strip.text.y = element_text(size = 9, color = "black",face = "bold"),
          strip.background = element_rect(fill=NA, linewidth=1),
          panel.grid.minor.x = element_blank())

  theme
}

br_project50 = function(ghgi, proj, settings, targets = NULL, legend_position = c(0.15, 0.2), legend_off = NULL) {
  
  proj_range = proj %>%
    group_by(year, grouping) %>%
    summarize(ymax = max(sum),
              ymin = min(sum),
              med = median(sum)) %>%
    mutate(year = as.numeric(year))
  
  # var_palette = unique(c(
  #   unique(ghgi$grouping),
  #   unique(proj$grouping),
  #   unique(proj_range$grouping),
  #   unique(targets$grouping)
  # ))
  
  var_palette = c(
    unique(ghgi$grouping),
    unique(proj$grouping),
    unique(targets$grouping))
  
  projections = ggplot() +
    # Historic
    geom_line(ghgi,mapping = aes(x = year, y = sum, color = grouping),size = 0.7
    ) +
    # WM
   # geom_line(proj,mapping = aes(x = year, y = sum, group = proj_name, color = grouping),alpha = 0.5,size = 0.7
    #) +
    geom_ribbon(proj_range,mapping = aes(x = year,ymax = ymax, ymin = ymin,
                                         fill = grouping, 
                                         color = grouping),
                alpha = 0.4 ,
                size = 0.7
    ) +
    geom_vline(xintercept = settings$base_year,
               linetype = 'dashed',
               color = "black",
              # size = 0.4,
               alpha = 0.5
              ) +
    # theming
    labs(title = "",
         y = expression(paste("Net GHG Emissions (MMt ", CO[2], "e)", sep = "")),
         x = "",
         color = "",
         fill = "") +
    scale_y_continuous(limits = c(0, 7200), expand = c(0, 0),
                       breaks = c(2000,4000,6000,round(ghgi$sum[1])),
                       labels = comma) +
    scale_x_continuous(breaks = c(2005, 2010, 2015, 2020, 2022, 2025, 2030, 2035, 2040, 2045, 2050), expand = c(0,0)) +
    scale_subpalette_single(var_palette) +
    guides(fill = guide_legend(nrow = 4, byrow = T)) +
    geom_hline(aes(yintercept = 0)) +
    theme_btr() +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.15, 0.2))
  
 projections
  
  # add targets boxes and horizontal lines
  if (!is.null(targets)) {
    projections = projections +
      geom_rect(
        data = targets %>% filter(year == 2020),
        aes(
          ymin = ymin,
          ymax = ymax,
          xmin = year - .3,
          xmax = year + .3,
          color = grouping,
          fill = grouping
        ),
        alpha = 0.5
      ) +
      geom_rect(
        data = targets %>% filter(year == 2025),
        aes(
          ymin = ymin,
          ymax = ymax,
          xmin = year - .3,
          xmax = year + .3,
          color = grouping,
          fill = grouping
        ),
        alpha = 0.5
      ) +
      geom_rect(
        data = targets %>% filter(year == 2030),
        aes(
          ymin = ymin,
          ymax = ymax,
          xmin = year - .3,
          xmax = year + .3,
          color = grouping,
          fill = grouping
        ),
        alpha = 0.5
      ) +
      geom_hline(
        data = (targets %>% filter(year == 2020)),
        aes(yintercept = ymax - .01),
        linetype = "dashed",
        color = "gray",
        size = 0.4,
        alpha = 0.5
      ) +
      geom_hline(
        data = (targets %>% filter(year != 2020)),
        aes(yintercept = ymax),
        linetype = "dashed",
        color = "gray",
        size = 0.4,
        alpha = 0.5
      ) +
      geom_hline(
        data = (targets %>% filter(year != 2020)),
        aes(yintercept = ymin),
        linetype = "dashed",
        color = "gray",
        size = 0.4,
        alpha = 0.5
      )
  }
  
  # remove legend
  if (!is.null(legend_off)) {
    projections = projections + nolegend
  }
  
  projections
  
}

###

br_project_pct_change50 = function(ghgi, proj, targets = NULL, legend_position = c(0.15, 0.2), legend_off = NULL) {
  
  proj_range = proj %>%
    group_by(year, grouping) %>%
    summarise(ymax = max(pct_change_05),
              ymin = min(pct_change_05),
              med = median(pct_change_05),
              .groups = 'drop') %>%
    mutate(year = as.numeric(year))
  
  # var_palette = unique(c(
  #   unique(ghgi$grouping),
  #   unique(proj$grouping),
  #   unique(proj_range$grouping),
  #   unique(targets$grouping)
  # ))
  
  var_palette = c(
    unique(ghgi$grouping),
    unique(proj$grouping),
    unique(targets$grouping))
  
  projections = ggplot() +
    # Historic
    geom_line(ghgi,mapping = aes(x = year, y = pct_change_05, group = proj_name, color = grouping),size = 0.7
    ) +
    # WM
    geom_line(proj,mapping = aes(x = year, y = pct_change_05, group = proj_name, color = grouping),alpha = 0.5,size = 0.7
    ) +
    geom_ribbon(proj_range,mapping = aes(x = year,ymax = ymax, ymin = ymin,fill = grouping,color = grouping),
                alpha = 0.4 ,
                size = 0.7
    ) +
    # theming
    labs(title = "",
         y = expression(paste("Percent Change Net GHG Emissions from 2005", sep = "")),
         x = "",
         color = "",
         fill = "") +
    scale_y_continuous(limits = c(-1, 0.05), expand = c(0, 0)) +
    scale_x_continuous(breaks = c(2005, 2010, 2015, 2022, 2025, 2030, 2035, 2045, 2050), expand = c(0,0)) +
    scale_subpalette_single(var_palette) +
    guides(fill = guide_legend(nrow = 4, byrow = T)) +
    geom_hline(aes(yintercept = 0)) +
    theme_btr() +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.15, 0.2))
  
  # projections
  
  # add targets boxes and horizontal lines
  if (!is.null(targets)) {
    projections = projections +
      geom_rect(
        data = targets %>% filter(year == 2020),
        aes(
          ymin = ymin,
          ymax = ymax,
          xmin = year - .3,
          xmax = year + .3,
          color = grouping,
          fill = grouping
        ),
        alpha = 0.5
      ) +
      geom_rect(
        data = targets %>% filter(year == 2025),
        aes(
          ymin = ymin,
          ymax = ymax,
          xmin = year - .3,
          xmax = year + .3,
          color = grouping,
          fill = grouping
        ),
        alpha = 0.5
      ) +
      geom_rect(
        data = targets %>% filter(year == 2030),
        aes(
          ymin = ymin,
          ymax = ymax,
          xmin = year - .3,
          xmax = year + .3,
          color = grouping,
          fill = grouping
        ),
        alpha = 0.5
      ) +
      geom_hline(
        data = (targets %>% filter(year == 2020)),
        aes(yintercept = ymax - .01),
        linetype = "dashed",
        color = "gray",
        size = 0.4,
        alpha = 0.5
      ) +
      geom_hline(
        data = (targets %>% filter(year != 2020)),
        aes(yintercept = ymax),
        linetype = "dashed",
        color = "gray",
        size = 0.4,
        alpha = 0.5
      ) +
      geom_hline(
        data = (targets %>% filter(year != 2020)),
        aes(yintercept = ymin),
        linetype = "dashed",
        color = "gray",
        size = 0.4,
        alpha = 0.5
      )
  }
  
  # remove legend
  if (!is.null(legend_off)) {
    projections = projections + nolegend
  }
  
  projections
  
}

####

# sector comparison between projections and lts to 2050
br_sectors50 = function(sector_df, var_choice, ytitle) {
  
  df = sector_df %>%
    filter(variable == var_choice) %>%
    filter(region == 'United States') %>%
    mutate(pct_change_05 = round((value/value[year==2005]-1),2))
  
  ghgi = df %>% filter(type == "GHGI")
  ghgi_2022 = ghgi %>% filter(year == 2022)
  
  lts_no2022 = df %>% filter(type == "LTS")
  lts_2022 = lts_no2022 %>% 
    filter(year == 2025) %>%
    mutate(
      value = ghgi_2022$value,
      year = 2022,
      datasrc = "copied from GHGI")
  lts = rbind(lts_no2022, lts_2022) %>%
    mutate(type = "Long-Term Strategy")
  
  lts_range = lts %>%
    group_by(year) %>%
    summarize(ymax = max(value), 
              ymin = min(value), 
              med = median(value)) %>%
    mutate(year = as.numeric(year),
           type = "Long-Term Strategy")
  
  proj = df %>%
    filter(type == "proj") %>%
    mutate(type = "2024 Policy Baseline")
  
  median = df %>% filter(type == "median")%>%
    mutate(type = "Median Value")
  
  lts_col = "#96BBA4"
  proj_col = "#0388B3"
  var_palette  = c("2024 Policy Baseline","Median Value",'Long-Term Strategy')
  
  ggplot() +
    # Historic
    geom_line(data = ghgi, aes(x = year, y = value), size = 0.7, color = "black") +
    # LTS
    geom_line(data = lts, aes(x = year, y = value, group = interaction(model, scenario), color = type), size = 0.7) +
    geom_ribbon(data = lts_range, aes(x=year, ymax=ymax, ymin=ymin, fill = type), alpha = 0.4 , size = 0.7) +
    # Projections
    geom_point(data = proj, aes(x = year, y = value, color = type)) +
    # Medians
    geom_segment(data = median, aes(x = year - 1, xend = year + 1, y = value, yend = value, color = type),
                 position = position_dodge2(width = 0.5), linewidth = 1) +
    scale_subpalette_single(var_palette) +
    # theming
    theme_custom() +
    labs(title = "", 
         y = ytitle, 
         x = "") +
    scale_y_continuous(limits = c(0,2500), expand = c(0,0), labels = comma) +
    scale_x_continuous(breaks = c(2005, 2022, 2025, 2030, 2035, 2040, 2045, 2050)) + 
    geom_hline(aes(yintercept=0)) +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.15, 0.2))
  
}