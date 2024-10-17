
theme_btr <- function() {
  
  theme <- theme_light() +
    theme(
          legend.background = element_rect(fill="white",color="gray"),
          legend.title=element_blank(),
          legend.position="right",
          legend.box = "vertical",
          legend.text = element_text(size = 8),
          legend.spacing.y = unit(0.5, "mm"),
          plot.margin = margin(8,10,8,8),
          axis.text.y = element_text(size=14),
          axis.text.x = element_text(size=14, angle = 45, hjust = 1),
          axis.ticks = element_blank(),
          axis.title.y = element_text(face="bold",vjust=2),
          axis.title.x = element_blank(),
          strip.text.x = element_text(size = 10, color = "black",face = "bold"),
          strip.text.y = element_text(size = 9, color = "black",face = "bold"),
          strip.background = element_rect(fill=NA, linewidth=1),
          panel.grid.minor.x = element_blank(),
          text = element_text(size = 14))

  theme
}

br_project = function(ghgi, proj, settings, targets = NULL, legend_position = c(0.15, 0.2), legend_off = NULL) {
  
  proj_range = proj %>%
    group_by(year, grouping) %>%
    summarize(ymax = max(sum),
              ymin = min(sum),
              med = median(sum)) %>%
    mutate(year = as.numeric(year))
  
  var_palette = c(
    unique(ghgi$grouping),
    unique(proj$grouping),
    unique(targets$grouping))
  
  projections = ggplot() +
    # Historic
    geom_line(ghgi,mapping = aes(x = year, y = sum, color = grouping),size = 0.7
    ) +
    
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
    scale_x_continuous(breaks = c(2005, 2010, 2015, 2020, 2022, 2025, 2030, 2035, 2040), expand = c(0,0)) +
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

br_project_pct_change = function(ghgi, proj, targets = NULL, legend_position = c(0.15, 0.2), legend_off = NULL) {
  
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
    scale_x_continuous(breaks = c(2005, 2010, 2015, 2022, 2025, 2030, 2035), expand = c(0,0)) +
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


