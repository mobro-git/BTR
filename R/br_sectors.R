# sector comparison between projections and lts
br_sectors = function(sector_df, var_choice, ytitle) {
  
  df = sector_df %>%
    filter(variable == var_choice) %>%
    filter(region == 'United States')
  
  ghgi = df %>% filter(type == "GHGI")
  ghgi_2022 = ghgi %>% filter(year == 2022)
  
  lts_no2022 = df %>% filter(type == "LTS")
  lts_2022 = lts_no2022 %>% 
    filter(year == 2025) %>%
    mutate(
      value = ghgi_2022$value,
      year = 2022,
      datasrc = "copied from GHGI")
  lts = rbind(lts_no2022, lts_2022)
  
  lts_range = lts %>%
    group_by(year) %>%
    summarize(ymax = max(value), 
              ymin = min(value), 
              med = median(value)) %>%
    mutate(year = as.numeric(year),
           type = "LTS")
  
  proj = df %>%filter(type == "proj")
  median = df %>% filter(type == "median")
  
  lts_col = "#96BBA4"
  proj_col = "#0388B3"
  
  ggplot() +
    # Historic
    geom_line(data = ghgi, aes(x = year, y = value), size = 0.7, color = "black") +
    # LTS
    geom_line(data = lts, aes(x = year, y = value, group = interaction(model, scenario)), size = 0.7, color = lts_col) +
    geom_ribbon(data = lts_range, aes(x=year, ymax=ymax, ymin=ymin), color = lts_col, fill = lts_col, alpha = 0.4 , size = 0.7) +
    # Projections
    geom_point(data = proj, aes(x = year, y = value), color = proj_col) +
    # Medians
    geom_segment(data = median, aes(x = year - 1, xend = year + 1, y = value, yend = value),
                 color = proj_col, position = position_dodge2(width = 0.5), linewidth = 1) +
    # theming
    theme_custom() +
    labs(title = "", 
         y = ytitle, 
         x = "") +
    scale_y_continuous(limits = c(0,2500), expand = c(0,0), labels = comma) +
    scale_x_continuous(breaks = c(2005, 2022, 2025, 2030, 2035)) + 
    geom_hline(aes(yintercept=0)) +
    nolegend
  
}
