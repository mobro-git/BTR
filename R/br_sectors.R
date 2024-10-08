# sector comparison between projections and lts
br_sectors = function(sector_df, var_choice, ytitle) {
  
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
    scale_x_continuous(breaks = c(2005, 2022, 2025, 2030, 2035, 2040)) + 
    geom_hline(aes(yintercept=0)) +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.15, 0.2),
      axis.text = element_text(size = 20),
      axis.title.y = element_text(size = 16)
      )
  
}

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

# sector comparison between projections and lts
br_sectors_callout_lts = function(sector_df, var_choice, ytitle, lts_mod, lts_scen) {
  
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
    scale_x_continuous(breaks = c(2005, 2022, 2025, 2030, 2035, 2040)) + 
    geom_hline(aes(yintercept=0)) +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.15, 0.2)) +
    geom_line(data = (lts %>% filter(model == lts_mod & scenario == lts_scen)), aes(x = year, y = value, group = interaction(model, scenario)), color = "red", size = 0.9)
  
}

br_sectors_callout_lts_panels = function(sector_df, lts_mod, lts_scen) {
  
  # buildings
  buildings = br_sectors_callout_lts(sector_df,
                         "Emissions|CO2|Energy|Demand|Buildings|Total",
                         expression(paste("Buildings Emissions (MMt C", O[2], ")")),
                         lts_mod = lts_mod, lts_scen = lts_scen)
  
  # electricity
  electricity = br_sectors_callout_lts(sector_df,
                           "Emissions|CO2|Energy|Supply|Electricity",
                           expression(paste("Electricity Emissions (MMt C", O[2], ")")),
                           lts_mod = lts_mod, lts_scen = lts_scen)
  
  # transportation
  transportation = br_sectors_callout_lts(sector_df,
                              "Emissions|CO2|Energy|Demand|Transportation|Total",
                              expression(paste("Transportation Emissions (MMt C", O[2], ")")),
                              lts_mod = lts_mod, lts_scen = lts_scen)
  
  # industry
  industry = br_sectors_callout_lts(sector_df,
                        "Emissions|CO2|Energy|Demand|Industry and Fuel Production|Total",
                        expression(paste("Industry Emissions (MMt C", O[2], ")")),
                        lts_mod = lts_mod, lts_scen = lts_scen)
  
  panels = (electricity + buildings)/(transportation + industry)
  panels
  
}


