br_project_brvs_sens = function(projections_ghgi,
                                brvs_tne,
                                config,
                                settings,
                                brvs_btr_subset = FALSE) {
  
  net_ghg <- projections_ghgi %>%
    filter(year %in% config$fives50,
           !grouping %in% c("IRA","wm_ltslulucf")) %>% 
    group_by(proj_name,grouping,year) %>%
    summarise(value = sum(value)) %>%
    mutate(grouping = case_when(grouping == 'wm' ~ "2024 Policy Baseline",
                                grouping == 'wm_sens' ~ "2024 BTR, Sens.",
                                TRUE~grouping))
  
 
  if (brvs_btr_subset == FALSE) {
    brvs_tne_clean <- brvs_tne %>%
      mutate(proj_name = 'br5_vs',
             grouping = "2023 BR Voluntary Supplement") %>%
      pivot_longer(cols = 5:11,
                   names_to = "year") %>%
      mutate(year = as.numeric(year)) %>% 
      filter(year >= settings$base_year) %>% 
      select(names(net_ghg))
  }
  
  if (brvs_btr_subset == TRUE) {
    brvs_tne_clean <- brvs_tne %>%
      mutate(model = case_when(model == "GCAM-PNNL" ~ "GCAM",
                               model == "NEMS-OP" ~ "OP-NEMS",
                               TRUE~model),
             proj_name = 'br5_vs',
             grouping = "2023 BR Voluntary Supplement") %>%
      pivot_longer(cols = 5:11,
                   names_to = "year") %>%
      mutate(year = as.numeric(year)) %>% 
      filter(model %in% config$model_wm,
             year >= settings$base_year) %>%
      select(names(net_ghg))
  }
  
  net_ghg_df <- net_ghg %>%
    rbind(brvs_tne_clean)
  
  ghgi <- net_ghg_df %>%
    filter(grouping == 'ghgi')
  
  net_ghg_ribbon <- net_ghg_df %>%
    group_by(grouping,year) %>%
    summarise(max = max(value),
              min = min(value))
  
  connect_brvs <- net_ghg_ribbon %>%
    filter(year == 2022) %>%
    mutate(grouping = "2023 BR Voluntary Supplement")
  connect_wm <- net_ghg_ribbon %>%
    filter(year == 2022) %>%
    mutate(grouping = "2024 Policy Baseline")
  connect_wm_sens <- net_ghg_ribbon %>%
    filter(year == 2022) %>%
    mutate(grouping = "2024 BTR, Sens.")
  
  net_ghg_final <- net_ghg_ribbon %>%
    filter(!grouping == 'ghgi') %>% 
    rbind(connect_brvs,
          connect_wm,
          connect_wm_sens)
  
  baseline = ghgi[ghgi$year == 2005,]$value
  
  ndc_targets = data.frame(
    year = c(2020, 2025, 2030),
    ymin = c((baseline * (1-.169)), (baseline * (1-.26)), (baseline * (1-.50))),
    ymax = c((baseline * (1-.171)), (baseline * (1-.28)), (baseline * (1-.52))),
    grouping = c("17% Below 2005","26-28% Below 2005","50-52% Below 2005")
  )
  

  p <- ggplot() +
    geom_line(ghgi, mapping = aes(x = year, y = value), color = 'black') +
    geom_ribbon(data = net_ghg_final, aes(x = year, ymax = max, ymin = min, group = grouping, fill = grouping, color = grouping), alpha = .6) +
    # theming
    labs(title = "",
         y = expression(paste("Net GHG Emissions (MMt ", CO[2], "e)", sep = "")),
         x = "",
         color = "",
         fill = "") +
    scale_y_continuous(limits = c(0, 7200), expand = c(0, 0),
                       breaks = c(2000,4000,6000,round(ghgi$value[1])),
                       labels = comma) +
    scale_x_continuous(breaks = c(2005, 2010, 2015, 2020, 2022, 2025, 2030, 2035, 2040, 2045, 2050), expand = c(0,0)) +
    guides(fill = guide_legend(nrow = 4, byrow = T)) +
    geom_hline(aes(yintercept = 0)) +
    theme_btr() +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.15, 0.2))
  
  
  projections = p +
    geom_rect(
      data = ndc_targets %>%
        filter(year == 2020),
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
      data = ndc_targets %>% filter(year == 2025),
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
      data = ndc_targets %>% filter(year == 2030),
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
      data = (ndc_targets %>% filter(year == 2020)),
      aes(yintercept = ymax - .01),
      linetype = "dashed",
      color = "gray",
      size = 0.4,
      alpha = 0.5
    ) +
    geom_hline(
      data = (ndc_targets %>% filter(year != 2020)),
      aes(yintercept = ymax),
      linetype = "dashed",
      color = "gray",
      size = 0.4,
      alpha = 0.5
    ) +
    geom_hline(
      data = (ndc_targets %>% filter(year != 2020)),
      aes(yintercept = ymin),
      linetype = "dashed",
      color = "gray",
      size = 0.4,
      alpha = 0.5
    ) +
    scale_subpalette_single(c(unique(net_ghg_final$grouping), unique(ndc_targets$grouping))) 
  
  projections
  
  
}

#####

kaya_brvs <- function(kaya_comparison50, data_long_clean, config, settings){
  
  kaya_data_range <- kaya_comparison50 %>%
    filter(br_version == "2024") %>%
    select(-value, -base) %>%
    pivot_wider(names_from = scenario, values_from = indexed) %>%
    mutate(
      variable = case_when(
        variable == "EmissPerEnergy" ~ "Emissions/Energy",
        variable == "EnergyPerGDP" ~ "Energy/GDP",
        variable == "GDPPerCap" ~ "GDP/Capita",
        TRUE ~ variable
      ),
      br_version = case_when(
        br_version == "2022" ~ "2022 BR",
        br_version == "2024" ~ "2024 BTR",
        TRUE ~ br_version
      )
    ) %>%
    filter(year <= 2050 & year >= 2005)
  
  kaya_comp_renamed <- kaya_comparison50  %>%
    mutate(
      variable = case_when(
        variable == "EmissPerEnergy" ~ "Emissions/Energy",
        variable == "EnergyPerGDP" ~ "Energy/GDP",
        variable == "GDPPerCap" ~ "GDP/Capita",
        TRUE~variable),
      br_version = case_when(
        br_version == "2022" ~ "2022 BR",
        br_version == "2024" ~ "2024 BTR",
        TRUE ~ br_version)) %>%
    filter(!(br_version == "2024 BTR" & variable %in% c("Emissions/Energy","Energy/GDP") & year >2022)) %>%
    filter(year <= 2050 & year >=2005)
  ############################
  # Voluntary Supplement Data
  ############################
  
  vs_df <- data_long_clean %>% 
    filter(scenario == 'leep_IRA',
           variable %in% c("Emissions / Energy",
                           "Energy / GDP",
                           "GDP / Capita",
                           "Population"),
           year %in% config$table50,
           year > 2022) %>%
    select(model,year,variable,value)
  
  index_df <- kaya_comp_renamed %>%
    filter(year == 2015,
           br_version == '2024 BTR') %>%
    mutate(model = 'EPA-GHGI',
           variable = case_when(variable == "Energy/GDP" ~ "Energy / GDP",
                                variable == "Emissions/Energy" ~ "Emissions / Energy",
                                variable == "GDP/Capita" ~ "GDP / Capita",
                                TRUE~variable)) %>%
    select(names(vs_df)) %>%
    distinct()
  
  vs_df_rbind <- vs_df %>%
    rbind(index_df)
  
  calc_df <- vs_df_rbind %>%
    group_by(variable) %>%
    mutate(pct_change_15 = round((value/value[year==2015]-1),2)) %>% 
    #mutate(pct_change_15 = round(((value - value[year == 2015]) / value[year == 2015]),2)) %>%
    mutate(index_value = 1 + pct_change_15) %>%
    ungroup() %>%
    filter(!(model == 'USREP-ReEDS' & variable %in% c('Population','GDP / Capita')))
  
  ribbon_vs_df <- calc_df %>%
    group_by(variable,year) %>%
    summarise(max = max(index_value),
              min = min(index_value)) %>%
    mutate(br_version = "2022 BR5:VS") %>% 
    select(br_version, everything())
  
  ############################
  # BTR Data
  ###########################
  
  btr_kaya <- kaya_data_range %>%
    # filter(year %in% config$table,
    #        year >= 2015) %>%
    mutate(variable = case_when(variable == "Energy/GDP" ~ "Energy / GDP",
                                variable == "Emissions/Energy" ~ "Emissions / Energy",
                                variable == "GDP/Capita" ~ "GDP / Capita",
                                TRUE~variable)) %>%
    select(names(ribbon_vs_df))
  
  kaya_full <- btr_kaya %>% rbind(ribbon_vs_df)
  
  
  
  p <- ggplot() +
    geom_ribbon(
      data = kaya_full,
      mapping = aes(
        x = year,
        ymin = min,
        ymax = max,
        fill = variable,
        color = variable,
        linetype = br_version
      ),
      alpha = 0.4,
      size = .7
    ) +
    scale_linetype_manual(values = c("2022 BR5:VS" = "dotted", "2024 BTR" = "solid")) +
    scale_subpalette_single(unique(btr_kaya$variable)) +
    labs(y = "Key Factor Index \n(2015 = 1.0)", title = "2024 BTR Inputs (solid) vs. 2022 BR5: Voluntary Supplement (dotted)") +
    scale_x_continuous(breaks = config$table50, expand = c(0, 0)) +
    geom_vline(
      xintercept = settings$base_year,
      linetype = 'dashed',
      color = "black",
      alpha = 0.5
    ) +
    theme_btr() +
    theme(
      legend.background = element_rect(color = NA),
      legend.position = 'bottom',
      panel.grid.minor.x = element_blank()
    ) +
    guides(color = guide_legend(override.aes = list(alpha = 1)))
  
  p
  
}

gdp_breakout <- function(data_long_clean, config) {
  
  var_list <- c('GDP|Consumption',
                'GDP|Imports',
                'GDP|Exports',
                'GDP|Government',
                'GDP|Investment',
                'Output|Industry|Value',
                'Output|Industry|Volume')
  
  gdp_df <- data_long_clean %>%
    filter(scenario %in% c('leep_IRA','wm'),
           variable %in% var_list,
           year %in% config$fives_proj,
           model %in% c('USREP-ReEDS', "OP-NEMS")) %>%
    select(model,scenario,year,variable,value) %>%
    group_by(scenario,year,variable) %>% 
    summarise(max = max(value),
              min = min(value))
  
  ggplot(data = gdp_df) +
    geom_ribbon(aes(x = year, ymax = max, ymin = min, fill = scenario, color = scenario), alpha = .6) +
    facet_wrap(~variable, scales = 'free_y') +
    #scale_y_continuous(labels = scales::label_currency()) +
    scale_subpalette_single(unique(gdp_df$scenario)) +
    theme_btr()
}


####

brvs_sens_sectors <- function(var_choice, brvs_btr_subset = FALSE, brvs_sectors, ghgi_comp_tab, config, settings,data_long_clean) {
  
  if (brvs_btr_subset == FALSE) {
  brvs_df <- brvs_sectors %>%
    filter(scenario == 'IRA',
           variable  == var_choice) %>%
    pivot_longer(cols = starts_with("20"),
                 names_to = "year") %>%
    mutate(year = as.numeric(year),
           grouping = "2023 BR Voluntary Supplement") %>%
    select(grouping, year, value)
  }
  
  if (brvs_btr_subset == TRUE) {
    brvs_df <- brvs_sectors %>%
      mutate(model = case_when(model == "GCAM-PNNL" ~ "GCAM",
                               model == "NEMS-OP" ~ "OP-NEMS",
                               TRUE~model)) %>%
      filter(scenario == 'IRA',
             variable  == var_choice,
             model %in% config$model_wm) %>%
      pivot_longer(cols = starts_with("20"),
                   names_to = "year") %>%
      mutate(year = as.numeric(year),
             grouping = "2023 BR Voluntary Supplement") %>%
      select(grouping, year, value)
  }
  
  brvs_median_df <- brvs_df %>%
    group_by(year) %>%
    summarise(median = median(value)) %>%
    mutate(grouping = "2023 BR Voluntary Supplement")
  
  lts_vars <- c("Emissions|CO2|Energy|Demand|Buildings|TotalDI",
                "Emissions|CO2|Energy|Demand|Transportation|TotalDI",
                "Emissions|CO2|Energy|Demand|Industry|TotalDI",
                "Emissions|CO2|Energy|Supply|Electricity")
  
  # ghgi 24 data
  ghgi_df <- ghgi_comp_tab %>%
    filter(model == config$model_hist) %>%
    filter(year %in% config$hist) %>%
    mutate(variable = case_when(
      variable == "Emissions|CO2|Energy|Demand|Industry|Total" ~ "Emissions|CO2|Energy|Demand|Industry and Fuel Production|Total",
      TRUE~variable
    )) %>%
    filter(variable == var_choice)
  # TODO: need to pull fuel production emissions from GHGI and create a new variable so that it can be included here for historical emissions
  
  ghgi_connect <- ghgi_df %>%
    filter(year == settings$base_year) %>%
    mutate(max = value,
           min = value)
  
  # LTS models and variables used for sector figures
  
  lts_df <- data_long_clean %>%
    filter(model %in% config$model_lts,
           year %in% config$fives_proj_sm50,
           variable %in% lts_vars) %>%
    # change names to match template update variable names
    mutate(variable = case_when(
      str_detect(variable, "Buildings") ~ "Emissions|CO2|Energy|Demand|Buildings|Total",
      str_detect(variable, "Transportation") ~ "Emissions|CO2|Energy|Demand|Transportation|Total",
      str_detect(variable, "Industry") ~ "Emissions|CO2|Energy|Demand|Industry and Fuel Production|Total",
      TRUE~variable)) %>%
    filter(variable == var_choice) %>%
    mutate(grouping = "LTS")
  
  lts_ribbon_df <- lts_df %>% 
    group_by(year) %>%
    summarise(max = max(value),
              min = min(value))
  
  ghgi_connect <- ghgi_df %>%
    filter(year == settings$base_year) %>%
    mutate(max = value,
           min = value) %>%
    select(names(lts_ribbon_df))
  
  lts_ribbon_df <- lts_ribbon_df %>%
    rbind(ghgi_connect) %>%
    mutate(grouping = "LTS")
  
  
  
  # models used in the projections and variables used for sector figures
  
  wm_df <- data_long_clean %>%
    filter(scenario %in% config$scen_wm,
           variable == var_choice,
           year %in% config$fives_proj_sm50,
           region == 'United States') %>%
    mutate(grouping = "2024 Policy Baseline") %>%
    select(names(brvs_df))
  
  wm_median_df <- wm_df %>%
    group_by(year) %>%
    summarise(median = median(value))%>%
    mutate(grouping = "2024 Policy Baseline")
  
  wm_sens_df <- data_long_clean %>%
    filter(scenario %in% config$scen_wm_sensitivities,
           !scenario == "wm",
           variable == var_choice,
           year %in% config$fives_proj_sm50,
           region == 'United States') %>%
    filter(variable == var_choice) %>%
    mutate(grouping = "2024 BTR, Sens.") %>%
    select(names(brvs_df))
  
  wm_sens_median_df <- wm_sens_df %>%
    group_by(year) %>%
    summarise(median = median(value))%>%
    mutate(grouping = "2024 BTR, Sens.")
  
  point_df <- brvs_df %>%
    rbind(wm_df,
          wm_sens_df)
  
  median_df <- brvs_median_df %>%
    rbind(wm_median_df,
          wm_sens_median_df)
  
  var_palette <- c("2024 Policy Baseline",
                   "2024 BTR, Sens.",
                   "2023 BR Voluntary Supplement",
                   "LTS")
  
  plot <- ggplot() +
    geom_line(data = ghgi_df, aes(x = year, y = value), color = 'black', size = 0.7) +
    geom_ribbon(data = lts_ribbon_df, aes(x = year, ymax = max, ymin = min , fill = grouping, color = grouping), alpha = 0.4 , size = 0.7) +
    geom_line(data = lts_df, aes(x = year, y = value, group = interaction(model, scenario), color = grouping)) +
    geom_point(data = point_df, aes(x = year, y = value, color = grouping)) +
    geom_segment(data = median_df, aes(x = year - 1, xend = year + 1, y = median, yend = median, color = grouping),
                 linewidth = 1) +
    scale_subpalette_single(var_palette) +
    labs(y = expression(paste("Mt C", O[2]))) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks = c(2005, 2022, 2025, 2030, 2035, 2040, 2045, 2050)) +
    theme_btr() +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.15, 0.2)
    )
    
  
  
  plot
  
}



