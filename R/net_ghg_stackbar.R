
net_ghg_sb_data_processing <- function(projections_all_sm, config, settings) {
  proj_all_sm_sb <- projections_all_sm %>%
    mutate(gas = case_when(gas == "CO2" ~ "CO2",
                           !gas %in% c("CO2", 'LULUCF Sink') ~ "Non-CO2",
                           gas == "LULUCF Sink" ~ "LULUCF Sink")) %>%
    group_by(proj_name,year,gas) %>%
    summarise(mmtco2e = sum(sum), .groups = 'drop') %>%
    filter(year >= settings$base_year) %>%
    filter(year %in% config$base_proj)
  
  no_ghgi <- proj_all_sm_sb %>% filter(!proj_name == 'ghgi')
  
  ghgi <- proj_all_sm_sb %>% filter(proj_name == 'ghgi')
  
  dfs <- list()
  
  for(name in unique(no_ghgi$proj_name)) {
    df <- proj_all_sm_sb %>% filter(proj_name == name)
    add_ghgi <- ghgi %>%
      rbind(df) %>%
      mutate(proj_name = name)
    
    dfs[[name]] <- add_ghgi
    
  }
  
  proj_all_sm_sb_final <- bind_rows(dfs)
  
  net_co2_df <- proj_all_sm_sb_final %>%
    group_by(proj_name, year) %>%
    summarise(net_co2 = sum(mmtco2e), .groups = 'drop')
  
  proj_all_sm_sb_join <- proj_all_sm_sb_final %>%
    left_join(net_co2_df, by = c('proj_name','year')) %>%
    mutate(year = as.factor(year)) %>%
    mutate(gas = factor(gas, levels = c('CO2','Non-CO2','LULUCF Sink')))
}


br_project_net_ghg_sb <- function(proj_all_sm_sb_join, config){
  fills <- c("CO2" =  "#0388B3",
             "Non-CO2" = "#E6544D",
             "LULUCF Sink" = "#16B231")
  
  #palette = create_subpalettes_df(proj_all_sm_sb_join, "gas")
  
  stackbar <- ggplot(proj_all_sm_sb_join, aes(year, mmtco2e)) +
    geom_bar(aes(fill = gas),
             stat = 'identity') +
    scale_fill_manual(values = fills,
                      labels = c(expression('CO'[2]), expression('Non-CO'[2]), 'LULUCF Sink')) +
    # scale_subpalette(sub_palettes, "temp") +
    geom_point(aes(year,net_co2)) +
    geom_hline(yintercept = 0,
               color = 'black',
               size = 1) +
    facet_grid(cols = vars(proj_name)) +
    scale_y_continuous(labels = comma, breaks = c(-1000,-500,0,2000,4000,6000)) +
    #  scale_x_continuous(breaks = config$base_proj) +
    labs(y = expression(paste("MMt ", CO[2], "e", sep = ""))) +
    guides(fill = guide_legend(order = 1),
           color = guide_legend(override.aes = list(shape = 16,
                                                    color = 'black',
                                                    size = 3),
                                title = NULL,
                                label = expression(paste0('Net ',CO[2],'e')))) +
    # scale_color_manual(
    #   values = c('net_co2' = 'black'),
    #   name = NULL,
    #   labels = c(expression(Net~CO[2]~e))
    # ) +
    theme_btr()
  
}
