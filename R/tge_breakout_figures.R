
tge_breakout_figure <- function(projections_ghgi, group = 'wm', sector_select = NULL, gas_select = NULL) {
  
  if(is.null(sector_select) & is.null(gas_select)) {
    
    projections_all_sm_df <- projections_ghgi %>%
      filter(grouping %in% c('ghgi',group)) %>% 
      group_by(proj_name, grouping, gas, usproj_sector, year) %>% 
      summarise(sum = sum(value),.groups='drop') %>%
      group_by(gas, usproj_sector) %>%
      ungroup()
    
    nonco2 = projections_all_sm_df %>% 
      #filter(grouping == "wm") %>%
      filter(usproj_sector != "LULUCF Sink") %>%
      filter(!(str_detect(proj_name, "usrr") & usproj_sector %in% c("Transportation","Energy") & year %in% c(2045, 2050))) %>%
      filter(!(usproj_sector == "Energy" & !year %in% c(2020, 2025, 2030, 2035, 2040, 2045, 2050))) %>%
      filter(year <= 2040)
    
    fig <- ggplot() +
      geom_line(data = nonco2, aes(x=year,y=sum,group=proj_name,color=gas), linewidth = 1) +
      facet_grid(usproj_sector~gas, scales = "free_y") +
      labs(y = "Mt CO2e") +
      theme_btr() +
      nolegend
    
  }
  
  if(is.null(sector_select) & !is.null(gas_select)) {
    
    proj_ghgi_df <- projections_ghgi %>%
     filter(grouping %in% c('ghgi',group),
     # filter(grouping == group,
             gas == gas_select) %>% 
      group_by(proj_name, grouping, usproj_sector, usproj_category,year) %>% 
      summarise(sum = sum(value),.groups='drop')
    
    fig <- ggplot() +
      geom_line(data = proj_ghgi_df, aes(x=year,y=sum,group=proj_name,color=usproj_category), linewidth = 1) +
      facet_wrap(usproj_sector~usproj_category, scales = "free_y") +
      labs(y = "Mt CO2e") +
      theme_btr() +
      nolegend
    
  }
  
  if(!is.null(sector_select) & is.null(gas_select)) {
    
    proj_ghgi_df <- projections_ghgi %>%
      filter(grouping %in% c('ghgi',group),
     # filter(grouping == group,
             usproj_sector == sector_select) %>% 
      group_by(proj_name, grouping, usproj_sector, usproj_category,year) %>% 
      summarise(sum = sum(value),.groups='drop')
    
    
    fig <- ggplot() +
      geom_line(data = proj_ghgi_df, aes(x=year,y=sum,group=proj_name,color=usproj_category), linewidth = 1) +
      facet_wrap(usproj_sector~usproj_category, scales = "free_y") +
      labs(y = "Mt CO2e") +
      theme_btr() +
      nolegend
    
  }
  
  if(!is.null(sector_select) & !is.null(gas_select)) {
    
    proj_ghgi_df <- projections_ghgi %>%
      filter(grouping %in% c('ghgi',group),
     # filter(grouping == group,
             usproj_sector == sector_select,
             gas == gas_select) %>% 
      group_by(proj_name, grouping, usproj_sector, usproj_category,year) %>% 
      summarise(sum = sum(value),.groups='drop')
    
    
    fig <- ggplot() +
      geom_line(data = proj_ghgi_df, aes(x=year,y=sum,group=proj_name,color=usproj_category), linewidth = 1) +
      facet_wrap(usproj_sector~usproj_category, scales = "free_y") +
      labs(y = "Mt CO2e") +
      theme_btr() +
      nolegend
    
  }
  
  fig
}
