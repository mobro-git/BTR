
tge_breakout_figure <- function(projections_ghgi, group = 'wm', sector_select = NULL, gas_select = NULL) {
  
  if(is.null(sector_select) & is.null(gas_select)) {
    
   nonco2 = projections_ghgi %>% 
  filter(grouping == "wm") %>%
  filter(usproj_sector != "LULUCF Sink") %>%
  filter(!(str_detect(proj_name, "usrr") & usproj_sector %in% c("Transportation","Energy") & year %in% c(2045, 2050))) %>%
  filter(!(usproj_sector == "Energy" & !year %in% c(2020, 2025, 2030, 2035, 2040, 2045, 2050))) %>%
  filter(year <= 2040)

  fig <- ggplot() +
  geom_line(data = nonco2, aes(x=year,y=value,group=proj_name,color=gas), linewidth = 1) +
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
      summarise(sum = sum(value),.groups='drop') %>%
      mutate(color_col = usproj_category)

  }
  
  if(!is.null(sector_select) & is.null(gas_select)) {
    
    proj_ghgi_df <- projections_ghgi %>%
      filter(grouping %in% c('ghgi',group),
     # filter(grouping == group,
             usproj_sector == sector_select) %>% 
      group_by(proj_name, grouping, usproj_sector, usproj_category,year) %>% 
      summarise(sum = sum(value),.groups='drop') %>%
      mutate(color_col = usproj_category)
    
  }
  
  if(!is.null(sector_select) & !is.null(gas_select)) {
    
    proj_ghgi_df <- projections_ghgi %>%
      filter(grouping %in% c('ghgi',group),
     # filter(grouping == group,
             usproj_sector == sector_select,
             gas == gas_select) %>% 
      group_by(proj_name, grouping, usproj_sector, usproj_category,year) %>% 
      summarise(sum = sum(value),.groups='drop')%>%
      mutate(color_col = usproj_category)
    
  }
  
  if(!is.null(sector_select) | !is.null(gas_select)) {
  
  fig <- ggplot() +
    geom_line(data = proj_ghgi_df, aes(x=year,y=sum,group=proj_name,color=color_col), linewidth = 1) +
    geom_vline(xintercept = 2022,
               linetype = 'dashed',
               color = "black",
               # size = 0.4,
               alpha = 0.5) +
    facet_wrap(usproj_sector~color_col, scales = "free_y") +
    labs(title = paste(sector_select, gas_select, sep = ':'),y = "Mt CO2e") +
    theme_btr() +
    nolegend  
  }
  
  fig
  
}
