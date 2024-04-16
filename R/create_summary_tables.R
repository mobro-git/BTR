
group_gas_breakout_dataset <- function(projections_all_sm, config) {
  
  years_sum <- projections_all_sm %>% filter(year %in% config$table)
  dataset <- years_sum %>%
    group_by(gas, year) %>%
    summarise(mmtco2e = sum(sum)) %>%
    filter(!gas=='LULUCF Sink')
  
  dataset_wide <- dataset %>% pivot_wider(names_from = year,
                                          values_from = mmtco2e) %>% 
    arrange(factor(gas, levels = config$gas_order))
  }


group_sector_breakout_dataset <- function(projections_all_sm, config) {
  
  years_sum <- projections_all_sm %>% filter(year %in% config$table)
  dataset <- years_sum %>% group_by(usproj_sector, year) %>% summarise(mmtco2e = sum(sum)) %>% filter(!usproj_sector=='LULUCF Sink')
  
  dataset_wide <- dataset %>% pivot_wider(names_from = year,
                                          values_from = mmtco2e)%>% 
    arrange(factor(usproj_sector, levels = config$sector_order))
}

lulucf_sink_breakout <- function(projections_all_sm, config){
  
  lulucf_sink_df <- projections_all_sm %>% ungroup() %>% 
    select(gas,year,sum) %>% 
    filter(gas=='LULUCF Sink') %>%
    filter(year %in% config$table) 
  
  lulucf_sink_wide <- lulucf_sink_df %>% pivot_wider(names_from = year,
                                                     values_from = sum)
}


total_gross_emissions <- function(projections_all_sm, config, tge_header){
  
  total_gross_emissions_df <- projections_all_sm %>% ungroup %>%
    filter(!gas=='LULUCF Sink') %>%
    filter(year %in% config$table) %>%
    group_by(year) %>% 
    summarise('Total Gross Emissions' = sum(sum))
  
  tge_wide <- total_gross_emissions_df %>% pivot_wider(names_from = year,
                                                       values_from = 'Total Gross Emissions')
  
  tge_final <- cbind(tge_header,tge_wide)
  
  
}


total_net_emissions <- function(tge_df,lulucf_sink_df){
  
  
}




