
gen_gas_dataset <- function(projections_all_sm, config) {
  
  years_sum <- projections_all_sm %>% filter(year %in% config$table)
  gas_dataset <- years_sum %>%
    group_by(proj_name, gas, year) %>%
    summarise(mmtco2e = sum(sum)) 
  #%>%
   # filter(!gas=='LULUCF Sink')
  
  #gas_dataset_wide <- gas_dataset %>% pivot_wider(names_from = year,
  #                                        values_from = mmtco2e) %>% 
   # arrange(factor(gas, levels = config$gas_order))
  }




gen_gas_breakout_df <- function(gas_dataset, config, category_order, lulucf_sink_breakout_df) {
  
  
  processed_dfs <- list()
  
  years_proj <- as.character(config$table[config$table > config$base_year])
  years_hist <- as.character(config$table[config$table <= config$base_year])
  
  
  for(value in category_order) {
    
    subset_df <- gas_dataset %>% filter(gas == value)
    
    ghgi_subset <-
      subset_df %>% filter(proj_name == 'ghgi') %>% filter(year <= config$base_year)
    proj_subset <-
      subset_df %>% filter(!proj_name == 'ghgi') %>% filter(year > config$base_year)
    
    ghgi_subset_wide <- ghgi_subset %>% pivot_wider(names_from = year,
                                                    values_from = mmtco2e) %>% ungroup() %>% select(years_hist)
    
    proj_subset_wide <-
      proj_subset %>% pivot_wider(names_from = year,
                                  values_from = mmtco2e)
    ghgi_subset_wide_rep <-
      do.call('rbind', replicate(nrow(proj_subset_wide), ghgi_subset_wide, simplify =
                                   FALSE))
    
    value_df <- cbind(proj_subset_wide, ghgi_subset_wide_rep) %>% select(proj_name, gas,   as.character(config$table))
    
    processed_dfs[[value]] <- value_df
    
    
  }
  breakout_df <- bind_rows(processed_dfs)
  
  
  gas_table_df <- breakout_df %>% rbind(lulucf_sink_breakout_df) 
  return(gas_table_df)
  
}

###################################

gen_sector_dataset <- function(projections_all_sm, config) {
  
  years_sum <- projections_all_sm %>% filter(year %in% config$table)
  sector_dataset <- years_sum %>% group_by(proj_name, usproj_sector, year) %>% summarise(mmtco2e = sum(sum)) 
  #%>% filter(!usproj_sector=='LULUCF Sink')
  
#   sector_dataset_wide <- sector_dataset %>% pivot_wider(names_from = year,
#                                           values_from = mmtco2e)%>% 
#     arrange(factor(usproj_sector, levels = config$sector_order))
}

gen_sector_breakout_df <- function(sector_dataset, config, category_order, lulucf_sink_breakout_df) {
  
  
  processed_dfs <- list()
  
  years_proj <- as.character(config$table[config$table > config$base_year])
  years_hist <- as.character(config$table[config$table <= config$base_year])
  
  
  for(value in category_order) {
    
    subset_df <- sector_dataset %>% filter(usproj_sector == value)
    
    ghgi_subset <-
      subset_df %>% filter(proj_name == 'ghgi') %>% filter(year <= config$base_year)
    proj_subset <-
      subset_df %>% filter(!proj_name == 'ghgi') %>% filter(year > config$base_year)
    
    ghgi_subset_wide <- ghgi_subset %>% pivot_wider(names_from = year,
                                                    values_from = mmtco2e) %>% ungroup() %>% select(years_hist)
    
    proj_subset_wide <-
      proj_subset %>% pivot_wider(names_from = year,
                                  values_from = mmtco2e)
    ghgi_subset_wide_rep <-
      do.call('rbind', replicate(nrow(proj_subset_wide), ghgi_subset_wide, simplify =
                                   FALSE))
    
    value_df <- cbind(proj_subset_wide, ghgi_subset_wide_rep) %>% select(proj_name, usproj_sector,   as.character(config$table))
    
    processed_dfs[[value]] <- value_df
    
    
  }
  breakout_df <- bind_rows(processed_dfs)
  
  
  usproj_sector_table_df <- breakout_df %>% rbind(lulucf_sink_breakout) 
  return(usproj_sector_table_df)
}



###########################################################



gen_total_gross_emissions <- function(gas_breakout_df){
  
  summarized_df <- gas_breakout_df %>% group_by(proj_name) %>% 
    summarise(across(where(is.numeric), sum, na.rm=TRUE)) %>% 
    mutate(source = 'Total Gross Emissions') %>% 
    select(proj_name,source, everything())
  
  
}


gen_total_net_emissions <- function(gas_breakout_df,lulucf_sink_breakout_df){
  
  cols_gas_breakout <- colnames(gas_breakout_df)
  
  missing_cols <- setdiff(cols_gas_breakout, colnames(lulucf_sink_breakout_df))
  lulucf_sink_breakout_df[missing_cols] <- NA
  
  lulucf_sink_breakout_df <- lulucf_sink_breakout_df[, cols_gas_breakout]
  
  combined_df <- bind_rows(gas_breakout_df, lulucf_sink_breakout_df)
  
  tne_df <- summarise_emissions(combined_df)%>% 
    mutate(source = 'Total Net Emissions') %>% 
    select(proj_name,source, everything())
  

  return(tne_df)
}

#################################################

create_gas_table_df <- function(gas_breakout_df,
                                total_gross_emissions_df,
                                lulucf_sink_breakout,
                                total_net_emissions) {
  
  total_gross_emissions_df<- total_gross_emissions_df %>% rename(gas = source)
  total_net_emissions<- total_net_emissions %>% rename(gas = source)
  
  cols_gas_breakout <- colnames(gas_breakout_df)
  
  missing_cols <- setdiff(cols_gas_breakout, colnames(lulucf_sink_breakout))
  lulucf_sink_breakout[missing_cols] <- NA
  
  lulucf_sink_breakout <- lulucf_sink_breakout[, cols_gas_breakout]
  
  
  gas_table_df <- gas_breakout_df %>% rbind(total_gross_emissions_df) %>% rbind(lulucf_sink_breakout) %>% rbind(total_net_emissions)
  
}

create_sector_table_df <- function(sector_breakout_df,
                                total_gross_emissions_df,
                                lulucf_sink_breakout,
                                total_net_emissions) {
  lulucf_sink_breakout<- lulucf_sink_breakout %>% rename(usproj_sector = gas)
  total_gross_emissions_df<- total_gross_emissions_df %>% rename(usproj_sector = source)
  total_net_emissions<- total_net_emissions %>% rename(usproj_sector = source)
  
  cols_sector_breakout <- colnames(sector_breakout_df)
  
  missing_cols <- setdiff(cols_sector_breakout, colnames(lulucf_sink_breakout))
  lulucf_sink_breakout[missing_cols] <- NA
  
  lulucf_sink_breakout <- lulucf_sink_breakout[, cols_sector_breakout]
  
  
  sector_table_df <- sector_breakout_df %>% rbind(total_gross_emissions_df) %>% rbind(lulucf_sink_breakout) %>% rbind(total_net_emissions)
  
}







