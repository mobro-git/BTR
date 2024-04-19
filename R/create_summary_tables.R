
group_gas_breakout_dataset <- function(projections_all_sm, config) {
  
  years_sum <- projections_all_sm %>% filter(year %in% config$table)
  gas_dataset <- years_sum %>%
    group_by(proj_name, gas, year) %>%
    summarise(mmtco2e = sum(sum)) %>%
    filter(!gas=='LULUCF Sink')
  
  #gas_dataset_wide <- gas_dataset %>% pivot_wider(names_from = year,
  #                                        values_from = mmtco2e) %>% 
   # arrange(factor(gas, levels = config$gas_order))
  }




gen_gas_breakout <- function(dataset, config, category_order) {
  
  
  processed_dfs <- list()
  
  years_proj <- as.character(config$table[config$table > config$base_year])
  years_hist <- as.character(config$table[config$table <= config$base_year])
  
  
  for(value in category_order) {
    
    subset_df <- dataset %>% filter(gas == value)
    
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
  return(breakout_df)
  
}

###################################

group_sector_breakout_dataset <- function(projections_all_sm, config) {
  
  years_sum <- projections_all_sm %>% filter(year %in% config$table)
  sector_dataset <- years_sum %>% group_by(proj_name, usproj_sector, year) %>% summarise(mmtco2e = sum(sum)) %>% filter(!usproj_sector=='LULUCF Sink')
  
#   sector_dataset_wide <- sector_dataset %>% pivot_wider(names_from = year,
#                                           values_from = mmtco2e)%>% 
#     arrange(factor(usproj_sector, levels = config$sector_order))
}

gen_sector_breakout_df <- function(dataset, config, category_order) {
  
  
  processed_dfs <- list()
  
  years_proj <- as.character(config$table[config$table > config$base_year])
  years_hist <- as.character(config$table[config$table <= config$base_year])
  
  
  for(value in category_order) {
    
    subset_df <- dataset %>% filter(usproj_sector == value)
    
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
  return(breakout_df)
}
###########################################################

lulucf_sink_breakout <- function(projections_all_sm, config){
  
  lulucf_sink_df <- projections_all_sm %>% ungroup() %>% 
    select(proj_name,gas,year,sum) %>% 
    filter(gas=='LULUCF Sink') %>%
    filter(year %in% config$table)
  
  years_lulucf_sink <- as.character(unique(lulucf_sink_df$year))
  lulucf_sink_hist <- lulucf_sink_df %>% filter(year <= config$base_year)
  years_lulucf_sink_hist <- as.character(unique(lulucf_sink_hist$year))
  
  
  ghgi_subset <-
    lulucf_sink_df %>% filter(proj_name == 'ghgi') %>% filter(year <= config$base_year)
  proj_subset <-
    lulucf_sink_df %>% filter(!proj_name == 'ghgi') %>% filter(year > config$base_year)
  
  ghgi_subset_wide <- ghgi_subset %>% pivot_wider(names_from = year,
                                                  values_from = sum) %>% ungroup() %>% select(years_lulucf_sink_hist)
  
  proj_subset_wide <-
    proj_subset %>% pivot_wider(names_from = year,
                                values_from = sum)
  ghgi_subset_wide_rep <-
    do.call('rbind', replicate(nrow(proj_subset_wide), ghgi_subset_wide, simplify =
                                 FALSE))
  
  value_df <- cbind(proj_subset_wide, ghgi_subset_wide_rep) %>% select(proj_name, gas, years_lulucf_sink)
  
  
}



###########################################################








summarise_emissions <- function(breakout_df){
  
  summarized_df <- breakout_df %>% group_by(proj_name) %>% 
    summarise(across(where(is.numeric), sum, na.rm=TRUE))
  
  
}


total_net_emissions <- function(gas_breakout_df,lulucf_sink_breakout_df){
  
  cols_gas_breakout <- colnames(gas_breakout_df)
  
  missing_cols <- setdiff(cols_gas_breakout, colnames(lulucf_sink_breakout_df))
  lulucf_sink_breakout_df[missing_cols] <- NA
  
  lulucf_sink_breakout_df <- lulucf_sink_breakout_df[, cols_gas_breakout]
  
  combined_df <- bind_rows(gas_breakout_df, lulucf_sink_breakout_df)
  
}








