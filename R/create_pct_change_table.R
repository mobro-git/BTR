
# Add percent change column compared to 2005 GHG Emissions Levels

add_pct_change_05 <- function(dataframe) {
  
  dataframe <- dataframe %>%
    group_by(category) %>%
    mutate(pct_change_05 = round((cat_sum/cat_sum[year==2005]-1),2)) %>% 
    ungroup()
  
}


create_pct_change_table <- function(category, grouping, projections_all_sm, config) {
  
  col_order <- c('category','year','low','high')

  summary <- projections_all_sm %>%
    filter(!gas == 'Total') %>%  # WTH?? fix pls usproj TODO
    rename(category = .data[[category]]) %>% 
    filter(grouping == grouping) %>%
    filter(year %in% config$table) %>% 
    group_by(proj_name,category, year) %>%
    summarise(cat_sum = sum(sum),.groups='drop') %>% 
    add_pct_change_05() %>%
    select(!cat_sum) 

  
  summary_total_gross <- projections_all_sm %>%
    filter(!gas == 'Total') %>%  # WTH?? fix pls usproj
    rename(category = .data[[category]]) %>% 
    filter(grouping == grouping) %>%
    filter(year %in% config$table) %>% 
    group_by(proj_name,category, year) %>%
    summarise(cat_sum = sum(sum),.groups='drop') %>%
    filter(!category == 'LULUCF Sink') %>%
    group_by(proj_name,year) %>%
    summarise(cat_sum = sum(cat_sum),.groups='drop') %>%
    
    mutate(pct_change_05 = (cat_sum/cat_sum[year==2005]-1)) %>% 
    mutate(pct_change_05 = round(pct_change_05,2)) %>%
    
    group_by(year) %>%
    mutate(low = min(pct_change_05, na.rm=TRUE),
           high = max(pct_change_05, na.rm=TRUE)) %>%
    ungroup() %>%
    
    select(year,low,high) %>% 
    distinct() %>%
    mutate(category = 'Total Gross Emissions') %>%
    select(all_of(col_order))
  
  summary_total_net <- projections_all_sm %>%
    filter(!gas == 'Total') %>%  # WTH?? fix pls usproj
    rename(category = .data[[category]]) %>% 
    filter(grouping == grouping) %>%
    filter(year %in% config$table) %>% 
    group_by(proj_name,category, year) %>%
    summarise(cat_sum = sum(sum),.groups='drop') %>%
    group_by(proj_name,year) %>%
    summarise(cat_sum = sum(cat_sum),.groups='drop') %>%
    mutate(pct_change_05 = (cat_sum/cat_sum[year==2005]-1)) %>% 
    mutate(pct_change_05 = round(pct_change_05,2)) %>% 
    group_by(year) %>%
    mutate(low = min(pct_change_05, na.rm=TRUE),
           high = max(pct_change_05, na.rm=TRUE)) %>%
    ungroup() %>% 
    select(year,low,high) %>% 
    distinct() %>%
    mutate(category = 'Total Net Emissions') %>%
    select(all_of(col_order))
  
  
  
  
  if(category == 'gas'){
    order_index <- c(2,1,4,3,5,6)
    processed_datasets <- list()
    for(gas in config$gas_order){
      
      cat_pct_change <- summary %>%
        filter(category == gas) %>%
        group_by(year) %>%
        mutate(low = min(pct_change_05, na.rm=TRUE),
               high = max(pct_change_05, na.rm=TRUE)) %>%
        ungroup() %>% 
        select(year,low,high) %>%
        distinct() %>%
        mutate(category = gas) %>% 
        select(all_of(col_order))
      
      processed_datasets[[gas]] <- cat_pct_change
    }
    pct_change_table_df <- bind_rows(processed_datasets)
    
  } else if(category == 'usproj_sector'){
    processed_datasets <- list()
    order_index <- c(2,5,3,1,6,4)
    for(sector in config$sector_order){
      
      cat_pct_change <- summary %>%
        filter(category == sector) %>%
        group_by(year) %>%
        mutate(low = min(pct_change_05, na.rm=TRUE),
               high = max(pct_change_05, na.rm=TRUE)) %>%
        ungroup() %>% 
        select(year,low,high) %>%
        distinct() %>%
        mutate(category = sector) %>% 
        select(all_of(col_order))
      
      processed_datasets[[sector]] <- cat_pct_change
    }
    pct_change_table_df <- bind_rows(processed_datasets)
    
  }
  else{rlang::abort('Enter "gas" or "usproj_sector" into as category.')}
  
  
  pct_change_table_df_high_low <- create_high_low_df(pct_change_table_df, config)
  pct_change_table_df_high_low <- pct_change_table_df_high_low[order_index,]
  
  
  total_gross_emissions_high_low <- create_high_low_df(summary_total_gross, config)
  total_net_emissions_high_low <- create_high_low_df(summary_total_net, config)
  
  
  summary_lulucf_sink <- summary %>% filter(category == 'LULUCF Sink') %>%
    group_by(year) %>%
    mutate(low = min(pct_change_05),
           high = max(pct_change_05)) %>%
    select(category, year, low, high) %>%
    distinct()
  
  lulucf_sink_high_low <- create_high_low_df(summary_lulucf_sink, config)
  proj_col_order <- c('2025_low',
                      '2025_high',
                      '2030_low',
                      '2030_high',
                      '2035_low',
                      '2035_high',
                      '2040_low',
                      '2040_high')
  
  final_summary_table <- pct_change_table_df_high_low %>%
    rbind(total_gross_emissions_high_low) %>% 
    rbind(lulucf_sink_high_low) %>% 
    rbind(total_net_emissions_high_low) %>% select(category,all_of(proj_col_order))
  
  rownames(final_summary_table) <- NULL

  
  
  final_summary_table_100 <- final_summary_table %>% mutate_if(is.numeric, ~ .*100)
  
  
  return(final_summary_table_100)
  

}

# Create html table in the style of gt()
create_pct_change_html_table <- function(final_summary_table, stubhead, config){
  
  hist_years <- c('2005','2010','2015','2020', config$base_year)
  proj_col_order <- c('2025_low',
                      '2025_high',
                      '2030_low',
                      '2030_high',
                      '2035_low',
                      '2035_high',
                      '2040_low',
                      '2040_high')
  
  html_table <-  final_summary_table %>%
    rename(!!stubhead := category) %>% 
    gt() %>%
    tab_spanner(label = "2025", columns = proj_col_order[1:2]) %>%
    tab_spanner(label = "2030", columns = proj_col_order[3:4]) %>%
    tab_spanner(label = "2035", columns = proj_col_order[5:6]) %>%
    tab_spanner(label = "2040", columns = proj_col_order[7:8]) %>%
    cols_label(`2025_low` = 'Low') %>%
    cols_label(`2030_low` = 'Low') %>% 
    cols_label(`2035_low` = 'Low') %>% 
    cols_label(`2040_low` = 'Low') %>%
    
    cols_label(`2025_high` = 'High') %>%
    cols_label(`2030_high` = 'High') %>% 
    cols_label(`2035_high` = 'High') %>% 
    cols_label(`2040_high` = 'High') %>%
    cols_align('center', columns = everything()) %>%
    cols_align('left', columns = stubhead) %>% 
    
    
    #tab_spanner(label = "Historical ", columns = all_of(hist_years), level = 2) %>%
    tab_spanner(label = "Projected", columns = all_of(proj_col_order))%>%
    tab_header(title = paste0('Projected Percent Change U.S. GHG Emissions Compared to 2005 Levels (2023 Policy Baseline), by ',stubhead,': 2025-2040 (%)')) %>%
    gt_theme_nc_blue()
  
  
  
}
