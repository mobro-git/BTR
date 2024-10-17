
# Add percent change column compared to 2005 GHG Emissions Levels

add_pct_change_05 <- function(dataframe) {
  
  dataframe <- dataframe %>%
    group_by(category) %>%
    mutate(pct_change_05 = round((cat_sum/cat_sum[year==2005]-1),2)) %>% 
    ungroup()
  
}


create_pct_change_table <- function(category, group, projections_all_sm, config, settings) {
  
  col_order <- c('category','year','low','high')

  summary <- projections_all_sm %>%
    rename(category = .data[[category]]) %>% 
    filter(grouping %in% c(group, 'ghgi')) %>%
    filter(year %in% config$table) %>% 
    group_by(proj_name,category, year) %>%
    summarise(cat_sum = sum(sum),.groups='drop') %>% 
    add_pct_change_05() %>%
    select(!cat_sum) 

  
  summary_total_gross <- projections_all_sm %>%
    rename(category = .data[[category]]) %>% 
    filter(grouping %in% c(group, 'ghgi')) %>%
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
    rename(category = .data[[category]]) %>% 
    filter(grouping %in% c(group, 'ghgi')) %>%
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
    order_index <- c(2,1,4,3,6,7,5)
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
    order_index <- c(2,4,3,1,5)
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
    pct_change_table_df$category <- ifelse(pct_change_table_df$category == 'IPPU', 'Industrial Processes', pct_change_table_df$category) # Change 'IPPU' to 'Industrial Processes'
    
    
  }
  else{rlang::abort('Enter "gas" or "usproj_sector" into as category.')}
  
  
  pct_change_table_df_high_low <- create_high_low_df(pct_change_table_df, settings)
  pct_change_table_df_high_low <- pct_change_table_df_high_low[order_index,]
  
  
  total_gross_emissions_high_low <- create_high_low_df(summary_total_gross, settings)
  total_net_emissions_high_low <- create_high_low_df(summary_total_net, settings)
  
  
  summary_lulucf_sink <- summary %>% filter(category == 'LULUCF Sink') %>%
    group_by(year) %>%
    mutate(low = min(pct_change_05),
           high = max(pct_change_05)) %>%
    select(category, year, low, high) %>%
    distinct()
  
  lulucf_sink_high_low <- create_high_low_df(summary_lulucf_sink, settings)
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
    rbind(total_net_emissions_high_low)# %>% select(category,all_of(proj_col_order))
  
  rownames(final_summary_table) <- NULL

  
  
  final_summary_table_100 <- final_summary_table %>% mutate_if(is.numeric, ~ .*100)
  
  
  return(final_summary_table_100)
  

}

create_html_table_merged_pct_change <- function(final_summary_table, stubhead, settings){
  
  hist_years <- c('2005','2010','2015','2020', settings$base_year)
  proj_col_order <- c('2025',
                      '2030',
                      '2035',
                      '2040')
  
  final_summary_table_chr <- final_summary_table %>% mutate_if(is.numeric, as.character)
  
  merged = final_summary_table_chr %>%
    mutate(
      `2025` = case_when(
        `2025_low` == `2025_high` ~ `2025_high`,
        `2025_low` != `2025_high` ~ paste0(`2025_low`," to ",`2025_high`)),
      `2030` = case_when(
        `2030_low` == `2030_high` ~ `2030_high`,
        `2030_low` != `2030_high` ~ paste0(`2030_low`," to ",`2030_high`)),
      `2035` = case_when(
        `2035_low` == `2035_high` ~ `2035_high`,
        `2035_low` != `2035_high` ~ paste0(`2035_low`," to ",`2035_high`)),
      `2040` = case_when(
        `2040_low` == `2040_high` ~ `2040_high`,
        `2040_low` != `2040_high` ~ paste0(`2040_low`," to ",`2040_high`))
    ) %>%
    select(-contains("_"))
  
  bold1 = nrow(merged)-3
  bold2 = nrow(merged)-1
  
  html_table <- merged  %>%
    rename(!!stubhead := category) %>%
    
    gt() %>%
    text_transform(locations = cells_body(columns = 1),
                   fn = function(x) {
                     subscript_numbers(x)
                   }) %>% 
    
    cols_align('center', columns = everything()) %>%
    cols_align('left', columns = stubhead) %>% 
    tab_spanner(label = "Historical ", columns = all_of(hist_years)) %>%
    tab_spanner(label = "Projections", columns = all_of(proj_col_order)) %>%
    tab_style(style = cell_borders(color = "#1F77AE", sides = c("bottom"),  weight = px(2)), #
              locations = cells_body(rows = c(bold1,bold2))) %>%

    gt_theme_nc_blue()
  
  html_table
  
}
