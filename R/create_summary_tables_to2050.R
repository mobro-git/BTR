
## Breakout dataframes - create each part of the final table separately: Gas or Sector DF, Total Gross, LULUCF Sink, Total Net
## ..then combine to create final table
##################

gen_lulucf_sink_breakout50 <- function(projections_all_sm, config, settings){
  
  lulucf_sink_df <- projections_all_sm %>%
    #ungroup() %>% 
    select(proj_name,gas,year,sum) %>% 
    filter(gas=='LULUCF Sink') %>%
    filter(year %in% config$table50)

  years_lulucf_sink <- as.character(sort(unique(lulucf_sink_df$year)))
  lulucf_sink_hist <- lulucf_sink_df %>% filter(year <= settings$base_year)
  years_lulucf_sink_hist <- as.character(unique(lulucf_sink_hist$year))
  
  ghgi_subset <-
    lulucf_sink_df %>%
    filter(proj_name == 'ghgi') %>%
    filter(year <= settings$base_year)
  
  proj_subset <-
    lulucf_sink_df %>%
    filter(!proj_name == 'ghgi') %>%
    filter(year > settings$base_year)
  
  ghgi_subset_wide <- ghgi_subset %>%
    pivot_wider(names_from = year, values_from = sum) %>%
    select(all_of(years_lulucf_sink_hist))
  
  proj_subset_wide <- proj_subset %>%
    pivot_wider(names_from = year, values_from = sum)
  
  ghgi_subset_wide_rep <-
    do.call('rbind', replicate(nrow(proj_subset_wide), ghgi_subset_wide, simplify = FALSE))
  
  lulucf_sink_breakout <- cbind(proj_subset_wide, ghgi_subset_wide_rep) %>% 
    select(proj_name, gas, all_of(years_lulucf_sink))
  
  return(lulucf_sink_breakout)
}


gen_gas_dataset <- function(projections_all_sm, config) {
  
  gas_dataset <- projections_all_sm %>%
    group_by(proj_name, gas, year) %>%
    summarise(mmtco2e = sum(sum),.groups='drop') %>% 
    filter(!gas=='LULUCF Sink')
  
}

gen_gas_breakout50 <- function(gas_dataset, config, settings, category_order) {
  
  gas_dataset <- gas_dataset %>%
    filter(year %in% config$table50)
  
  processed_dfs <- list()
  
  years_proj <- as.character(config$table50[config$table50 > settings$base_year])
  years_hist <- as.character(config$table50[config$table50 <= settings$base_year])
  
  
  for(value in category_order) {
    
    subset_df <- gas_dataset %>% filter(gas == value)
    
    ghgi_subset <-
      subset_df %>% filter(proj_name == 'ghgi') %>% filter(year <= settings$base_year)
    proj_subset <-
      subset_df %>% filter(!proj_name == 'ghgi') %>% filter(year > settings$base_year)
    
    ghgi_subset_wide <- ghgi_subset %>% pivot_wider(names_from = year,
                                                    values_from = mmtco2e) %>% ungroup() %>% select(years_hist)
    
    proj_subset_wide <-
      proj_subset %>% pivot_wider(names_from = year,
                                  values_from = mmtco2e)
    ghgi_subset_wide_rep <-
      do.call('rbind', replicate(nrow(proj_subset_wide), ghgi_subset_wide, simplify =
                                   FALSE))
    
    value_df <- cbind(proj_subset_wide, ghgi_subset_wide_rep) %>% select(proj_name, gas,   as.character(config$table50))
    
    processed_dfs[[value]] <- value_df
    
    
  }
  breakout_df <- bind_rows(processed_dfs)
  return(breakout_df)
  
}

###################################

gen_sector_dataset50 <- function(projections_all_sm, config) {
  
  years_sum <- projections_all_sm %>% filter(year %in% config$table50)
  sector_dataset <- years_sum %>%
    group_by(proj_name, usproj_sector, year) %>%
    summarise(mmtco2e = sum(sum),.groups='drop') %>% 
    filter(!usproj_sector=='LULUCF Sink')
  
}

gen_sector_breakout50 <- function(sector_dataset, config, settings, category_order) {
  
  
  processed_dfs <- list()
  
  years_proj <- as.character(config$table50[config$table50 > settings$base_year])
  years_hist <- as.character(config$table50[config$table50 <= settings$base_year])
  
  
  for(value in category_order) {
    
    subset_df <- sector_dataset %>% filter(usproj_sector == value)
    
    ghgi_subset <-
      subset_df %>% filter(proj_name == 'ghgi') %>% filter(year <= settings$base_year)
    proj_subset <-
      subset_df %>% filter(!proj_name == 'ghgi') %>% filter(year > settings$base_year)
    
    ghgi_subset_wide <- ghgi_subset %>% pivot_wider(names_from = year,
                                                    values_from = mmtco2e) %>% ungroup() %>% select(years_hist)
    
    proj_subset_wide <-
      proj_subset %>% pivot_wider(names_from = year,
                                  values_from = mmtco2e)
    ghgi_subset_wide_rep <-
      do.call('rbind', replicate(nrow(proj_subset_wide), ghgi_subset_wide, simplify =
                                   FALSE))
    
    value_df <- cbind(proj_subset_wide, ghgi_subset_wide_rep) %>% select(proj_name, usproj_sector,   as.character(config$table50))
    
    processed_dfs[[value]] <- value_df
    
    
  }
  sector_breakout <- bind_rows(processed_dfs)
  
  return(sector_breakout)
}



###########################################################



gen_total_gross_emissions <- function(gas_dataset,config){
  
  summarized_df <- gas_dataset %>%
    filter(year %in% config$kaya) %>%
    group_by(proj_name, year) %>% 
    summarise(value = sum(mmtco2e),.groups='drop') %>% 
    mutate(source = 'Total Gross Emissions') %>% 
    select(proj_name,source, everything())
}


gen_total_net_emissions <- function(gas_dataset,lulucf_sink_breakout,settings,config){
  
  cols_gas_breakout <- colnames(gas_dataset)
  
  # Add NA columns missing in LULUCF that are present in projections
  missing_cols <- setdiff(cols_gas_breakout, colnames(lulucf_sink_breakout))
  lulucf_sink_breakout[missing_cols] <- NA
  
  lulucf_sink_breakout <- lulucf_sink_breakout[, cols_gas_breakout]
  
  combined_df <- bind_rows(gas_dataset, lulucf_sink_breakout)
  
  tne_df <- combined_df %>%
    gen_total_gross_emissions(config) %>% 
    mutate(source = 'Total Net Emissions') %>% 
    select(proj_name,source, everything())
  
  write_csv(tne_df, paste0('output/',settings$version,'/proj_tables/total_net_emissions.csv'))
  

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

#################################################################

## Create HTML tables

create_high_low_df50 <- function(summary_subset, settings){
  # Function to create low/high year columns from table row subsets
  
  proj_years <- c('2025','2030','2035','2040','2045','2050')
  hist_years <- c('2005','2010','2015','2020', settings$base_year)
  proj_col_order <- c('2025_low',
                      '2025_high',
                      '2030_low',
                      '2030_high',
                      '2035_low',
                      '2035_high',
                      '2040_low',
                      '2040_high',
                      '2045_low',
                      '2045_high',
                      '2050_low',
                      '2050_high')
  
  df_low <- summary_subset %>% 
    select(category,year,low) %>%
    pivot_wider(names_from = 'year',
                values_from = 'low')
  
  
  df_high <- summary_subset %>% 
    select(category,year,high) %>%
    pivot_wider(names_from = 'year',
                values_from = 'high')
  
  
  
  
  df_merged <- merge(df_low[, c('category',proj_years)],df_high[, c('category',proj_years)], by = 'category', suffix = c('','_high'))
  
  for(year in proj_years) {
    colnames(df_merged)[colnames(df_merged)==year] <- paste0(year,'_low')
    colnames(df_merged)[colnames(df_merged) == paste0(year,'_high')] <- paste0(year,'_high')
  }
  
  df_merge_final <- merge(df_low %>% select(category, all_of(hist_years)), df_merged) %>%
    select(category, all_of(hist_years), all_of(proj_col_order))
  
}

##


subscript_numbers <- function(string) {
  gsub("([0-9]+)","<sub>\\1</sub>",string,perl = TRUE)
}


create_summary_table50 <- function(category, group, projections_all_sm, config, settings) {
  
  col_order <- c('category','year','low','high')
  
  
  summary <- projections_all_sm %>%
    rename(category = .data[[category]]) %>% 
    filter(grouping %in% c(group, 'ghgi')) %>%
    filter(year %in% config$table50) %>% 
    group_by(proj_name,category, year) %>%
    summarise(cat_sum = sum(sum),.groups='drop')
  
  summary_total_gross <- summary %>%
    filter(!category == 'LULUCF Sink') %>%
    group_by(proj_name, year) %>%
    summarise(value = sum(cat_sum),.groups='drop') %>%
    group_by(year) %>%
    mutate(low = min(value),
           high = max(value)) %>% 
    select(year,low,high) %>% 
    distinct() %>%
    mutate(category = 'Total Gross Emissions') %>% 
    select(all_of(col_order))
  
  
  summary_total_net <- summary %>%
    group_by(proj_name, year) %>%
    summarise(value = sum(cat_sum),.groups='drop') %>%
    group_by(year) %>%
    mutate(low = min(value),
           high = max(value)) %>%
    select(year,low,high) %>% 
    distinct() %>%
    mutate(category = 'Total Net Emissions') %>% 
    select(all_of(col_order))
  
  
  
  if(category == 'gas'){
    order_index <- c(2,1,4,3,6,7,5) # added NF3
    processed_datasets <- list()
    for(gas in config$gas_order){
      
      cat_summary <- summary %>%
        filter(category == gas) %>%
        group_by(proj_name, year) %>%
        summarise(value = sum(cat_sum),.groups='drop') %>%
        group_by(year) %>%
        mutate(low = min(value),
               high = max(value)) %>%
        select(year, low, high) %>%
        distinct() %>%
        mutate(category = gas) %>% 
        select(all_of(col_order))
      
      processed_datasets[[gas]] <- cat_summary
    }
    summary_table_df <- bind_rows(processed_datasets)
    #summary_table_df$category <- subscript_numbers(summary_table_df$category)
    
    
  } else if(category == 'usproj_sector'){
    processed_datasets <- list()
    order_index <- c(2,4,3,1,5)
    for(sector in config$sector_order){
      
      cat_summary <- summary %>%
        filter(category == sector) %>%
        group_by(proj_name, year) %>%
        summarise(value = sum(cat_sum),.groups='drop') %>%
        group_by(year) %>%
        mutate(low = min(value),
               high = max(value)) %>%
        select(year, low, high) %>%
        distinct() %>%
        mutate(category = sector) %>% 
        select(all_of(col_order))
      
      processed_datasets[[sector]] <- cat_summary
    }
    summary_table_df <- bind_rows(processed_datasets)
    summary_table_df$category <- ifelse(summary_table_df$category == 'IPPU', 'Industrial Processes', summary_table_df$category) # Change 'IPPU' to 'Industrial Processes'
    
    
    
  }
  else{rlang::abort('Enter "gas" or "usproj_sector" into as category.')}
  
  summary_table_df_high_low <- create_high_low_df50(summary_table_df, settings)
  summary_table_df_high_low <- summary_table_df_high_low[order_index,]
  
  
  total_gross_emissions_high_low <- create_high_low_df50(summary_total_gross, settings)
  total_net_emissions_high_low <- create_high_low_df50(summary_total_net, settings)
  
  
  summary_lulucf_sink <- summary %>% filter(category == 'LULUCF Sink')%>%
    group_by(category,proj_name, year) %>%
    summarise(value = sum(cat_sum),.groups='drop') %>%
    group_by(year) %>%
    mutate(low = min(value),
           high = max(value)) %>%
    select(category, year, low, high) %>%
    distinct()
  
  lulucf_sink_high_low <- create_high_low_df50(summary_lulucf_sink, settings)
  
  final_summary_table <- summary_table_df_high_low %>%
    rbind(total_gross_emissions_high_low) %>% 
    rbind(lulucf_sink_high_low) %>% 
    rbind(total_net_emissions_high_low)
  
  rownames(final_summary_table) <- NULL
  
  final_summary_table <- final_summary_table %>% mutate_if(is.numeric, round)
  
  numeric_cols <- sapply(final_summary_table,is.numeric)
  final_summary_table[, numeric_cols] <- lapply(final_summary_table[, numeric_cols], scales::comma)
  
  final_summary_table[] <- lapply(final_summary_table, function(x) gsub("-","(", x))
  final_summary_table[] <- lapply(final_summary_table, function(x) ifelse(grepl("\\(", x), paste0(x, ")"), x))
  
  return(final_summary_table)
}

#######################################################

create_html_table50 <- function(final_summary_table, stubhead, settings){
  
  hist_years <- c('2005','2010','2015','2020', settings$base_year)
  proj_col_order <- c('2025_low',
                      '2025_high',
                      '2030_low',
                      '2030_high',
                      '2035_low',
                      '2035_high',
                      '2040_low',
                      '2040_high',
                      '2045_low',
                      '2045_high',
                      '2050_low',
                      '2050_high')
  
#  table_title <- paste0('Historical and Projected U.S. GHG Emissions (2023 Policy Baseline), by ',stubhead,': 2005-2040 (MMTCO<sub>2</sub>e)')
  
  html_table <-  final_summary_table  %>%
    rename(!!stubhead := category) %>%
    
    gt() %>%
    text_transform(locations = cells_body(columns = 1),
                   fn = function(x) {
                     subscript_numbers(x)
                   }) %>% 
    
    tab_spanner(label = "2025", columns = proj_col_order[1:2]) %>%
    tab_spanner(label = "2030", columns = proj_col_order[3:4]) %>%
    tab_spanner(label = "2035", columns = proj_col_order[5:6]) %>%
    tab_spanner(label = "2040", columns = proj_col_order[7:8]) %>%
    tab_spanner(label = "2045", columns = proj_col_order[9:10]) %>%
    tab_spanner(label = "2050", columns = proj_col_order[11:12]) %>%
    cols_label(`2025_low` = 'Low') %>%
    cols_label(`2030_low` = 'Low') %>% 
    cols_label(`2035_low` = 'Low') %>% 
    cols_label(`2040_low` = 'Low') %>%
    cols_label(`2045_low` = 'Low') %>%
    cols_label(`2050_low` = 'Low') %>%
    
    
    cols_label(`2025_high` = 'High') %>%
    cols_label(`2030_high` = 'High') %>% 
    cols_label(`2035_high` = 'High') %>% 
    cols_label(`2040_high` = 'High') %>%
    cols_label(`2045_high` = 'High') %>% 
    cols_label(`2050_high` = 'High') %>% 
    
    cols_align('center', columns = everything()) %>%
    cols_align('left', columns = stubhead) %>% 
    
    tab_spanner(label = "Historical ", columns = all_of(hist_years), level = 2) %>%
    tab_spanner(label = "Projected", columns = all_of(proj_col_order)) %>%
   # tab_header(title = gt::html(table_title)) %>%
    
    gt_theme_nc_blue()
  
}

create_html_table_merged50 <- function(final_summary_table, stubhead, settings){
  
  hist_years <- c('2005','2010','2015','2020', settings$base_year)
  proj_col_order <- c('2025',
                      '2030',
                      '2035',
                      '2040',
                      '2045',
                      '2050')
  
  merged = final_summary_table %>%
    mutate(
      `2025` = case_when(
        `2025_low` == `2025_high` ~ `2025_high`,
        `2025_low` != `2025_high` ~ paste0(`2025_low`," - ",`2025_high`)),
      `2030` = case_when(
        `2030_low` == `2030_high` ~ `2030_high`,
        `2030_low` != `2030_high` ~ paste0(`2030_low`," - ",`2030_high`)),
      `2035` = case_when(
        `2035_low` == `2035_high` ~ `2035_high`,
        `2035_low` != `2035_high` ~ paste0(`2035_low`," - ",`2035_high`)),
      `2040` = case_when(
        `2040_low` == `2040_high` ~ `2040_high`,
        `2040_low` != `2040_high` ~ paste0(`2040_low`," - ",`2040_high`)),
      `2045` = case_when(
        `2045_low` == `2045_high` ~ `2045_high`,
        `2045_low` != `2045_high` ~ paste0(`2045_low`," - ",`2045_high`)),
      `2050` = case_when(
        `2050_low` == `2050_high` ~ `2050_high`,
        `2050_low` != `2050_high` ~ paste0(`2050_low`," - ",`2050_high`))
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
    tab_spanner(label = "Projected", columns = all_of(proj_col_order)) %>%
    tab_style(style = cell_borders(color = "#1F77AE", sides = c("bottom"),  weight = px(2)), #
              locations = cells_body(rows = c(bold1,bold2))) %>%

    gt_theme_nc_blue()
  
  html_table
  
}


