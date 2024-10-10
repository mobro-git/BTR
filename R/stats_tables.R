section_pct_change_05 <- function(data_long_clean, var, config) {
  
  df <- data_long_clean %>%
    filter(variable == var,
           scenario == 'wm') %>%
    select(model,year,value) %>%
    mutate(pct_change_05 = round((value/value[year==2005]-1),2),
           value = round(value,2)) %>%
    filter(year %in% config$table) %>%
    select(model, year, pct_change_05)
  
  df <- df[order(df$year),]
  
  df_wide <- df %>% pivot_wider(names_from = 'year',
                                values_from = 'pct_change_05')
  
  return(df_wide)
}

create_stats_table <- function(df) {
  
  table <- gt(df) %>% fmt_percent(columns = 2:ncol(df),
                                  decimals = 0) %>%
    sub_missing(missing_text = "-") %>% 
    cols_align(align = 'center') %>%
    cols_align(align = "left",
               columns = names(df)[1])
  return(table)
}

zev_share_table <- function(data_long_clean, ice_var) {
  df <- data_long_clean %>% 
    filter(variable == ice_var,
           scenario == "wm",
           year %in% c(2030,2035,2040)) %>%
    mutate(value = round(1-value,2)) %>% 
    select(model,year,value) %>%
    distinct()
  
  df <- df[order(df$year),]
  
  df_wide <- df %>% pivot_wider(names_from = 'year',
                                values_from = 'value')
  
  table <- gt(df_wide) %>% fmt_percent(columns = 2:4,
                                       decimals = 0) 
  return(table)
}
