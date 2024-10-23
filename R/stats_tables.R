section_pct_change_05 <- function(data_long_clean, var, config, scens = "wm") {
  
  df <- data_long_clean %>%
    filter(variable == var,
           scenario %in% c('Historic',scens)) %>%
    select(model,scenario,year,value) %>%
    mutate(pct_change_05 = round((value/value[year==2005]-1),2),
           value = round(value,2)) %>%
    filter(year %in% config$table) %>%
    select(model, scenario, year, pct_change_05) %>%
    filter(!(model != "EPA-GHGI" & year %in% c(2020,2022)))
  
  df <- df[order(df$year),]
  
  df_wide <- df %>% pivot_wider(names_from = 'year',
                                values_from = 'pct_change_05')
  
  df_stats = data.frame(model = c("min","max"), 
                        scenario = "stat",
                        "2005" = c(min(df_wide$`2005`,na.rm = TRUE), max(df_wide$`2005`,na.rm = TRUE)),
                        "2010" = c(min(df_wide$`2010`,na.rm = TRUE), max(df_wide$`2010`,na.rm = TRUE)),
                        "2015" = c(min(df_wide$`2015`,na.rm = TRUE), max(df_wide$`2015`,na.rm = TRUE)),
                        "2020" = c(min(df_wide$`2020`,na.rm = TRUE), max(df_wide$`2020`,na.rm = TRUE)),
                        "2022" = c(min(df_wide$`2022`,na.rm = TRUE), max(df_wide$`2022`,na.rm = TRUE)),
                        "2025" = c(min(df_wide$`2025`,na.rm = TRUE), max(df_wide$`2025`,na.rm = TRUE)),
                        "2030" = c(min(df_wide$`2030`,na.rm = TRUE), max(df_wide$`2030`,na.rm = TRUE)),
                        "2035" = c(min(df_wide$`2035`,na.rm = TRUE), max(df_wide$`2035`,na.rm = TRUE)),
                        "2040" = c(min(df_wide$`2040`,na.rm = TRUE), max(df_wide$`2040`,na.rm = TRUE)))
  names(df_stats) = names(df_wide)
  
  df_combine = rbind(df_stats,df_wide)

  return(df_combine)
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

zev_share_table <- function(data_long_clean, ice_var, scens) {
  df <- data_long_clean %>% 
    filter(variable == ice_var,
           scenario %in% scens,
           year %in% c(2030,2035,2040)) %>%
    mutate(value = round(1-value,2)) %>% 
    select(model,scenario,year,value) %>%
    distinct()
  
  df <- df[order(df$year),]
  
  df_wide <- df %>% pivot_wider(names_from = 'year',
                                values_from = 'value')
  
  df_stats = data.frame(model = c("min","max"), 
                        scenario = "stat",
                        "2030" = c(min(df_wide$`2030`,na.rm = TRUE), max(df_wide$`2030`,na.rm = TRUE)),
                        "2035" = c(min(df_wide$`2035`,na.rm = TRUE), max(df_wide$`2035`,na.rm = TRUE)),
                        "2040" = c(min(df_wide$`2040`,na.rm = TRUE), max(df_wide$`2040`,na.rm = TRUE)))
  names(df_stats) = names(df_wide)
  
  df_combine = rbind(df_stats,df_wide)
  
  table <- gt(df_combine) %>% fmt_percent(columns = 3:5,
                                       decimals = 0) 
  return(table)
}
