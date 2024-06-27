# Functions to create values for in-text statistics used in docs/BTR/results_overview.Rmd

get_figure_subset_df <- function(figmap_df, fig_num) {
  figure_subset_df <- figmap_df %>%
    filter(figure_no == fig_num)
}

get_pct_change_40_df <- function(data_long_clean, var_list, config){
  
  df <- data_long_clean %>%
    filter(variable %in% var_list) %>%
    filter(scenario == 'wm') %>%
    filter(region == 'United States') %>%
    group_by(model,year) %>%
    summarise(value = sum(value), .groups = 'drop') %>%
    filter(year %in% config$fives_sumtab) %>%
    complete(model, year = config$fives_proj) %>%
    group_by(model) %>%
    mutate(pct_change_40 = value[year==2040]/value[year==2020]) %>%
    mutate(pct_change_40 = (pct_change_40-1)*100) %>%
    mutate(pct_change_40 = round(pct_change_40)) %>%
    select(model, pct_change_40) %>%
    distinct()
  
}

get_pct_change_40_value <- function(pct_change_40_df, model_name) {
  model_df <- pct_change_40_df %>% filter(model == model_name)
  value <- model_df$pct_change_40
}

get_pct_change_40_value_list <- function(pct_change_40_df) {
  models <- unique(pct_change_40_df$model)
  value_list <- list()
  
  for(name in models) {
    value <- get_pct_change_40_value(pct_change_40_df, name)
    value_list[name] <- value
  }
  return(value_list)
}

get_value_list_from_var_list <- function(figmap_df, figure_num, var_list, data_long_clean, config) {
  figure_subset_df <- get_figure_subset_df(figmap_df, figure_num)
  pct_change_40_df <- get_pct_change_40_df(data_long_clean, var_list, config)
  value_list <- get_pct_change_40_value_list(pct_change_40_df)
  return(value_list)
}