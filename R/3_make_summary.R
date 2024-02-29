
make_summary_long <- function(data_long, variable_info_lookup) {

  summary_long <- data_long %>%
    mutate(var = def_from_variable(variable, "var", variable_info_lookup)) %>%
    filter(def_from_variable(variable, "summary_flag", variable_info_lookup)) %>% # filter keeps the TRUE values
    relocate_standard_col_order() %>%
    arrange_standard()

}

make_summary <- function(summary_long, variable_info_lookup) {

  summary_long <- summary_long %>%
    filter(!is.na(var)) %>%
    filter(!is.na(value)) %>%
    group_by(
      model, scenario, region, var, year, unit
    ) %>%
    summarize(value = sum(value, na.rm = TRUE)) %>% # should only be one value b/c pulls summary variables, might be unnecessary? maybe summing over subnational?
    ungroup() %>%
    select(-unit) %>%
    pivot_wider(
      names_from = var,
      values_from = value
    ) %>%
    arrange_standard()

}


