
var_year_comparison <- function(data_from_graph, var_choice, comparison_year) {
  
  var_df <- data_from_graph %>%
    filter(variable == var_choice,
           year == comparison_year) %>%
    group_by(model,scenario,year,variable) %>%
    summarise(value = sum(value))
}

