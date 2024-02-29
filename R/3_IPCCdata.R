
make_ipcc_data = function(ratio_var, per_diff_var) {

  # IPCC_small.csv created in scripts/IPCC_AR6.R
  ipcc_small = read_csv("data-raw/extra-data/IPCC_small.csv")

  fig_vars = c("Final Energy|Percent Electricity",
               "Final Energy|Electricity|Percent of 2015",
               "Primary Energy|Fossil|Percent difference from 2015",
               "Emissions|CO2|Percent difference from 2015")

  ratio = make_ratio_variables(ipcc_small, ratio_var) %>%
    filter(variable %in% fig_vars &
             year == 2050 &
             !is.na(value),
           value <= 1 # 2 values are ~9 and ~11, not sure whats up there but obviously incorrect
           ) %>%
    mutate(value = value * 100) # ratio_var to percent

  per_diff = make_per_diff_variables(ipcc_small, per_diff_var) %>%
    filter(variable %in% fig_vars &
             year == 2050 &
             !is.na(value))

  ipcc = rbind(ratio, per_diff) %>%
    mutate(Region = 'US: IPCC AR6',
           `CO2 Target` = "Net Zero")

  ipcc_a = ipcc %>%
    filter(variable %in% fig_vars[1:2]) %>%
    select(`CO2 Target`, Region, model, scenario, variable, value) %>%
    pivot_wider(names_from = "variable", values_from = "value") %>%
    rename(x = `Final Energy|Percent Electricity`,
           y = `Final Energy|Electricity|Percent of 2015`) %>%
    select(-model, -scenario) %>%
    mutate(source = "Literature")

  ipcc_b = ipcc %>%
    filter(variable %in% fig_vars[3:4]) %>%
    select(`CO2 Target`, Region, model, scenario, variable, value) %>%
    pivot_wider(names_from = "variable", values_from = "value") %>%
    rename(x = `Primary Energy|Fossil|Percent difference from 2015`,
           y = `Emissions|CO2|Percent difference from 2015`) %>%
    select(-model, -scenario) %>%
    mutate(source = "Literature")

  list(ipcc_a = ipcc_a,
       ipcc_b = ipcc_b)

}
