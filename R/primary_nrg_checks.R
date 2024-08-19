
check_primary_nrg_df <- function(data_long_clean, config) {
  
  primarynrg_total = data_long_clean %>%
    filter(
      variable == "Primary Energy" &
        model %in% config$model_wm &
        scenario == "wm" & year %in% c(2020, 2025, 2030, 2035, 2040)
    ) %>%
    mutate(variable = "Total")
  
  primarynrg_breakouts = data_long_clean %>%
    filter(
      variable %in% c(
        "Primary Energy|Biomass",
        "Primary Energy|Coal",
        "Primary Energy|Gas",
        "Primary Energy|Geothermal",
        "Primary Energy|Hydro",
        "Primary Energy|Nuclear",
        "Primary Energy|Oil",
        "Primary Energy|Other",
        "Primary Energy|Solar",
        "Primary Energy|Wind"
      ) &
        model %in% config$model_wm &
        scenario == "wm" & year %in% c(2020, 2025, 2030, 2035, 2040)
    ) %>%
    mutate(variable = str_remove(variable, "Primary Energy\\|")) %>%
    rbind(primarynrg_total) %>%
    mutate(variable = factor(variable, levels = unique(variable)))
}
 
check_primary_nrg_fig <- function(check_primary_nrg_df) { 
  plot = ggplot() +
    geom_line(data = check_primary_nrg_df, aes(
      x = year,
      y = value,
      color = model,
      group = model
    )) +
    facet_grid(variable ~ scenario, scales = "free_y")
  plot
}


