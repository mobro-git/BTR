
gcam = read_csv("data-extra/gcam_05012024_CO2Emissions_raw.csv") %>%
  rename(unit = Units,
         variable = reporting.category) %>%
  mutate(unit = "Mt CO2/yr") %>%
  mutate(model = "GCAM") %>%
  select(model, scenario, region, variable, unit, year, value) %>%
  group_by(model,scenario,region,variable,unit,year) %>%
  summarise(value = sum(value), .groups = "drop")

gcam_allNRG = gcam %>%
  group_by(model,scenario,region,unit,year) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  mutate(variable = "Emissions|CO2|Energy") %>%
  select(names(gcam))

gcam_all = rbind(gcam, gcam_allNRG)  

write_csv(gcam_all, "data-raw/model-runs/gcam_05012024_CO2Emissions_raw_CONVERTED.csv")
