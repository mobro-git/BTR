
#' complete_implicit_na
#' fill in implicit NAs so differences can be calculated from unreported variables that are implicitly 0s
#' @param df
#'
#' @return

complete_implicit_na = function(df) {

  print("Completing implicit NAs")
  # model-scenario combinations to cross reference. only add back implicit missing data for model-scenario combinations submitted
  submitted = unique(df[c("model","scenario")])

  complete_all = df %>%
    group_by(model,unit,year,datasrc) %>%
    complete(scenario, variable, region)

  complete_zeros = complete_all %>%
    mutate(value = case_when(
      is.na(value) ~ 0,
      TRUE~value))

  # joins so that zeros are only kept for submitted model-scenario combinations
  complete_join = inner_join(submitted, complete_zeros, by = c("model","scenario"))

}

#' make_clean_data()
#' manual changes to data
#' @param df
#'
#' @return

make_clean_data = function(df) {

  # for interactive testing
  # tar_load(data_long)
  # df = data_long %>% unit_conversion() %>% complete_implicit_na()

  print("Making clean data")

  # sum industry and fuel production emissions for internal models
  ind_emissions = df %>% filter(model %in% c("USREP-ReEDS","GCAM-PNNL", "NEMS-OP")) %>%
    filter(variable %in% c("Emissions|CO2|Energy|Demand|Industry",
                           #"Emissions|CO2|Industrial Processes",
                           "Emissions|CO2|Energy|Supply|Biogas",
                           "Emissions|CO2|Energy|Supply|Biomass Liqids",
                           "Emissions|CO2|Energy|Supply|Gas",
                           "Emissions|CO2|Energy|Supply|Heat",
                           "Emissions|CO2|Energy|Supply|Hydrogen",
                           "Emissions|CO2|Energy|Supply|Petroleum Refining",
                           "Emissions|CO2|Energy|Supply|Synthetic Gas",
                           "Emissions|CO2|Energy|Supply|Synthetic Liquids"
                           )) %>%
    mutate(variable = "Emissions|CO2|Energy|Demand|Industry and Fuel Production") %>%
    group_by(scenario,model,region,unit,year,variable) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(datasrc = "calculated") %>%
    select(model,scenario,unit,year,datasrc,variable,region,value)

  ind_emissions_indirect = df %>% filter(model %in% c("USREP-ReEDS","GCAM-PNNL","NEMS-OP")) %>%
    filter(variable %in% c("Emissions|CO2|Energy|Demand|Industry|Indirect",
                           "Emissions|CO2|Energy|Supply|Indirect")) %>%
    select(model,scenario,unit,year,datasrc,variable,region,value) %>%
    mutate(variable = "Emissions|CO2|Energy|Demand|Industry and Fuel Production|Indirect") %>%
    group_by(scenario,model,region,unit,year,variable) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(datasrc = "calculated") %>%
    select(model,scenario,unit,year,datasrc,variable,region,value)

  ind_var = rbind(ind_emissions, ind_emissions_indirect)

  # sum nonco2 emissions for gcam
  gcampnnl_nonco2 = df %>%
    filter(model == "GCAM-PNNL" &
             variable %in% c("Emissions|CH4","Emissions|N2O","Emissions|F-Gases")) %>%
    group_by(scenario,model,region,unit,year,variable) %>%
    mutate(variable = "Emissions|Non-CO2 GHG") %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(datasrc = "calculated") %>%
    select(model,scenario,unit,year,datasrc,variable,region,value)

  # make aggregate Capacity Additions|Electricity|Storage variable for USREP-ReEDS
  usrep_storage = df %>%
    filter(model == "USREP-ReEDS" &
             variable %in% c("Capacity Additions|Electricity|Storage Capacity|Battery","Capacity Additions|Electricity|Storage Capacity|PSH")) %>%
    mutate(variable = "Capacity Additions|Electricity|Storage Capacity") %>%
    group_by(scenario,model,region,unit,year,variable) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(datasrc = "calculated") %>%
    select(model,scenario,unit,year,datasrc,variable,region,value)

  df_capadd = rbind(df, usrep_storage)

  # calculate average capacity additions 2021-2035 for internal models
  cap_add_avg = df_capadd %>%
    filter(model %in% c("USREP-ReEDS", "GCAM-PNNL", "NEMS-OP", "NEMS-EIA"),
           (grepl("Capacity Additions", variable) | grepl("Capacity Retirements", variable)),
           (year >= 2021 & year <= 2035)) %>%
    filter(!(variable %in% c("Capacity Additions|Electricity|Biomass", "Capacity Additions|Electricity|Biomass|w/ CCS", "Capacity Additions|Electricity|Biomass|w/o CCS",
                             "Capacity Additions|Electricity|Coal", "Capacity Additions|Electricity|Gas",
                             "Capacity Additions|Electricity|Geothermal",
                             "Capacity Additions|Electricity|Oil", "Capacity Additions|Electricity|Oil|w/ CCS", "Capacity Additions|Electricity|Oil|w/o CCS",
                             "Capacity Additions|Electricity|Solar|CSP","Capacity Additions|Electricity|Solar|PV",
                             "Capacity Additions|Electricity|Solar|PV|Rooftop","Capacity Additions|Electricity|Solar|PV|Utility-Scale",
                             "Capacity Additions|Electricity|Storage Capacity|Battery","Capacity Additions|Electricity|Storage Capacity|PSH",
                             "Capacity Additions|Electricity|Wind|Offshore","Capacity Additions|Electricity|Wind|Onshore",
                             "Capacity Additions|Electricity|Gas|CT|w/o CCS","Capacity Additions|Electricity|Gas|CT|w/ CCS", "Capacity Additions|Electricity|Gas|CT",
                             "Capacity Additions|Electricity|Gas|CC|w/o CCS","Capacity Additions|Electricity|Gas|CC|w/ CCS", "Capacity Additions|Electricity|Gas|CC",
                             "Capacity Additions|Electricity|Gas|ST|w/o CCS","Capacity Additions|Electricity|Gas|ST|w/ CCS", "Capacity Additions|Electricity|Gas|ST",
                             "Capacity Additions|Electricity"))) %>%
    filter(!(variable %in% c("Capacity Retirements|Electricity|Biomass", "Capacity Retirements|Electricity|Biomass|w/ CCS",
                             "Capacity Retirements|Electricity|Biomass|w/o CCS",
                             "Capacity Retirements|Electricity|Coal", "Capacity Retirements|Electricity|Gas",
                             "Capacity Retirements|Electricity|Geothermal",
                             "Capacity Retirements|Electricity|Oil", "Capacity Retirements|Electricity|Oil|w/ CCS", "Capacity Retirements|Electricity|Oil|w/o CCS",
                             "Capacity Retirements|Electricity|Solar|CSP","Capacity Retirements|Electricity|Solar|PV",
                             "Capacity Retirements|Electricity|Solar|PV|Rooftop","Capacity Retirements|Electricity|Solar|PV|Utility-Scale",
                             "Capacity Retirements|Electricity|Storage Capacity|Battery","Capacity Retirements|Electricity|Storage Capacity|PSH",
                             "Capacity Retirements|Electricity|Wind|Offshore","Capacity Retirements|Electricity|Wind|Onshore",
                             "Capacity Retirements|Electricity|Gas|CT|w/o CCS","Capacity Retirements|Electricity|Gas|CT|w/ CCS",
                             "Capacity Retirements|Electricity|Gas|CT",
                             "Capacity Retirements|Electricity|Gas|CC|w/o CCS","Capacity Retirements|Electricity|Gas|CC|w/ CCS",
                             "Capacity Retirements|Electricity|Gas|CC",
                             "Capacity Retirements|Electricity|Gas|ST|w/o CCS","Capacity Retirements|Electricity|Gas|ST|w/ CCS",
                             "Capacity Retirements|Electricity|Gas|ST",
                             "Capacity Retirements|Electricity"))) %>%
    group_by(model, scenario, unit, datasrc, variable, region) %>%
    summarize(value = mean(value)) %>%
    mutate(variable = paste0(variable,"|Average 2021-2035"), year = 2035)

  df2 = rbind(df, ind_var, gcampnnl_nonco2, cap_add_avg) %>%
    arrange(model) %>%
    filter(!(model == "MARKAL-NETL" & variable %in% c("Emissions|Non-CO2 GHG"))) %>% # remove, #s too low
    filter(!(model == "NEMS-OP" & variable %in% c("Emissions|CO2|Industrial Processes"))) # remove, reported as 0

  # copy nonco2 and land sink gcam numbers for other models
  gcam_netghg = df2 %>%
    filter(model == "GCAM-PNNL" &
             variable %in% c("Emissions|Non-CO2 GHG", "Carbon Sequestration|LULUCF") &
             scenario %in% c("No IRA", "IRA") &
             year %in% c(2025, 2030, 2035, 2040, 2045, 2050)) %>%
    mutate(datasrc = "GCAM-PNNL values")

  op_nems = gcam_netghg %>% mutate(model = "NEMS-OP")
  regen = gcam_netghg %>% mutate(model = "REGEN-EPRI")
  usrep = gcam_netghg %>% mutate(model = "USREP-ReEDS")
  markal = gcam_netghg %>% filter(variable == "Emissions|Non-CO2 GHG") %>% mutate(model = "MARKAL-NETL")

  copied_netghg = rbind(op_nems, regen, usrep, markal)

  df3 = rbind(df2, copied_netghg)

  # make Emissions|CO2 for NEMS-OP and REGEN-EPRI
  gcam_nonenergyco2 = df3 %>%
    filter(model == "GCAM-PNNL" &
             variable == "Emissions|CO2|Industrial Processes" &
             scenario %in% c("No IRA", "IRA", "IRA.High") &
             year %in% c(2025, 2030, 2035, 2040, 2045, 2050)) %>%
    mutate(datasrc = "GCAM-PNNL values")
  gcam_yrs = unique(gcam_nonenergyco2$year)

  op_nems_ind = gcam_nonenergyco2 %>% mutate(model = "NEMS-OP")
  regen_ind = gcam_nonenergyco2 %>% mutate(model = "REGEN-EPRI")

  total_co2 = df3 %>%
    filter(model %in% c("NEMS-OP", "REGEN-EPRI") &
             variable %in% c("Emissions|CO2|Energy") &
             year %in% gcam_yrs &
             scenario %in% c("No IRA", "IRA")) %>%
    rbind(op_nems_ind, regen_ind) %>%
    mutate(variable = "Emissions|CO2",
           datasrc = "calculated w/ GCAM-PNNL values") %>%
    group_by(model,scenario,unit,year,datasrc,region,variable) %>%
    summarise(value = sum(value))

  all = rbind(df3, total_co2)

  all

}


#' unit_conversion()
#' manual changes to data for unit conversion for the leep report
#' @param df
#'
#' @return


unit_conversion = function(df) {

  all_converted = df %>%
    # Mt CO2-equiv/yr to Mt CO2e/yr
    mutate(
      unit = case_when(
        unit == "Mt CO2-equiv/yr" ~ "Mt CO2e/yr",
        TRUE ~ unit)) %>%
    # convert EJ to quads
    mutate(
      value = case_when(
        unit == "EJ/yr" ~ value * 0.9478,
        TRUE ~ value)) %>%
    mutate(
      unit = case_when(
        unit == "EJ/yr" ~ "Quads",
        TRUE ~ unit)) %>%
   # convert Mt CH4/yr to Mt CO2e/yr
    mutate(
      value = case_when(
        unit == "Mt CH4/yr" ~ value * 28,
        TRUE ~ value)) %>%
    mutate(
      unit = case_when(
        unit == "Mt CH4/yr" ~ "Mt CO2e/yr",
        TRUE ~ unit)) %>%
    # convert kt N2O/yr to Mt CO2e/yr
    mutate(
      value = case_when(
        unit == "kt N2O/yr" ~ value * 265/1000,
        TRUE ~ value)) %>%
    mutate(
      unit = case_when(
        unit == "kt N2O/yr" ~ "Mt CO2e/yr",
        TRUE ~ unit))

  all_converted

}

