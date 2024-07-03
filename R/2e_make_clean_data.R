
#' complete_implicit_na
#' fill in implicit NAs so differences can be calculated from unreported variables that are implicitly 0s
#' @param df
#'
#' @return

complete_implicit_na = function(df) {

  print(paste0("Completing implicit NAs"))
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

#' manual_data_update()
#' manual changes to data
#' @param df
#'
#' @return

manual_data_update <- function(df) {

  return(df)

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

make_data_long_clean <- function(data_long,
                                 ratio_var,
                                 summation_var,
                                 cumulative_var,
                                 annual_growth_rate_var,
                                 per_diff_var,
                                 config,
                                 settings) {
  
  data_long_clean = data_long %>%
    filter(year >= 2020) %>%
    complete_implicit_na() %>%
    manual_data_update() %>%
    make_calculated_vars(ratio_var,
                         summation_var,
                         cumulative_var,
                         annual_growth_rate_var,
                         per_diff_var)
  
  # save off data_long data to csv
  outputpath = paste0("output/",settings$version)
  create_folders(outputpath)
  create_folders(paste0(outputpath,"/interim_data"))
  write_csv(data_long_clean,paste0(outputpath,"/interim_data/data_long_clean.csv"))
  
  return(data_long_clean)
  
}





