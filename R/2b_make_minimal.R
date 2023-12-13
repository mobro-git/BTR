# emf_data_min
# this object only has a minimum amount of processing, including
#
# rows: remove variables inconsistent with the data template
# columns: standardize column names and make sure standard columns exist
# values: make sure the year and value columns are numeric
#         and standardize percentage format
# dataframe: make everything in the long format
#
# Compared to emf_data_long, emf_data_min does not care about the model/scenario name


#' process_minimal_from_raw
#'
#' This function produces a dataframe with minimum processing
#' It 1. removes empty col headers and empty rows
#' 2. tranform values into long format
#' 3. standardize column names
#'
#' @param data
#'
#' @return min_processed_data
#' @export
process_minimal_from_raw <- function(data) {

  data %>%
    select(-starts_with("...")) %>% # empty col headers dropped
    janitor::remove_empty("rows") %>%
    transform_emf_to_long() %>%
    standardize_col_names()
}


#' transform_emf_to_long
#' @param data
#' Transform original EMF format data to long format
#' Specifically, transform columns 2010 to 2100 into year and value
#' filter out na values and set year to numeric
#'
#' @return data
#'
#' @export
transform_emf_to_long <- function(data) {

  if(!"value" %in% names(data)){
    data %>%
      tidyr::pivot_longer(
        cols = num_range(prefix = "", range = 2010:2100),
        names_to = "year",
        values_to = "value"
      ) %>%
      mutate(year = as.numeric(year)) %>%
      filter(!is.na(value))
  } else {
    data
  }
}


#' Standardize data file column names
#' rename/standardize column names and retain extra columns
#'
#'
#' Note: Standard columns and name-cleaning pairs recorded in function.
#' Should retain any extra columns, and work with either wide or long format.
#' Standardizing column names does not necessarily mean that all columns are present.
#'
#' @param data EMF-style data object
#' @export
standardize_col_names <- function(data) {

  rename_candidates <- c(
    "model" = "Model",
    "scenario" = "Scenario",
    "region" = "Region",
    "variable" = "Variable",
    "unit" = "Unit",
    "unit" = "Units",
    "year" = "Year",
    "value" = "Value",
    "datasrc" = "file"
  )

  rename_present <- rename_candidates[rename_candidates %in% names(data)]

  data %>%
    select(-starts_with("...")) %>% # empty col headers dropped
    rename(!!! rename_present)
}

