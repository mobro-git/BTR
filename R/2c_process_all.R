
# Read in raw data with minimal processing

read_process_minimal_from_raw <- function(filepath) {
  
  print(filepath)
  read_raw_data_file(filepath) %>%
    process_minimal_from_raw() %>%
    standardize_col_names()
  
}


# Load and process raw data

read_process_data_file <- function(filepath, config) {
  print(paste0("Reading ",filepath))
  read_raw_data_file(filepath) %>%
    process_data_file(config)
}

read_usproj_data_file <- function(filepath, crosswalk_usproj_csv) {
  print(paste0("Reading ",filepath))
  
  crosswalk <- read_csv(crosswalk_usproj_csv)
  
  raw <-  read_file_ext(filepath) %>%
    mutate(datasrc = fs::path_file(filepath)) %>% 
    map_usproj_scenario_names(crosswalk) %>% 
    select(datasrc, model, scenario, everything(),-notes)
  
}


process_data_file <- function(data, config = NULL) {

  min <- data %>%
    process_minimal_from_raw()

  # standardization steps:
  std <- min %>%
    standardize_col_names() %>%
    standardize_row_data(config) %>%
    filter(!is.na(unit)) %>%
    normalize_units()

  res <- std %>%

    # analysis stuff:
    map_scenario_names(config$scen_mapping) %>%
    filter(!is.na(scenario)) %>%
    # TODO: need to re-format transform_to_national to handle all sub-national results
    # transform_to_national() %>% # right now uses sum(.x, na.rm = TRUE)
    select(model, scenario, region, variable, unit, year, value, datasrc) %>%
    assert_has_standard_cols()
}



#' standardize_row_data
#'
#'
#' filter out NA model and NA scenarios
#' only keep variable and unit combos consistent with the data template
#' standardize the format of year and value
#' change variables expressed in percentages into scentific expressions

standardize_row_data <- function(data, config) {

  # TODO: should some of these be warnings/errors or evaluated other places?

  data %>%
    filter(!is.na(model) & !is.na(scenario)) %>%

    # Only keep variable data which is in the template
    filter(variable %in% config$template$variable) %>%

    # Only keep data with variable/unit combination that match the template
    semi_join(config$template, by = c("variable", "unit")) %>%

    mutate(year = as.numeric(year),
           value = as.numeric(value)) %>%
    mutate(value = if_else(unit == "%", value * .01, value)) # pct 100 -> 1

}


#' read_scen_mapping
#'
#' this function reads the scenario mapping file and specifies the column type
#' for a list of pre-specified variables: datasrc, model, scenario, model_new, scenario_new

read_scen_mapping <- function(filepath) {
  scen_mapping <- readr::read_csv(
    filepath,
    col_types = readr::cols(
      datasrc = col_character(),
      model = col_character(),
      scenario = col_character(),
      model_new = col_character(),
      scenario_new = col_character()
    ))
  scen_mapping
}

#' crosswalk_scenario_names
#'
#' Modify `model` and `scenario` names to be canonical
#'
#' This function first checks whether all combinations of datasrc-model-scenario in data are in the crosswalk_model-runs
#' Then, it replaces model and scenario name with standardized names in the crosswalk_model-runs file

map_scenario_names <- function(data,
                               scen_mapping = NULL) {

  #### check whether scenn_mapping is appropriately constructed
  #### and all combinations of datasrc-model-scenario in the data
  #### exist in scen_mapping


  join_vars <- c("datasrc", "model", "scenario")
  # scen_mapping <- if(is.null(scen_mapping)) {
  #   rlang::global_env()$config$scen_mapping
  # } else scen_mapping

  if(is.null(scen_mapping)) stop("You must pass `scen_mapping` to map_scenario_names.")

  # make sure crosswalk has everything needed
  preflight <- distinct_at(data, join_vars) %>%
    anti_join(scen_mapping, by = join_vars)

  if(nrow(preflight) > 0) {
    write_csv(preflight, "data-raw/scenario-mapping-ADDITIONS.csv")
    print(glue::glue_data(preflight, "{datasrc}, {model}, {scenario}"))
    rlang::abort("Model+scenario combinations above not present in scenario crosswalk (data-raw/scenario-mapping.csv).
                 Combinations have been saved off to data-raw/scenario-mapping-ADDITIONS.csv. 
                 There might still be missing model+scenario in another datasource.")
  }

  # change model and scenario names in data to standardized names in scen_mapping file
  # check if the dataframe has standard columns
  scen_mapping <- select(scen_mapping, join_vars, model_new, scenario_new)

  res <- data %>%
    left_join(scen_mapping, by = join_vars) %>%
    mutate(
      model = model_new,
      scenario = scenario_new
    ) %>%
    select(-model_new, -scenario_new) %>%
    select(model, scenario, region, year, variable, unit, value, everything()) %>%
    assert_has_standard_cols()

  res
}

# Add model and scenario columns and names to usproj dataframe based on the crosswalk_usproj file

map_usproj_scenario_names <- function(raw, crosswalk) {
  
  # make sure crosswalk has everything needed
  preflight <- distinct_at(raw, "datasrc") %>%
    anti_join(crosswalk, by = "datasrc")
  
  if(nrow(preflight) > 0) {
    paste0(preflight)
    rlang::abort(paste0("Please add ", preflight, " to the usproj scenario crosswalk (data-raw/crosswalk/crosswalk_usproj.csv)"))
  }
  
  crosswalked = raw %>%
    left_join(crosswalk, by = "datasrc")
  return(crosswalked)
  
}


#' assert_has_standard_cols
#' checking if all standard columns exist in the dataframe
#' If not, throw an error
#'
#' @param data
#'
#' @return
#' @export

standard_cols = c("model", "scenario", "region", "variable", "unit", "year", "value", "datasrc")

assert_has_standard_cols <- function(data) {

  if(! all(standard_cols %in% names(data))) stop("Missing at least one standard column")

  invisible(data)
}


#' check_omissions
#' outputs omitted data due to variable or unit mismatch
#'
#' @param data
#'
#' @return
#' @export

check_omissions = function(data_raw, data_long, template_original, template) {

  data_raw = data_raw %>% arrange(variable)

  raw_vars = unique(data_raw$variable)
  orig_vars = unique(template_original$variable)
  new_vars = unique(template$variable)

  unmatched_orig = setdiff(raw_vars,orig_vars) # unmatched variables without template additions

  unmatched = setdiff(raw_vars,new_vars) # unmatched variables after template additions

  unmatched

}

