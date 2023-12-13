# normalize_data.R

# For the purpose of an analysis, scenario values in input files (ID's) need to
# be mapped to run 'archetypes' e.g., for comparison across models. These values
# are both stored under `scenario` for simplicity, and the goal is to keep
# metadata inside `datasrc` when the names are changed. For example, over the
# course of an analysis changes may be made to the "Reference" scenario, such
# that a new run is used to represent the reference.


######################   unused functions   ###########################################

# data <- emf_data_long
# input condition: data has at least standard columns, potential mix of national and regional values.
# Regional should be summable.
# output condition: standard columns, now with region="USA".
# Total value of all unit/variable combinations is the same as the non-overlapping subset of the inputs.
# Preference for national over regional total if both exist.

# TODO: need to re-format transform_to_national to handle all sub-national results

transform_to_national <- function(data) {

  candidate_grp_vars <- c("model", "scenario", "variable", "unit", "year",
                          "datasrc") # not: region, value
  grp_vars <- names(data)[names(data) %in% candidate_grp_vars]

  candidate_val_vars <- c("value", 2010:2050)
  val_vars <- names(data)[names(data) %in% candidate_val_vars]

  natl_sum <- function(rgn, val) {
    if("USA" %in% rgn) {
      sum(val[rgn == "USA"], na.rm = TRUE)
    } else sum(val, na.rm = TRUE)
  }

  res <- data %>%
    mutate(region = if_else(
      region %in% c("USA", "US", "United States"),
      "USA", region)) %>%
    group_by_at(grp_vars) %>%
    summarize_at(.vars = val_vars, .funs = ~natl_sum(region, .x)) %>%
    mutate(region = "USA") %>%
    ungroup()

}


# is a variable actually 0 or is should it be NA?

all_blank_or_zero <- function(x) {
  all(is.na(x) | x == 0)
}


#' map_variable_names
#' unused as of Feb 9 2022
#'
#' was intended to map incorrect or old variable names to what it should be on the current template
#'
#' @param data
#' @param mapping
#'
#' @return
#' @export

map_variable_names <- function(data,
                               mapping = NULL) {

  join_vars <- c("variable")
  grp_vars <- setdiff(names(data), "value")

  mapping <- if(is.null(mapping)) {
    rlang::global_env()$config$variable_mapping
  } else mapping

  if(is.null(mapping)) stop("You must either pass `mapping` to map_variable_names, or make config$variable_mapping available in the global environment.")

  # drop modeled year information from ReEDS for now
  data <- filter(data, ! str_detect(variable, "Mod Yr$"))

  res <- data %>%
    left_join(mapping, by = join_vars) %>%
    mutate(variable = if_else(!is.na(variable_new), variable_new, variable)) %>%
    select(-variable_new) %>%
    group_by_at(vars(grp_vars)) %>%
    summarize(value = sum(value)) %>%
    ungroup()

  res
}


#' Transform long format data to tidy format
#' unused as of Feb 9 2022
#'
#' @export
transform_long_to_tidy <- function(data) {

  data %>%
    select(-one_of("unit")) %>%
    tidyr::pivot_wider(
      names_from = "variable",
      values_from = "value"
    )

}
