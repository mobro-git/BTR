##########################################################################
# Part 0. Import the figure list and perform checks
##########################################################################

#' import_figure_csv
#' this function reads the plot list and performs checks on the given plot_list csv
#'
#' @param plot_list
#' @param figure_type please make sure figure_type only takes on the value stacked, diff, or ts
#' @return

import_figure_csv <- function(plot_list, figure_type, config) {

  if (! (figure_type %in% c("scatter","stackbar","ref_stackbar","timeseries","diffbar","cone","band","sankey","corrplot"))) {
    rlang::abort("Please use 'scatter','stackbar','ref_stackbar','timeseries','diffbar','cone','band','sankey','corrplot' for figure_type.")
  }

  df = readr::read_csv(plot_list, col_types = cols())

  status = df %>%
    assert_figure_csv_has_standard_columns(figure_type) %>%
    assert_vars_in_template_or_calculated(figure_type, config) %>%
    assert_models_in_config(config) %>%
    assert_scenarios_in_config(config) %>%
    assert_regions_in_config(config) %>%
    assert_years_in_config_or_numeric(config) %>%
    assert_valid_page_filter(figure_type) %>%
    check_figure_specification(figure_type)

  if (status == TRUE) {
    df %>% set_default_page_filter()
  }
}

##########################################################################
# Part 1. Functions that check the validity of the plot map csv files
# 1. check the column names
# 2. check whether all variables are present in the template/calculated list,
# 3. check whether all regions, models, scenarios specifications are present in
#     config
# 4. check whether each figure_no corresponds to unique specifications
##########################################################################


#############################################
# Part 1.1 check column names
#############################################

# standard column list: fist check if the csv's have standardized column names

standard_fig_cols <- c("figure_no", "title_name", "variable", "variable_rename", "flip_sign",
                       "regions", "models", "years", "scenarios",
                       "type", "x", "y", "facet1", "facet2", "page_filter", "color", "scales")

standard_stackbar_cols <- standard_fig_cols

standard_ref_stackbar_cols <- c(standard_fig_cols, "ref_model")

standard_diffbar_cols <- c(standard_fig_cols, "ref_type", "ref_value", "line_request")

standard_timeseries_cols <- standard_fig_cols

standard_cone_cols <- c(standard_fig_cols, "range")

standard_band_cols <- c("figure_no", "title_name", "variable", "variable_rename", "flip_sign",
                        "regions", "models", "years", "scenarios",
                        "type", "x", "y", "page_filter", "color", "scales", "ref_scen", "ref_name", "range_name")

standard_scatter_cols <- c(standard_fig_cols, "shape", "xtype", "ytype","xlab", "ylab",
                           "ref", "ref_regions", "ref_models", "ref_years",	"ref_scenarios",
                           "ref_label_change",	"ref_label_content",	"text")

standard_sankey_cols <- c("figure_no", "title_name", "regions", "models", "years", "scenarios",
                          "source", "source_var", "target", "target_var", "link_var")

standard_corrplot_cols <- c("figure_no", "title_name", "variable", "variable_rename", "flip_sign",
                            "regions", "models", "years", "scenarios","page_filter","use","method_cor","method_corrplot","type","diag")


#' assert_figure_csv_has_standard_columns
#'
#' This function checks if all of the standard columns for a specific plot type is present in the mapping list
#' @param status for the plot list
#' @param figure_type please make sure figure_type only takes on the value stacked, diff, or ts
#'
#' @return

assert_figure_csv_has_standard_columns <- function(status, figure_type) {
  if(! all(get(paste("standard_", figure_type, "_cols", sep = "")) %in% names(status))) {
    rlang::abort(paste("Missing at least one standard column in the ", figure_type, " plot mapping csv.", sep = ""),
                 class = 'plot_mapping_csv')
  }
  invisible(status)
}


#####################################
# Part 1.2 check whether all vars
# are covered
#####################################

#' assert_vars_in_template_or_calculated
#'
#' check if all vars are present in the template
#' @param status for plot list
#'
#' @return TRUE or abort

assert_vars_in_template_or_calculated <- function(status, figure_type, config) {

  if (figure_type != "sankey") {
    plot_vars = unique(status$variable)
  }

  else if (figure_type == "sankey") {
    plot_vars = c(
      unique(status$source_var),
      unique(status$link_var),
      unique(status$target_var))
  }

    template_vars = unique(config$template$variable)

    all_vars = c(unique(config$template$variable),
                 unique(config$calculated_var$ratio_var$variable),
                 unique(config$calculated_var$summation_var$variable),
                 unique(config$calculated_var$cumulative_var$new_variable),
                 unique(config$calculated_var$annual_growth_rate_var$new_variable),
                 unique(config$calculated_var$per_diff_var$new_variable))

    present = plot_vars %in% all_vars

    if (! all(present)) {
      rlang::abort(paste("Variable \"", plot_vars[(plot_vars %in% all_vars) == 0], "\" NOT in the template or calculated variable list.",
                         sep = ""),
                   class = 'plot_variable')
    }

  invisible(status)

}

#####################################
# Part 1.3 check whether all configurations
# are specified in config
#####################################

#' assert_models_in_config
#'
#' make sure all the values in the 'models' column exist in config
#' @param status for plot list
#'
#' @return status or abort

assert_models_in_config <- function(status, config) {

  not_present = c()

  unique_model_config = unique(status$models)

  for(item in unique_model_config) {

    present = (item %in% names(config) | (item %in% config$all_models))

    if(! all(present)) {not_present = append(not_present, item)}

  }

  if(length(not_present) > 0) {
    rlang::abort(message = paste("The following item in the 'models' column NOT in config: ",not_present),
                 class = 'plot_mapping_csv models')
  }
  invisible(status)

}

#' assert_scenarios_in_config or valid
#'
#' make sure all the values in the 'scenarios' column exist in config
#' @param status
#'
#' @return status or abort
#' @export
assert_scenarios_in_config <- function(status, config) {

  not_present = c()

  unique_scenario_config = unique(status$scenarios)

  for(item in unique_scenario_config) {

    present = (item %in% names(config) | (item %in% config$all_scenarios))

    if(! all(present)) {not_present = append(not_present, item)}

  }

  if(length(not_present) > 0) {
    rlang::abort(message = paste("The following item in the 'scenarios' column NOT in config: ",not_present),
                 class = 'plot_mapping_csv scenarios')
  }
  invisible(status)
}

#' assert_regions_in_config
#'
#' make sure all the values in the 'regions' column exist in config
#' @param status
#'
#' @return status or abort
#' @export
assert_regions_in_config <- function(status, config) {

  present = unique(status$regions) %in% names(config)

  if(! all(present)) {
    rlang::abort(message = paste("\"", status$regions[!present], "\" in the 'regions' column NOT in config.", sep = ""),
                 class = 'plot_mapping_csv regions')
  }
  invisible(status)
}

#' assert_years_in_config_or_numeric
#'
#' make sure all the values in the 'years' column either exist in config or
#' are numeric
#' @param status
#'
#' @return status or abort
#' @export
assert_years_in_config_or_numeric <- function(status, config) {

  not_present = c()

  unique_years_config = unique(status$years)

  for(item in unique_years_config) {

    present = (item %in% names(config) | is.numeric(item))

    if(! all(present)) {not_present = append(not_present, item)}

  }

  if(length(not_present) > 0) {
    rlang::abort(message = paste("The following item in the 'years' column NOT in config: ",not_present),
                 class = 'plot_mapping_csv years')
  }

  invisible(status)
}

#' assert_valid_page_filter
#'
#' make sure all the values in the 'page_filter' column refer to valid column names
#' @param status
#'
#' @return status or abort
#' @export
assert_valid_page_filter <- function(status, figure_type) {

  if(figure_type != "sankey") {

    unique_page_filter = unique(status$page_filter)

    present = (unique_page_filter %in% c("region", "year", "scenario", "model")) || is.na(unique_page_filter)

    if(! all(present)) {
      rlang::abort(message = paste("\"", status$page_filter[!present], "\" in the 'page_filter' column need to be region, year, scenario, or model.", sep = ""),
                   class = 'plot_mapping_csv page_filter')
    }
  }

  invisible(status)
}


#' set_default_page_filter
#'
#' set all the empty in the 'page_filter' to 'region'
#' @param df
#'
#' @return df or abort
#' @export
set_default_page_filter <- function(df) {

  unique_page_filter = unique(df$page_filter)

  empty_page_filter = is.na(unique_page_filter)

  if (any(empty_page_filter)){
    rlang::warn("No value for certain figures in the page_filter column. Setting to region as default.")
    df = df %>%
      mutate(page_filter = case_when(
        is.na(page_filter) ~ 'region'))
  }
  df
}


#####################################
# Part 1.4 check figure specification
# consistency
#####################################

# objects for figure_type

fig_type_obj <- c("title_name",
                  "regions", "models", "years", "scenarios",
                  "type", "x", "y", "facet1", "facet2", "page_filter", "color", "scales")

stackbar = fig_type_obj

ref_stackbar = c(fig_type_obj, "ref_model")

diffbar = c(fig_type_obj, "ref_type", "ref_value", "line_request")

timeseries = fig_type_obj

cone = c(fig_type_obj, "range")

band <- c("title_name",
          "regions", "models", "years", "scenarios",
          "type", "x", "y", "page_filter", "color", "scales",
          "ref_scen","ref_name","range_name")

scatter = c(fig_type_obj, "xtype", "ytype", "xlab", "ylab",
            "ref", "ref_regions", "ref_models", "ref_years",	"ref_scenarios",
            "ref_label_change",	"ref_label_content",	"text")

sankey <- c("title_name",
            "regions", "models", "years", "scenarios",
            "source", "source_var", "target", "target_var", "link_var")

corrplot <- c("title_name","regions","models","years","scenarios","page_filter","use","method_cor","method_corrplot","type","diag")


#' check_figure_specification
#'
#' This function checks if each figure number corresponds to only one set of specification
#' @param status
#' @param figure_type
#'
#' @return TRUE or kill the process
#' @export
#'
check_figure_specification <- function(status, figure_type) {

  if(figure_type != "sankey") {

    ###  check if the figure number corresponds to only one set of specification -----
    summary_list = status %>%
      group_by(figure_no) %>%
      summarise(across(.cols = get(figure_type), .fns = n_distinct))

    if (sum(select(summary_list, -c("figure_no")) != 1) != 0) {
      rlang::abort(paste("Figure ", summary_list$figure_no[which(select(summary_list, -c("figure_no")) != 1)],
                         " do not have unique plot specifications such as title_name, region, scenario, model,
                       facet, color, or scale",
                         sep = "", collapse = "\n"), class = 'plot_variable')
    }
  }
  TRUE
}
