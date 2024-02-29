##########################################################################
# Part 1. Data Processing

# 1. crudely filters out relevant vars and flip signs if applicable
# 2. model, scenario, year, region filters
# 3. lumped filter for figures
# 4. figure specific processing
##########################################################################

#########################################
# 1.1 Crude Filter
#########################################

#' preliminary_data_processing_for_plotting
#' This function only crudely filters out relevant variables and prepare the values to be graphed
#' @param df_input emf_data_long
#' @param plot_list list of plot requests
#'
#' @return

 preliminary_data_processing_for_plotting <- function(df_input, plot_list) {

  # create a dataframe with only variables that are going to be plotted
  df <- df_input %>%
    filter(variable %in% unique(plot_list$variable)) %>%
    # complete_implicit_na() %>%
    left_join(plot_list, by = ("variable" = "variable")) %>%
    distinct()

  # flip the signs of the values when indicated
  df <- df %>%
    mutate(value = ifelse(flip_sign == 0, value, -value))

  df
}

#########################################
# 1.2 More Detailed Filter:
# scenario, model, year, region
#########################################

#' sccenario_filter
#'
#' return a list of arranged scenarios
#' @param df
#'
#' @return filtered df
scenario_filter <- function(df, column, config) {

  if(exists(as.character(unique(df[[column]])), config)) {
    df %>%
      filter(scenario %in% config[[unique(df[[column]])]]) %>%
      mutate(scenario = factor(scenario, levels = config[[unique(df[[column]])]]))
  } else {
    df %>%
      filter(scenario == unique(df[[column]]))
  }
}


#' model_filter
#'
#' @param df
#'
#' @return
model_filter <- function(df, column, config) {

  if(exists(as.character(unique(df[[column]])), config)) {
    df %>%
      filter(model %in% config[[unique(df[[column]])]]) %>%
      mutate(model = factor(model, levels = config[[unique(df[[column]])]]))
  } else {
    df %>%
      filter(model == unique(df[[column]]))
  }
}

#' year_filter
#'
#' @param df
#'
#' @return
year_filter <- function(df, column, config) {
  if(exists(as.character(unique(df[[column]])), config)) {
    df %>%
      filter(year %in% config[[unique(df[[column]])]])
  } else {
    df %>%
      filter(year == unique(df[[column]]))
  }
}

#' region_filter
#'
#' @param df
#'
#' @return
region_filter <- function(df, column, config) {
  df %>%
    filter(region %in% config[[unique(df[[column]])]])
}


#########################################
# 1.3 Lumped Filters w/ nonzero obs checks
#########################################

#' smyr_filter
#' scenario, model, year, region filter
#'
#' @param df
#'
#' @return
#' @export

smyr_filter <- function(df, config) {

  if (nrow(df) == 0) {
    rlang::warn(message = "There is no data corresponding to this figure title without filter.")
  } else {
    checklist = c("scenario", "model", "year", "region")
    for (factor in checklist) {
      filter_fn = get(paste(factor, "_filter", sep = ""))
      df = filter_fn(df, paste(factor, "s", sep = ""), config)
      if (nrow(df) == 0) {
        rlang::warn(message = "There is no data corresponding to this figure title after the ",
                    factor, " filter.", sep = "")
        return(NULL)
      }
    }
  }
  df
}

ref_smyr_filter <- function(df, config) {
  if (nrow(df) == 0) {
    rlang::warn(message = "There is no data corresponding to this figure title without filter.")
  } else {
    checklist = c("scenario", "model", "year", "region")
    for (factor in checklist) {
      filter_fn = get(paste(factor, "_filter", sep = ""))
      df = filter_fn(df, paste("ref_", factor, "s", sep = ""), config)
      if (nrow(df) == 0) {
        rlang::warn(message = "There is no reference data corresponding to this figure title after the ",
                    factor, " filter.", sep = "")
        return(NULL)
      }
    }
  }
  df
}


#########################################
# 1.4 Figure Specific Processing
#########################################

#' time_series_figure_specific_data_processing
#'
#' @param df
#'
#' @return
time_series_figure_specific_data_processing <- function(df, config) {

  figure = unique(df$title_name)
  # filter out specific scenarios, models, years, and regions
  df <- smyr_filter(df, config)
  ## Group and rename the variables
  if (!is.null(df)) {
    df <- df %>%
      group_by(title_name, figure_no)
  }

  df
}

#' corrplot_figure_specific_data_processing
#'
#' @param df
#'
#' @return
corrplot_figure_specific_data_processing <- function(df, config) {

  figure = unique(df$title_name)
  # filter out specific scenarios, models, years, and regions
  df <- smyr_filter(df, config)
  ## Group and rename the variables
  if (!is.null(df)) {
    df <- df %>%
      group_by(title_name, figure_no)
  }

  df
}



#' stacked_bar_figure_specific_data_processing
#'
#' @param df
#'
#' @return

stacked_bar_figure_specific_data_processing <- function(df, config) {
  figure = unique(df$title_name)

  # filter out specific scenarios, models, years, and regions
  df <- smyr_filter(df, config)
  ## Group and rename the variables

  if (!is.null(df)) {
    df <- df %>%
      group_by(model, scenario, region, year, unit, title_name, datasrc, figure_no)
  }

  df
}

#' ref_stacked_bar_figure_specific_data_processing
#'
#' @param df
#'
#' @return

ref_stacked_bar_figure_specific_data_processing <- function(df, config) {
  figure = unique(df$title_name)

  # filter out specific scenarios, models, years, and regions
  df <- smyr_filter(df, config)
  ## Group and rename the variables

  if (!is.null(df)) {
    df <- df %>%
      group_by(model, scenario, region, year, unit, title_name, datasrc, figure_no)
  }

  df
}


#' diff_bar_figure_specific_data_processing
#'
#' @param df
#'
#' @return

diff_bar_figure_specific_data_processing <- function(df, config) {

  figure = unique(df$title_name)

  df <- smyr_filter(df, config)

  if (!is.null(df)) {

    # compute relevant numbers
    # year as reference
    if (unique(df$ref_type) == "year") {
      df = df %>%
        group_by(model, region, scenario, unit, variable) %>%
        filter(ref_value %in% unique(year)) %>%
        mutate(diff = value - value[year == ref_value]) %>%
        ungroup()}
    else if (unique(df$ref_type) == "scenario") {
          df = df %>%
            group_by(model, region, year, unit, variable) %>%
            filter(ref_value %in% unique(scenario)) %>%
            mutate(diff = value - value[scenario == ref_value]) %>%
            ungroup() %>%
            filter(scenario != ref_value)}

    if (nrow(df) == 0) {
      rlang::warn(paste("There is no observation for figure ", figure, ". Please check if there is any problem.", sep = ""))
    } else {
      ## Group and rename the variables
      df <- df %>%
        group_by(model, scenario, region, year, unit, title_name, datasrc, figure_no)

      if (unique(df$line_request)) {
        df = df %>%
          mutate(diff_sum = sum(diff))
      }
    }
  }

  df
}


#' cone_uncertainty_figure_specific_data_processing
#'
#' @param df
#'
#' @return

cone_uncertainty_figure_specific_data_processing <- function(df, config) {
  figure = unique(df$title_name)

  # filter out specific scenarios, models, years, and regions
  df <- smyr_filter(df, config)
  ## Group and rename the variables

  if (!is.null(df)) {
    df <- df %>%
      group_by(model, scenario, region, year, unit, title_name, datasrc, figure_no)
  }

  df
}


#' band_figure_specific_data_processing
#'
#' @param df
#'
#' @return

band_figure_specific_data_processing <- function(df, config) {
  figure = unique(df$title_name)

  # filter out specific scenarios, models, years, and regions
  df <- smyr_filter(df, config) %>%
    mutate(range = case_when(
      scenario == ref_scen ~ ref_name,
      TRUE ~ range_name
    ))  %>%
    mutate(range = factor(range, levels = c(unique(df$ref_name), unique(df$range_name)))) %>%
    group_by(across(c(-variable, -value))) %>%
    summarise(value = sum(value)) %>%
    ungroup()

  if(unique(df$difference)){
    diff <- df %>%
      group_by(model, region, year, unit, variable_rename) %>%
      filter(ref_scen %in% unique(scenario)) %>%
      mutate(diff = value - value[scenario == ref_scen]) %>%
      ungroup() %>%
      select(-range,-value) %>%
      pivot_longer(cols = diff, names_to = "range", values_to = "value") %>%
      filter(scenario != ref_scen) %>%
      mutate(range = "Difference from Reference")

    df <- rbind(df, diff)
  }

  ## Group and rename the variables

  if (!is.null(df)) {
    df <- df %>%
      group_by(model, scenario, region, year, unit, title_name, datasrc, figure_no)
  }

  df
}

#' scatterplot_figure_specific_data_processing
#'
#' @param df
#'
#' @return

scatterplot_figure_specific_data_processing <- function(df, config) {
  figure = unique(df$title_name)

  # filter out specific scenarios, models, years, and regions
  main_df <- smyr_filter(df, config)

  if (unique(df$ref)) {
    ref_df <- ref_smyr_filter(df, config)

    if(nrow(ref_df) != 0) {
      ref_df[[unique(ref_df$ref_label_change)]] = unique(ref_df$ref_label_content)
    }
  }

  df <- rbind(main_df, ref_df) %>%
    distinct()

  if (unique(df$xtype) == "variable" & unique(df$ytype) == "variable") {
    df <- df %>%
      ungroup() %>%
      pivot_wider(names_from = variable,
                  values_from = value,
                  id_cols = -c("datasrc", "unit", "flip_sign") )
  }

  if (!is.null(df)) {
    df <- df %>%
      group_by(model, scenario, region, year, title_name, figure_no)
  }

  df
}

