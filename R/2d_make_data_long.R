# centralized variable processing here for emf_data_long

#' make_emf_data_long
#'
#' @param data_long_read
#'
#'
#' first step: filter out all model-scenario-variable combinations that have value zero (valid?)
#' second step: rearrange columns
#' third step: change all country abbreviations to their full names
#' fourth step: make calculated variables
#'
#' @return new dataframe
#' @export
#'
#' @examples
make_data_long <- function(data_long_read) {
  data_long <- data_long_read %>% {
    # drop all-zero model-run-variable data
    group_by(., model, scenario, variable) %>%
      filter(!all(value == 0)) %>%
      ungroup()} %>%
    relocate_standard_col_order() %>%
    arrange_standard() %>%
    country_abbr() %>%
    filter(!is.na(value))
}

make_calculated_vars <- function(data_long, ratio_var, summation_var, cumulative_var, annual_growth_rate_var, per_diff_var) {

  print("Creating summation variables")
  summation <- bind_rows(
    data_long,
    make_summation_variables(data_long, summation_var)
  )

  print("Creating ratio variables")
  summation_ratio <- bind_rows(
    summation,
    make_ratio_variables(summation, ratio_var)
  )

  print("Creating percent difference variables")
  summation_ratio_perdiff <- bind_rows(
    summation_ratio,
    make_per_diff_variables(summation_ratio, per_diff_var)
  )

  # cumulative and annual growth rate vars may use some calculated vars and have to come last

  print("Creating cumulative variables")
  summation_ratio_perdiff_cumulative <- bind_rows(
    summation_ratio_perdiff,
    make_cumulative_variables(summation_ratio_perdiff, cumulative_var)
  )

  print("Creating annual growth rate variables")
  all_vars <- bind_rows(
    summation_ratio_perdiff_cumulative,
    make_agr_variables(summation_ratio_perdiff_cumulative, annual_growth_rate_var)
    )

  full_vars <- all_vars %>% filter(!is.na(value))

}

#' relocate_standard_col_order
#' @param data
#'
#' This function first identifies all the standard columns that are present in the data
#' it then returns a re-arranged data, in the order specified in "standard_cols" (except for datasrc, which is dropped)
#'
#' @return rearranged data
#' @export

standard_cols <- c("model","scenario","region","variable","unit","year","value")

relocate_standard_col_order <- function(data) {

  vars_present <- intersect(standard_cols, names(data))
  res <- relocate(data, all_of(vars_present))
}


#' arrange_standard
#'
#' this function arranges data in a pre-specified manner
#' i.e., in the order of: model, scenario, region, variable, unit, and year
#'
#' Note: '!!!' force splits a list of objects and forces early evaluation of the objects
#' '!!', on the other hand, forces a single object
#' @param data to be arranged
#'
#' @return arranged data
#' @export

arrange_standard <- function(data) {

  arr_exprs <- list(
    "model"    = expr(model),
    "scenario" = expr(scenario != "Ref"), # Ref appear first
    "scenario" = expr(scenario),
    "region"   = expr(region != "USA"), # USA appear first
    "region"   = expr(region),
    "variable" = expr(variable),
    "unit"     = expr(unit),
    "year"     = expr(year)
  )

  arr_exprs_present <- arr_exprs[names(arr_exprs) %in% names(data)] %>% set_names(NULL)

  arrange(data, !!!arr_exprs_present, .by_group = FALSE)
}

#' country_abbr
#'
#' @param data_long
#'
#' Standardize the values of regions in data_long
#' e.g., from USA to United States
#'
#' @return data_long
#' @export
#'
#' @examples
country_abbr <- function(data_long) {

  data_long %>%
    mutate(region = case_when(
      region == "USA" ~ "United States",
      region == "CAN" ~ "Canada",
      region == "MEX" ~ "Mexico",
      TRUE~region))
}

#' make_ratio_variables
#'
#' @param data_long
#' @param ratio_var
#'
#' @return
#' @export
#'
#' @examples
make_ratio_variables <- function(data_long, ratio_var) {

  # num_var <- unique(ratio_var$numerator)
  # den_var <- unique(ratio_var$denominator)
  # all_var <- c(num_var, den_var)
  #
  # data_long1 <- data_long %>%
  #   filter(variable %in% all_var)

  ratio = list()
  for (i in 1:nrow(ratio_var)) {
    ratio[[i]] <- data_long %>%
      group_by(model, scenario, region, year) %>%
      summarise(
        value = (value %forwhich% (variable == ratio_var$numerator[i])) /
          (value %forwhich% (variable == ratio_var$denominator[i])),
        variable = ratio_var$variable[i],
        unit = ratio_var$unit[i],
        datasrc = "make_ratio_variables"
      ) %>%
      ungroup()
  }

  all_ratio_vars <- bind_rows(ratio) %>%
    filter(!is.na(variable)) %>%
    arrange_standard()

  all_ratio_vars
}

#' make_summation_variables
#'
#' @param data_long
#' @param summation_var
#'
#' @return
#' @export
#'
#' @examples

make_summation_variables <- function(data_long, summation_var) {

  summation = list()
  for (i in 1:max(summation_var$index)) {

    var_list <- summation_var %>%
      filter(index == i)

    pre_join <- var_list %>%
      select(lower_level, flip_sign) %>%
      rename(variable = lower_level)

    data_long_pre_join <- data_long %>%
      filter(variable %in% pre_join$variable)

    data_long_join <- pre_join %>%
      full_join(data_long_pre_join, by = "variable")

    summation[[i]] <- data_long_join %>%
      filter(variable %in% var_list$lower_level) %>%
      mutate(value = value*flip_sign) %>%
      group_by(model, scenario, region, year) %>%
      summarise(
        value = sum(value),
        variable = unique(var_list$variable),
        unit = unique(var_list$unit),
        datasrc = "make_summation_variables",
        .groups = "drop") %>%
      ungroup()
  }

  bind_rows(summation) %>%
    filter(!is.na(variable)) %>%
    arrange_standard()
}


#' cumulate
#'
#' @param data
#' @param vars
#'
#' @return
#' @export
#'
#' @examples

cumulate <- function(data, vars) {

  year_range <- seq.int(from = vars$start_yr, to = vars$end_yr)

  interp <- data %>%
    filter(variable %in% unique(vars$variable)) %>%
    filter(!is.na(value)) %>%
    complete(nesting(model, scenario, region, variable, unit, datasrc),
             year = year_range) %>%
    arrange(model, scenario, region, variable, year) %>%
    nest(year, value) %>%
    mutate(int_pts = map(data,
                         .f = ~approx(x=.$year, y=.$value, xout = .$year) %>% as_tibble() )) %>%
    unnest(cols = c(data, int_pts)) %>%
    mutate(value = y) %>%
    select(-x, -y)

  cumulative <- interp %>%
    group_by(model, scenario, region, variable, unit, datasrc) %>%
    summarise(
      value = sum(value, na.rm = TRUE), .groups = "keep") %>%
    mutate(variable = vars$new_variable,
           year = vars$end_yr) %>%
    ungroup()

  cumulative
}

#' make_cumulative_variables
#'
#' @param data
#' @param cumulative_var
#'
#' @return
#' @export
#'
#' @examples

make_cumulative_variables <- function(data, cumulative_var) {

  1:nrow(cumulative_var) %>%
     map_dfr(~ cumulate(data, cumulative_var[.x,]))

}


#' make_agr_variables
#'
#' @param data
#' @param annual_growth_rate_var
#'
#' @return
#' @export
#'
#' @examples

make_agr_variables <- function(data, annual_growth_rate_var) {

  var_table <- annual_growth_rate_var %>%
    pivot_longer(cols = c(`start_yr`,`end_yr`), names_to = "range", values_to = "year")

  df <- inner_join(var_table, data, by = c("year","variable"))

  calc <- df %>%
    group_by(model,scenario,region,new_variable,unit,datasrc) %>%
    summarise(value_num = log((value %forwhich% (range == "end_yr")) / (value %forwhich% (range == "start_yr"))),
              value_den = ((year %forwhich% (range == "end_yr")) - (year %forwhich% (range == "start_yr"))),
              value = (value_num/value_den)*100,
              year = year %forwhich% (range == "end_yr")) %>%
    ungroup() %>%
    rename(variable = new_variable) %>%
    mutate(unit = "% Annual Growth Rate",
           datasrc = "make_annual_growth_rate_var") %>%
    select(-value_num,-value_den) %>%
    relocate_standard_col_order()

  # Error message catching too much stuff. Commenting out for now until I can figure out how to see less. Only issue is that i'm generating a lot of NAs, but otherwise its all fine

  # if(any(is.na(calc$value))) {
  #
  #   missing_vars <- calc %>%
  #     filter(is.na(value)) %>%
  #     distinct(model,scenario,variable)
  #
  #   print(missing_vars)
  #
  #   stop("Annual growth rate variables missing start or end year values in raw data")
  # }

}


#' make_per_diff_variables
#'
#' @param data
#' @param per_diff_var
#'
#' @return
#' @export
#'
#' @examples
#'
make_per_diff_variables <- function(data_long, per_diff_var) {

  grouping_variables = c("model", "scenario", "year", "region")

  new_vars = list()
  for (i in 1:nrow(per_diff_var)) {
    new_record = per_diff_var[i, ]

    if (!new_record$ref_type %in% grouping_variables) {
      rlang::error(paste("make per diff variables: ref_type for ",  new_record$variable,
                         " unknown. Please choose value, year, scenario, region, or model.", sep = ""))
    }

    if (!new_record$per_type %in% c("of", "difference")) {
      rlang::error(paste("make per diff variables: per_type for ",  new_record$variable,
                         " unknown. Please choose 'of' or 'difference'.", sep = ""))
    }


    new_vars[[i]] = data_long %>%
      filter(variable == new_record$variable) %>%
      group_by_at(grouping_variables[! grouping_variables %in% new_record$ref_type]) %>%
      filter(any(!!sym(new_record$ref_type) == new_record$ref_value))

    if (new_record$per_type == "of") {
      new_vars[[i]] = new_vars[[i]] %>%
        mutate(value = (value/value[!!sym(new_record$ref_type) == new_record$ref_value])*100)
    }
    else {
      new_vars[[i]] = new_vars[[i]] %>%
        mutate(value = (value/value[!!sym(new_record$ref_type) == new_record$ref_value] - 1)*100)
    }

    new_vars[[i]] = new_vars[[i]] %>%
      mutate(variable = new_record$new_variable,
             unit = new_record$unit,
             datasrc = "make_per_diff_variables") %>%
      ungroup() %>%
      filter(!is.na(variable)) %>%
      arrange_standard()
  }

  new_vars %>% bind_rows()
}


#' index_data_long
#'
#' @param data
#' @param per_diff_var
#'
#' @return
#' @export
#'
#' @examples
#'
index_data_long <- function(data_long, index_var) {

  grouping_variables = c("model", "scenario", "year", "region")

  new_vars = list()
  for (i in 1:nrow(index_var)) {
    new_record = index_var[i, ]

    new_vars[[i]] = data_long %>%
      filter(variable == new_record$variable) %>%
      group_by_at(grouping_variables[! grouping_variables %in% new_record$ref_type]) %>%
      filter(any(!!sym(new_record$ref_type) == new_record$ref_value)) %>%
      mutate(value = value/value[!!sym(new_record$ref_type) == new_record$ref_value]) %>%
      #mutate(variable = paste(new_record$variable,"|Index",sep="")) %>%
      mutate(unit = "Index",
             datasrc = "index_variables") %>%
      ungroup() %>%
      filter(!is.na(variable)) %>%
      arrange_standard()
  }

  new_vars %>% bind_rows()

}
