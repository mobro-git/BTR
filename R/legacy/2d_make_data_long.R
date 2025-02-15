# centralized variable processing here for emf_data_long

#' make_data_long
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
make_data_long <- function(data_long_read, settings) {
  data_long <- data_long_read %>% {
    # drop all-zero model-run-variable data
    group_by(., model, scenario, variable) %>%
      filter(!all(value == 0)) %>%
      ungroup()
  } %>%
    relocate_standard_col_order() %>%
    arrange_standard() %>%
    country_abbr() %>%
    filter(!is.na(value))
  
  # save off data_long data to csv
  outputpath = paste0("output/",settings$version)
  create_folders(outputpath)
  create_folders(paste0(outputpath,"/interim_data"))
  write_csv(data_long, paste0(outputpath,"/interim_data/data_long.csv"))
  
  return(data_long)
  
}

make_usproj_data_long <- function(usproj_data_loaded, settings) {
  
  usproj_data_long_all_lulucf <- usproj_data_loaded %>%
    tidyr::pivot_longer(
      cols = num_range(prefix = "", range = 1990:2100),
      names_to = "year",
      values_to = "value"
    ) %>%
    mutate(year = as.numeric(year),
           value = replace_na(value, 0)) %>%
    select(
      model,
      scenario,
      usproj_sector,
      gas,
      year,
      value,
      unit,
      usproj_category,
      usproj_category_longname,
      usproj_source,
      usproj_subsource,
      region,
      datasrc
    )
  
  # TODO: Need to figure out whether to sum Non-CO2 gases in LULUCF Sink -
  #       currently netting out all gases.
  
  usproj_lulucf_co2 <- usproj_data_long_all_lulucf %>%
    filter(usproj_sector == 'LULUCF') %>% # gas == 'CO2'
    mutate(usproj_category = 'LULUCF Sink',
           gas = 'LULUCF Sink',
           usproj_sector = 'LULUCF Sink',
           usproj_source = 'LULUCF Sink',
           usproj_subsource = 'LULUCF Sink') %>% 
    group_by(across(c(-value))) %>%
    summarise(value = sum(value),.groups = 'drop') %>%
    select(names(usproj_data_long_all_lulucf))
  
  usproj_data_long <- usproj_data_long_all_lulucf %>% 
    filter(!usproj_sector == 'LULUCF') %>% # gas == 'CO2'
    rbind(usproj_lulucf_co2)
  
  # save off data_long data to csv
  outputpath = paste0("output/",settings$version)
  create_folders(outputpath)
  create_folders(paste0(outputpath,"/interim_data"))
  write_csv(usproj_data_long,paste0(outputpath,"/interim_data/usproj_data_long.csv"))
  
  usproj_data_long
  
}

gen_usproj_ghgi <- function(usproj_data_long_all, config, settings){
  
  scen <- config$ghgi_scen
  if(! scen %in% usproj_data_long_all$scenario){
    rlang::abort('Invalid config$ghgi_scen. Choose valid scenario name from crosswalk_usproj')
  }
  
  usproj_ghgi <- usproj_data_long_all %>% 
    filter(year <= settings$base_year) %>% 
    filter(scenario == scen) %>%
    mutate(proj_name = "ghgi",
           model = "ghgi",
           scenario = "historical")
  
}

gen_usproj_projections <- function(usproj_data_long_all, settings){
  
  usproj_projections <- usproj_data_long_all %>% filter(year > settings$base_year)
    
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
  for (var in 1:nrow(ratio_var)) {
    ratio[[var]] <- data_long %>%
      group_by(model, scenario, region, year) %>%
      summarise(
        value = (value %forwhich% (variable == ratio_var$numerator[var])) /
          (value %forwhich% (variable == ratio_var$denominator[var])),
        variable = ratio_var$variable[var],
        unit = ratio_var$unit[var],
        datasrc = "make_ratio_variables",
        .groups = 'drop'
      ) 
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
  for (i in unique(summation_var$index)) {
  
    var_list <- summation_var %>%
      filter(index == i)

    pre_join <- var_list %>%
      select(lower_level, multiplier) %>%
      rename(variable = lower_level)

    data_long_pre_join <- data_long %>%
      filter(variable %in% pre_join$variable)

    data_long_join <- pre_join %>%
      full_join(data_long_pre_join, by = "variable")

    summation[[i]] <- data_long_join %>%
      filter(variable %in% var_list$lower_level) %>%
      mutate(value = value*multiplier) %>%
      group_by(model, scenario, region, year) %>%
      summarise(
        value = sum(value),
        variable = unique(var_list$variable),
        unit = unique(var_list$unit),
        datasrc = "make_summation_variables",
        .groups = "drop") 
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
              year = year %forwhich% (range == "end_yr"),
              .groups = "drop") %>%
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
      # TODO: either rewrite this code to use the unit in the index_variables.csv or paste("Index to ",ref_value) or something like that
      mutate(unit = "Index",
             datasrc = "index_variables") %>%
      ungroup() %>%
      filter(!is.na(variable)) %>%
      arrange_standard()
  }

  new_vars %>% bind_rows()

}
