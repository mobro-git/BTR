# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) 

source("packages.R")

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Set target options:
tar_option_set(
  packages = c("dplyr","readr","tidyverse","datasets"), # packages to make available to targets
  error = "abridge"
)

# Pull in color palette and figure elements
source("scripts/theming.R")

# Define targets
tar_plan(
  
  ##### Config -----------------
  
  config = list(
    # variables and mapping
    scen_mapping = read_scen_mapping(scen_mapping_csv),
    template = template,
    calculated_var = all_calculated,
    
    #models
    models = c("GCAM-LTS","GCAM-PNNL","NEMS-OP","USREP-ReEDS"),
    
    fives = c(seq(2005,2022,by = 1),seq(2025,2040,by = 5)),
    annual = c(seq(2005,2040,by = 1)),
    
    fives_lts = c(seq(2005,2022,by = 1),seq(2025,2050,by = 5)),
    annual_lts = c(seq(2005,2050,by = 1)),
    
    annual_1990 = c(seq(1990,2040,by = 1)),
    annual_2010 = c(seq(2010,2040,by = 1)),
    table = c(2005, 2010, 2015, 2020, 2022, 2025, 2030 , 2035, 2040)
  ),

  ##### Template ---------------------------------------------------

  tar_target(template_xlsx, "data-raw/BTR24_data_template_v1.xlsx", format = "file"),
  tar_target(template, read_emf_template_xlsx(template_xlsx)),

  #### Data Files ----------------------------------------------------------------

  tar_target(data_folder, "data-raw/model-runs/", format = "file"),
  tar_target(data_files, dir_ls(data_folder), format = "file"),

  tar_target(scen_mapping_csv, "data-raw/scenario-mapping.csv", format = "file"),


  #### Data Processing -----------------------

  # _Calculated variables ----

  tar_target(ratio_var_list, "data-raw/calculated_var/ratio_variables.csv", format = "file"),
  ratio_var = readr::read_csv(ratio_var_list, col_types = cols()),

  tar_target(summation_var_list, "data-raw/calculated_var/summation_variables.csv", format = "file"),
  summation_var = readr::read_csv(summation_var_list, col_types = cols()),

  tar_target(cumulative_var_list, "data-raw/calculated_var/cumulative_variables.csv", format = "file"),
  cumulative_var = readr::read_csv(cumulative_var_list, col_types = cols()),

  tar_target(annual_growth_rate_var_list, "data-raw/calculated_var/annualgrowthrate_variables.csv", format = "file"),
  annual_growth_rate_var = readr::read_csv(annual_growth_rate_var_list, col_types = cols()),

  tar_target(per_diff_var_list, "data-raw/calculated_var/per_diff_variables.csv", format = "file"),
  per_diff_var = readr::read_csv(per_diff_var_list, col_types = cols()),

  tar_target(index_var_list, "data-raw/calculated_var/index_variables.csv", format = "file"),
  index_var = readr::read_csv(index_var_list, col_types = cols()),
  
  tar_target(all_calculated, list(ratio_var = ratio_var,
                                  summation_var=summation_var,
                                  cumulative_var = cumulative_var,
                                  annual_growth_rate_var = annual_growth_rate_var,
                                  per_diff_var = per_diff_var)),

  # _Making data_long ----

  data_loaded = {
    map_dfr(data_files, ~read_process_data_file(.x, config)) %>%
      arrange_standard()},

  data_long = make_data_long(data_loaded),

  data_long_clean = {
    data_long %>%
      complete_implicit_na() %>%
      manual_data_update() %>%
      make_calculated_vars(ratio_var,
                           summation_var,
                           cumulative_var,
                           annual_growth_rate_var,
                           per_diff_var)},

  ### Figure mapping --------------

  tar_map(
    values = figmap_values("figure-maps"),
    tar_target(figmap_csv, figmap_csv_target(file), format = "file"),
    tar_target(figmap, figmap_target(figmap_csv, config))
    )
  
)


