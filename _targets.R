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
    version = "2024_BTR1",
    scen_mapping = read_scen_mapping(crosswalk_model_runs_csv),
    template = template,
    calculated_var = all_calculated,
    base_year = 2022,
    
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

  ##### Template + Crosswalks ---------------------------------------------------

  # BTR reporting template
  tar_target(template_xlsx, "data-raw/template/BTR24_data_template_v1.xlsx", format = "file"),
  tar_target(template, read_emf_template_xlsx(template_xlsx)),
  
  # scenario+model crosswalks
  tar_target(crosswalk_model_runs_csv, "data-raw/crosswalk/crosswalk_model-runs.csv", format = "file"),
  tar_target(crosswalk_usproj_csv, "data-raw/crosswalk/crosswalk_usproj.csv", format = "file"), 

  #### Data Files ----------------------------------------------------------------

  # WM and WAM scenario - BTR template modeling
  tar_target(data_folder, "data-raw/model-runs/", format = "file"),
  tar_target(data_files, dir_ls(data_folder), format = "file"),
  
  # usproj Non-CO2 and CO2 from IPPU and NEU 
  tar_target(usproj_data_folder, "data-raw/usproj-data/", format = "file"),
  tar_target(usproj_files, dir_ls(usproj_data_folder), format = "file"),

  # Past projections and drivers
  tar_target(past_proj_csv, "data-raw/ncbr_comparison/netghg_ncbr_comparisons.csv", format = "file"),
  tar_target(past_proj, read_csv(past_proj_csv)),
  
  tar_target(past_driver_csv, "data-raw/ncbr_comparison/tbl_5-6_drivers_comparison.csv", format = "file"),
  tar_target(past_driver, read_csv(past_driver_csv)),

  #### Data Processing -----------------------

  # _calculated variables ----

  tar_target(ratio_var_list, "data-raw/calculated-var/ratio_variables.csv", format = "file"),
  ratio_var = readr::read_csv(ratio_var_list, col_types = cols()),

  tar_target(summation_var_list, "data-raw/calculated-var/summation_variables.csv", format = "file"),
  summation_var = readr::read_csv(summation_var_list, col_types = cols()),

  tar_target(cumulative_var_list, "data-raw/calculated-var/cumulative_variables.csv", format = "file"),
  cumulative_var = readr::read_csv(cumulative_var_list, col_types = cols()),

  tar_target(annual_growth_rate_var_list, "data-raw/calculated-var/annualgrowthrate_variables.csv", format = "file"),
  annual_growth_rate_var = readr::read_csv(annual_growth_rate_var_list, col_types = cols()),

  tar_target(per_diff_var_list, "data-raw/calculated-var/per_diff_variables.csv", format = "file"),
  per_diff_var = readr::read_csv(per_diff_var_list, col_types = cols()),

  tar_target(index_var_list, "data-raw/calculated-var/index_variables.csv", format = "file"),
  index_var = readr::read_csv(index_var_list, col_types = cols()),
  
  tar_target(all_calculated, list(ratio_var = ratio_var,
                                  summation_var=summation_var,
                                  cumulative_var = cumulative_var,
                                  annual_growth_rate_var = annual_growth_rate_var,
                                  per_diff_var = per_diff_var)),

  # _modeled-data long ----

  data_loaded = {
    map_dfr(data_files, ~read_process_data_file(.x, config)) %>%
      arrange_standard()},

  data_long = make_data_long(data_loaded, config),

  data_long_clean = make_data_long_clean(data_long,
                                         ratio_var,
                                         summation_var,
                                         cumulative_var,
                                         annual_growth_rate_var,
                                         per_diff_var,
                                         config), 
  
  # _usproj data long ----
  
  usproj_data_loaded = {
    map_dfr(usproj_files, ~read_usproj_data_file(.x, crosswalk_usproj_csv)) %>%
      arrange_standard()},
  
  usproj_data_long = make_usproj_data_long(usproj_data_loaded, config),
  
  ### Figure mapping --------------

  tar_map(
    values = figmap_values("figure-maps"),
    tar_target(figmap_csv, figmap_csv_target(file), format = "file"),
    tar_target(figmap, figmap_target(figmap_csv, config))
    ),
  
  ### Outputs ----
  
  tar_render(ncbr_btr_comparison,
             "docs/report/ncbr_btr_comparison.Rmd",
             output_dir = paste0("output/",config$version,"btr_tables_figs"),
             output_file = "ncbr_btr_comparison",
             params = list(mode = "targets"))
  
)


