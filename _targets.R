# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) 

source("packages.R")

# Set target options:
tar_option_set(
  packages = c("dplyr","readr","tidyverse") # packages to make available to targets
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Replace the target list below with your own:
list(
  
  
<<<<<<< HEAD
  ##### Config -----------------
  config = list(
    scen_mapping = read_scen_mapping(scen_mapping_csv),
    template = template,
    calculated_var = all_calculated,
    
    # models
    all_models = c("GCAM","USREP-ReEDS","OP-NEMS","Non-CO2","LULUCF"),
    
    # scenarios
    sn_lulucf = c("lulufc_hi","lulucf_low"),
    
    # time intervals
    fives = c(seq(2005,2022,by = 1),seq(2025,2035,by = 5)),
    ones = c(seq(2005,2035,by = 1))
  ),
  
  ######################################################################################### -
  ######################################################################################### -
  
  ##### Template and Metadata ---------------------------------------------------
  
  tar_target(template_csv, "data-raw/EMF37_data_template_R2_v2.xlsx", format = "file"),
  tar_target(template, read_emf_template_xlsx(template_csv)),
  
  tar_target(scen_mapping_csv, "data-raw/scenario-mapping.csv", format = "file"),
  
  ######################################################################################### -
  ######################################################################################### -
  
  #### Data Files ----------------------------------------------------------------
  
  tar_target(data_folder, path("data-raw", "model-runs"), format = "file"),
  tar_target(data_files, dir_ls(data_folder), format = "file"),
  
  ####################################################################################### -
  ######################################################################################### -
  
  ### Data Processing -----------------------
  
  # _Calculated variables ----
  
  tar_target(ratio_var_list, "data-raw/process_data/ratio_variables.csv", format = "file"),
  ratio_var = readr::read_csv(ratio_var_list, col_types = cols()),
  
  tar_target(summation_var_list, "data-raw/process_data/summation_variables.csv", format = "file"),
  summation_var = readr::read_csv(summation_var_list, col_types = cols()),
  
  tar_target(cumulative_var_list, "data-raw/process_data/cumulative_variables.csv", format = "file"),
  cumulative_var = readr::read_csv(cumulative_var_list, col_types = cols()),
  
  tar_target(annual_growth_rate_var_list, "data-raw/process_data/annualgrowthrate_variables.csv", format = "file"),
  annual_growth_rate_var = readr::read_csv(annual_growth_rate_var_list, col_types = cols()),
  
  tar_target(per_diff_var_list, "data-raw/process_data/per_diff_variables.csv", format = "file"),
  per_diff_var = readr::read_csv(per_diff_var_list, col_types = cols()),
  
  tar_target(index_var_list, "data-raw/process_data/index_variables.csv", format = "file"),
  index_var = readr::read_csv(index_var_list, col_types = cols()),
  
  tar_target(all_calculated, list(ratio_var = ratio_var,
                                  summation_var=summation_var,
                                  cumulative_var = cumulative_var,
                                  annual_growth_rate_var = annual_growth_rate_var,
                                  per_diff_var = per_diff_var)),
  
  tar_target(all_calculated_var, c(
    unique(all_calculated$ratio_var$variable),
    unique(all_calculated$summation_var$variable),
    unique(all_calculated$cumulative_var$new_variable),
    unique(all_calculated$annual_growth_rate_var$new_variable),
    unique(all_calculated$per_diff_var$new_variable))
  ),
  
  # _Making data_long ----
  data_raw = map_dfr(data_files, read_raw_data_file),
  
  data_min = map_dfr(data_files, read_process_minimal_from_raw),
  
  # TODO: refactor for new setup once we build data stream
  omitted_var = check_omissions(data_raw, data_long, template_original, template),
  
  omitted_data = {data_raw %>% filter(variable %in% omitted_var)},
  
  data_long_read = {
    map_dfr(data_files, ~read_process_data_file(.x, config)) %>%
      # map_variable_names() %>%
      arrange_standard()},
  
  data_long = make_data_long(data_long_read),
  
  # data_long but can add in transformations or filter out models/variables
  # TODO: add bistline % electricity variables back in here
  clean_data = {
    data_long %>%
      unit_conversion() %>%
      complete_implicit_na() %>%
      make_clean_data() %>%
      make_calculated_vars(ratio_var, summation_var, cumulative_var, annual_growth_rate_var, per_diff_var)},
  
  data_output = write_csv(clean_data, "output/data/leep_data_output.csv"),
  
  data_wide = {clean_data %>% pivot_wider(names_from = "year", values_from = "value")},
  
  # indexed version of clean_data. index_var determines which variables are indexed, only these are included
  clean_data_index = index_data_long(clean_data, index_var),
  
  ######################################################################################### -
  ######################################################################################### -
  
  ### Plot Mapping CSVs --------------
  
  tar_map(
    values = figmap_list,
    tar_target(figmap_csv, figmap_csv_path(fig_subject, fig_type), format = "file"),
    tar_target(figmap, import_figure_csv(figmap_csv, fig_type, config))
  ),
  
  
  #### Reports ----------------------------
  
  tar_render(car_tables_figs_report,
             "docs/report/btr-tables-figs-2024.Rmd",
             output_dir = path(config_2022$report_output_root, "car-tables-figs"),
             output_format = "all", # Change to "all" or "rtf_document" for different output types
             output_file = "btr-tables-figs",
             params = list(
               config = "config_2022", mode="targets")),
  
  
  ######################################################################################### -
  ######################################################################################### -
  
  ### Figures  -------------------
  
  # Final Figures
  # 
  # tar_render(
  #   chapter1,
  #   "docs/final figures/chapter1.Rmd",
  #   output_dir = "output/final_figures/rmd",
  #   output_file = "chapter1",
  #   envir = rlang::global_env(), ## hacky way to get rmarkdown to render in the global environ and not create its own
  #   params = list(
  #     mode = "targets"),
  # ),
  # 
  # tar_render(
  #   chapter2,
  #   "docs/final figures/chapter2.Rmd",
  #   output_dir = "output/final_figures/rmd",
  #   output_file = "chapter2",
  #   envir = rlang::global_env(), ## hacky way to get rmarkdown to render in the global environ and not create its own
  #   params = list(
  #     mode = "targets"),
  # ),
  # 
  # tar_render(
  #   chapter3,
  #   "docs/final figures/chapter3.Rmd",
  #   output_dir = "output/final_figures/rmd",
  #   output_file = "chapter3",
  #   envir = rlang::global_env(), ## hacky way to get rmarkdown to render in the global environ and not create its own
  #   params = list(
  #     mode = "targets"),
  # ),
  # 
  # tar_render(
  #   chapter4,
  #   "docs/final figures/chapter4.Rmd",
  #   output_dir = "output/final_figures/rmd",
  #   output_file = "chapter4",
  #   envir = rlang::global_env(), ## hacky way to get rmarkdown to render in the global environ and not create its own
  #   params = list(
  #     mode = "targets"),
  # ),
  # 
  # tar_render(
  #   chapter5,
  #   "docs/final figures/chapter5.Rmd",
  #   output_dir = "output/final_figures/rmd",
  #   output_file = "chapter5",
  #   envir = rlang::global_env(), ## hacky way to get rmarkdown to render in the global environ and not create its own
  #   params = list(
  #     mode = "targets"),
  # ),
  # 
  # tar_render(
  #   appendix,
  #   "docs/final figures/appendix.Rmd",
  #   output_dir = "output/final_figures/rmd",
  #   output_file = "appendix",
  #   envir = rlang::global_env(), ## hacky way to get rmarkdown to render in the global environ and not create its own
  #   params = list(
  #     mode = "targets"),
  # ),
  
  # # Plot maps
  # 
  # ts = create_graph("leep", "time_series", config, clean_data, figmap_leep_timeseries),
  # cone = create_graph("leep", "cone_uncertainty", config, clean_data, figmap_leep_cone),
  # stackbar = create_graph("leep", "stacked_bar", config, clean_data, figmap_leep_stackbar),
  # diffbar = create_graph("leep", "diff_bar", config, clean_data, figmap_leep_diffbar)
=======
>>>>>>> c28a0830aa727a4e84cc0d6d28f367fc999d0170
  
)
