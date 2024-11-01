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
  
  ##### Config and settings -----------------
  
  settings = list(
    version = "2024_BTR1",
    base_year = 2022,
    scen_mapping = read_scen_mapping(crosswalk_model_runs_csv),
    template = template,
    calculated_var = all_calculated
    # TODO: Add end_year to settings
  ), 
  
  config = list(
    # models
    model_wm = c("GCAM","OP-NEMS","USREP-ReEDS"),
    model_lts = c("GCAM-LTS","OP-NEMS-LTS"),
    model_hist = "EPA-GHGI",
    model_wm_hist = c("GCAM","OP-NEMS","USREP-ReEDS","EPA-GHGI"),
    
    # scenarios
    ghgi_scen = "wm", # Set usproj scenario to pull ghgi data
    scen_wm = c("wm"), #, "leep_IRA"
    scen_wm_hist = c("wm","Historic"),
    scen_wm_ira = c("wm","leep_IRA"),
    scen_wm_sensitivities = c("wm", 
                              "wm_adv", # OP-NEMS
                              "wm_highfuelcost", # OP-NEMS, GCAM
                              "wm_lowfuelcost", # OP-NEMS, GCAM, USREP-ReEDS
                              "wm_lowcostre_highfuelcost"), #GCAM, USREP-ReEDS
    
    # regions
    usa = "United States",

    # years
    fives = c(seq(2005,settings$base_year,by = 1),seq(2025,2040,by = 5)),
    fives50 = c(seq(2005,settings$base_year,by = 1),seq(2025,2050,by = 5)),
    fives_sumtab = c(seq(2005,2020,by = 5),settings$base_year,seq(2025,2040,by = 5)),
    annual = c(seq(2005,2040,by = 1)),
    hist = c(seq(2005,settings$base_year, by = 1)),
    seq_ncbr_comp = c(seq(1990,2020,by=5)),
    seq_ncbr_comp_sm = c(seq(2005,2040,by=5)),
    seq_ncbr_comp_sm50 = c(seq(2005,2050,by=5)),
    
    
    fives_lts = c(seq(2005,settings$base_year,by = 1),seq(2025,2050,by = 5)),
    annual_lts = c(seq(2005,2050,by = 1)),
    
    fives_proj = c(seq(2020,2050, by = 5)),
    fives_proj_40 = c(seq(2020,2040, by = 5)),
    fives_proj_sm = c(seq(2025,2040, by = 5)),
    fives_proj_sm50 = c(seq(2025,2050, by = 5)),
    
    kaya = c(seq(1990,settings$base_year,by=1),seq(2025,2050,by = 5)),
    
    annual_proj = c(seq(2020,2050, by = 1)),
    last_proj = 2040,
    base_proj = c(settings$base_year,seq(2025,2050, by = 5)),
    base_proj_20 = c(2020,settings$base_year,seq(2025,2050, by = 5)),
    
    annual_1990 = c(seq(1990,2040,by = 1)),
    annual_1990_fives = c(seq(1990,settings$base_year,by = 1), seq(2025,2040, by = 5)),
    annual_1990_fives50 = c(seq(1990,settings$base_year,by = 1), seq(2025,2050, by = 5)),
    annual_2010 = c(seq(2010,2040,by = 1)),
    table = c(seq(2005, 2020, by=5),settings$base_year,seq(2025,2040,by=5)),
    table_sm = c(2005, 2010, 2015, 2020, settings$base_year, 2025, 2030 , 2035),
    table50 = c(seq(2005, 2020, by=5),settings$base_year,seq(2025,2050,by=5)),
    
    # ordering
    gas_order = c("CO2", "CH4", "N2O", "HFCs", "PFCs", "SF6", "NF3"),
    sector_order = c("Energy","Transportation","IPPU","Agriculture","Waste")#,"LULUCF") # Uncomment if not netting out all LULUCF emissions
  ),
  
  ##### Template + Crosswalks ---------------------------------------------------
  
  # BTR reporting template
  tar_target(template_original_xlsx, "data-raw/template/EMF37_data_template_R2_v2.xlsx", format = "file"),
  tar_target(template_original, read_emf_template_xlsx(template_original_xlsx)),
  
  tar_target(template_additions_csv, "data-raw/template/template_additions.xlsx", format = "file"),
  tar_target(template_additions, read_emf_template_xlsx(template_additions_csv)),
  
  tar_target(template, rbind(template_original,template_additions)),
  
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
  tar_target(past_proj_csv, "data-raw/ncbr_comparison/total_gross_ghg_ncbr_comparisons_ar5.csv", format = "file"),
  tar_target(past_proj, read_csv(past_proj_csv)),
  
  tar_target(past_driver_csv, "data-raw/ncbr_comparison/tbl_5-6_drivers_comparison.csv", format = "file"),
  tar_target(past_driver, read_csv(past_driver_csv)),
  
  tar_target(past_kaya_no_emissions_xlsx, "data-raw/ncbr_comparison/kaya_comparison_2024_btr1_ar5.xlsx", format = "file"),
  tar_target(past_kaya_no_emissions, read_xlsx(past_kaya_no_emissions_xlsx)),
  
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
  
  # _usproj data long ----
  
  usproj_data_loaded = {
    map_dfr(usproj_files, ~read_usproj_data_file(.x, crosswalk_usproj_csv)) %>%
      arrange_standard()},
  
  usproj_data_long_all = make_usproj_data_long(usproj_data_loaded, settings), 
  
  # usproj w/o historical data
  usproj_data_long = gen_usproj_projections(usproj_data_long_all, settings), 
  
  # _ghgi data 
  
  #      from usproj
  ghgi_cat = gen_usproj_ghgi(usproj_data_long_all, config, settings),
  
  #      from compiled_tables_2024
  tar_target(ghgi_comp_tab_csv, "data-raw/ghgi/EPA-GHGI_wide_wnotes_2024.csv", format = "file"),
  
  ghgi_comp_tab = {
    read_csv(ghgi_comp_tab_csv) %>%
      mutate(datasrc = ghgi_comp_tab_csv) %>%
      pivot_longer(cols = `1990`:`2022`, names_to = "year", values_to = "value") %>%
      mutate(year = as.numeric(year)) %>%
      select(names(data_long_clean_no_hist))
    },
  
  # _modeled-data long ----
  
  data_loaded = {
    map_dfr(data_files, ~read_process_data_file(.x, settings)) %>%
      arrange_standard()},

  data_long = make_data_long(data_loaded, settings),
  
  data_long_clean_no_hist = make_data_long_clean(data_long,
                                                 ratio_var,
                                                 summation_var,
                                                 cumulative_var,
                                                 annual_growth_rate_var,
                                                 per_diff_var,
                                                 config,
                                                 settings), 
  
  data_long_clean = paste_hist(data_long_clean_no_hist, ghgi_comp_tab, c("wm","leep_IRA")),
  
  # indexed version of data_long_clean. index_var determines which variables are indexed, only these are included
  data_long_index = index_data_long(data_long_clean, index_var),
  
  # KAYA Compilation ----
  
  kaya_comparison = full_kaya_comparison(past_kaya_no_emissions,past_proj,total_gross_emissions,data_long_clean),
  kaya_comparison50 = full_kaya_comparison50(past_kaya_no_emissions,past_proj,total_gross_emissions,data_long_clean),
  
  # LULUCF ----
  tar_target(lulucf_data_extra_xlsx, "data-extra/USDA NFS Raw Data/LULUCF projections DRAFT compilation 10 23 2024.xlsx", format = "file"),
  
  tar_target(lulucf_btr_crosswalk_csv, "data-raw/crosswalk/crosswalk_lulucf_btr.csv", format = "file"),
  lulucf_btr_crosswalk = read_csv(lulucf_btr_crosswalk_csv),
  
  lulucf_btr_data_raw_breakouts = make_btr_lulucf_data_raw(lulucf_data_extra_xlsx,lulucf_btr_crosswalk,settings), 
  # TODO: add check to make sure that all model-scenario combos are accounted for (e.g. the check we have in read_process_data_file that creates the models-runs-crosswalk additions file)
  
  lulucf_btr_data_raw = make_btr_lulucf_net_total(lulucf_btr_data_raw_breakouts),
  
  tar_target(lulucf_folder, "data-raw/lulucf/", format = "file"),
  tar_target(lulucf_files_with_check, list_lulucf_files(lulucf_folder,lulucf_btr_data_raw)),
  tar_target(lulucf_files, lulucf_files_with_check, format = "file"),
  
  tar_target(lulucf_crosswalk_csv, "data-raw/crosswalk/crosswalk_lulucf_all.csv", format = "file"),
  tar_target(lulucf_crosswalk, read_csv(lulucf_crosswalk_csv)), 
  
  lulucf_data = {
    map_dfr(lulucf_files, ~read_lulucf_data_file(.x, lulucf_crosswalk, var_crosswalk)) %>%
      arrange_standard()}, 
  
  ### Projections Compilation --------------
  
  # crosswalk between BTR and usproj template variables
  tar_target(var_crosswalk_csv, "data-raw/crosswalk/crosswalk_var.csv", format = "file"),
  tar_target(var_crosswalk, read_csv(var_crosswalk_csv)), # TODO: CHECK FOR VARIABLES!
  
  ffc_raw_data = get_ffc_model_runs(data_long_clean, var_crosswalk, usproj_data_long),
  
  usproj_all = add_ffc_lulucf(ffc_raw_data, lulucf_data, usproj_data_long, var_crosswalk, settings),
  
  # crosswalk compilation
  tar_target(crosswalk_compilation_csv, "data-raw/crosswalk/crosswalk_compilation.csv", format = "file"),
  tar_target(crosswalk_compilation, read_csv(crosswalk_compilation_csv)), # TODO: CHECK FOR MODELS AND SCENARIOS!
  
  # _complete projections ----
  projections_all = map_proj_name_v2(usproj_all, crosswalk_compilation, settings),
  projections_ghgi = add_historical_data(ghgi_cat, projections_all), # bind ghgi historical data to projections
  projections_all_sm = gen_proj_all_sm(projections_ghgi, settings), # gas and sector sums for each projection

  # _summary table breakouts ----
  lulucf_sink_breakout = gen_lulucf_sink_breakout(projections_all_sm, config, settings),
  
  gas_dataset = gen_gas_dataset(projections_all_sm, config),
  gas_breakout = gen_gas_breakout(gas_dataset, config, settings, category_order = config$gas_order),
  
  sector_dataset = gen_sector_dataset(projections_all_sm, config),
  sector_breakout = gen_sector_breakout(sector_dataset, config, settings, category_order = config$sector_order),
  
  # Sum Total Gross Emissions ----
  tar_target(total_gross_emissions, gen_total_gross_emissions(gas_dataset, config)),
  
  # Calculate Total Net Emissions and write ----
  tar_target(total_net_emissions, gen_total_net_emissions(gas_dataset, lulucf_sink_breakout, settings, config)),
  
  ### QA/QC ----
  
  check_nrg_excl_trn_acct = {
    nrg_excl_trn_acct <- data_long_clean %>% 
      filter(variable %in% c('BTR|Emissions|CO2|Energy excl TRN Subtract','BTR|Emissions|CO2|Energy excl TRN Sum'))%>% 
      pivot_wider(names_from = variable) %>%
      mutate(diff = `BTR|Emissions|CO2|Energy excl TRN Sum` - `BTR|Emissions|CO2|Energy excl TRN Subtract`)
  },
  
  check_primary_nrg_df = check_primary_nrg_df(data_long_clean,config),
  check_primary_nrg_fig = check_primary_nrg_fig(check_primary_nrg_df),
  
  check_lts_comp_var = check_lts_comp_var(data_long_clean,summation_var,settings),
  
  ### Figure mapping --------------
  
  tar_map(
    values = figmap_values("figure-maps"),
    tar_target(figmap_csv, figmap_csv_target(file), format = "file"),
    tar_target(figmap, figmap_target(figmap_csv, config, settings))
  ),
  
  ### Outputs ----
  
  # figure map outputs
  nrgco2_sb = create_graph("nrgco2", "stacked_bar", config, settings, data_long_clean, figmap_nrgco2_stackbar),
  nrgco2_db = create_graph("nrgco2", "diff_bar", config, settings, data_long_clean, figmap_nrgco2_diffbar),
  nrgco2_ts = create_graph("nrgco2", "time_series", config, settings, data_long_clean, figmap_nrgco2_timeseries),
  nrgco2_cu = create_graph("nrgco2", "cone_uncertainty", config, settings, data_long_clean, figmap_nrgco2_cone),
  
  kaya_ts = create_graph("Kaya", "time_series", config, settings, data_long_index, figmap_kaya_timeseries),
  leepcompare_ts = create_graph("leepcompare", "time_series", config, settings, data_long_clean, figmap_leepcompare_timeseries),
  
  # sensitivity figure maps
  sens_ts = create_graph("sens", "time_series", config, settings, data_long_clean, figmap_sens_timeseries),
  sens_sb = create_graph("sens", "stacked_bar", config, settings, data_long_clean, figmap_sens_stackbar),
  sens_db = create_graph("sens", "diff_bar", config, settings, data_long_clean, figmap_sens_diffbar),
  
  # TODO: remove variables we dont have historical data for
  # TODO: add historical data from EIA for energy variables
  # TODO: create versions of the stacked bar figures that have historical data for 2020
  nrgco2hist = create_graph("nrgco2hist", "time_series", config, settings, data_long_clean, figmap_nrgco2hist_timeseries),
    
  # markdowns
  tar_render(btr_tables_figs,
             "docs/report/btr_tables_figs.Rmd",
             output_dir = paste0('output/',settings$version,"/tables_figs/"),
             output_file = paste0("btr_tables_figs_",Sys.Date(),".html"),
             params = list(mode = "targets")),
  
  tar_render(results_overview,
             "docs/report/results_overview.Rmd",
             output_dir = paste0('output/',settings$version,"/results_overview/"),
             output_file = paste0("results_overview_",Sys.Date(),".html"),
             params = list(mode = "targets")),
  
  tar_render(ncbr_comp_brvs,
            "docs/report/ncbr_comp_brvs.Rmd",
            output_dir = paste0('output/',settings$version,"/results_overview/"),
            output_file = paste0("ncbr_comp_brvs_",Sys.Date(),".html"),
            params = list(mode = "targets")),
  
  tar_render(leepcompare,
             "docs/report/leep_comparison_nrgco2.Rmd",
             output_dir = paste0('output/',settings$version,"/results_overview/"),
             output_file = paste0("leepcompare_",Sys.Date(),".html"),
             params = list(mode = "targets")),
  
  #TODO: fix the 2020-2022 sector analysis figures that go out to 2050
  tar_render(btr_tables_figs_to2050,
             "docs/report/btr_tables_figs_to2050.Rmd",
             output_dir = paste0('output/',settings$version,"/tables_figs/to2050/"),
             output_file = paste0("btr_tables_figs_to2050_",Sys.Date(),".html"),
             params = list(mode = "targets")),
  
  tar_render(tge_breakouts,
             "docs/report/tge_breakouts.Rmd",
             output_dir = paste0('output/',settings$version,"/tables_figs/tge_breakouts/"),
             output_file = paste0("btr_tge_breakouts_",Sys.Date(),".html"),
             params = list(mode = "targets")),
  
  tar_render(results_overview_sens,
             "docs/report/results_overview_sens.Rmd",
             output_dir = paste0('output/',settings$version,"/results_overview_sens/"),
             output_file = paste0("results_overview_sens",Sys.Date(),".html"),
             params = list(mode = "targets")),
  
   tar_render(btr_tables_figs_sens,
              "docs/report/btr_tables_figs_sens.Rmd",
              output_dir = paste0('output/',settings$version,"/tables_figs_sens"),
              output_file = paste0("btr_tables_figs_sens",Sys.Date(),".html"),
              params = list(mode = "targets"))
  
)
