
# pull ffc co2 emissions data from model-runs and map to usproj accounting framework

get_ffc_model_runs = function(data_long_clean, var_crosswalk, usproj_data_long) {
  
  ffc_raw_data = data_long_clean %>% 
    filter(variable %in% unique(var_crosswalk$btr_var))
  
  ffc_mapped = ffc_raw_data %>%
    rename("btr_var" = "variable") %>%
    left_join(var_crosswalk, by = "btr_var") %>%
    select(names(usproj_data_long), btr_var)
  
}


# add ffc co2 emissions projections from model-runs and lulucf sink projections to usproj_data_long

add_ffc_lulucf = function(ffc_raw_data,lulucf_data,usproj_data_long,var_crosswalk,settings) {
  
  usproj_no_ffc_lulucf_proj = usproj_data_long %>%
    # remove FFC and LULUCF projections, keep only historic data - *****EXCEPT KEEP FFCUST PROJECTIONS (not in crosswalk)*****
    filter(!(
      usproj_category %in% unique(var_crosswalk$usproj_category) &
        year > settings$base_year)) %>%
    mutate(btr_var = "") %>%
    rbind(ffc_raw_data)
  
  lulucf_ordered = lulucf_data %>% 
    select(names(usproj_no_ffc_lulucf_proj)) %>% 
    rbind(usproj_no_ffc_lulucf_proj)
    
}


# map proj_name in crosswalk to usproj_all

# map_proj_name <-  function(usproj_all, crosswalk_compilation, config) {
#   
#   #Filter out non-national estimates
#   proj_usa <- usproj_all %>% filter(region == 'United States')
#   
#   
#   # Create empty list
#   proj_names <- list()
#   
#   #Loop through each row of projections
#   for(i in 1:nrow(proj_usa)){
#     
#     row <- proj_usa[i,]
#     
#     model <- row$model
#     scenario <- row$scenario
#     year <- row$year
#     
#     if (year > settings$base_year) {
#       if (model == 'usproj') {
#         proj_names[i] <- 'usproj'
#       }
#       else if (model %in% crosswalk_compilation$ffc_model & scenario %in% crosswalk_compilation$ffc_scen) {
#         merged_row <- row %>% left_join(crosswalk_compilation, by = c('model'='ffc_model', 'scenario'='ffc_scen', 'scenario' = 'usproj_scen'))
#         proj_names[i] <- merged_row$proj_name
#       }
#      # else print(#paste('Model/Scenario combo:', '(',model,', ',scenario,')', ', is not recognized.'))
#     }
#     else proj_names[i] <- 'ghgi_historical'
#   }
#   
#   proj_usa$proj_name <- proj_names
#   
#   return(proj_usa %>% select(proj_name, everything()))
#  
# }

map_proj_name_v2 = function(usproj_all, crosswalk_compilation, settings) {
  
  #Filter out non-national estimates
  proj_usa <- usproj_all %>% 
    filter(region == 'United States' &
             year > settings$base_year)
  
  # shorthand name
  xw = crosswalk_compilation
  
  # checks to make sure all selected components are present in usproj_all
  check = check_combos_present(usproj_all, xw)
  
  # Create empty list
  projections <- list()
  
  #Loop through each row of projections
  for(i in 1:nrow(xw)){
    
    row <- xw[i,]
    
    proj_i_usproj = proj_usa %>%
      filter(model == "usproj" & scenario == row$usproj_scen)
    
    proj_i_ffc = proj_usa %>%
      filter(model == row$ffc_model & scenario == row$ffc_scen)
    
    proj_i_lulucf = proj_usa %>%
      filter(model == row$lulucf_model & scenario == row$lulucf_scen)
               
    proj_i = rbind(proj_i_ffc,proj_i_lulucf,proj_i_usproj) %>%
      mutate(proj_name = row$proj_name,
             grouping = row$grouping) %>%
      select(proj_name, grouping, everything())
    
    projections[[i]] = proj_i
    
  }
  
  projections_all = bind_rows(projections)
  
  write_csv(projections_all, paste0('output/',settings$version,'/proj_tables/projections_all.csv'))
  
  return(projections_all)
  
}

# Add ghgi data to projections

add_historical_data <- function(ghgi_cat, projections_all) {
  
  ghgi_data = ghgi_cat %>%
    mutate(btr_var = "",
           grouping = "ghgi") %>%
    select(names(projections_all))
  
  projections_ghgi = rbind(ghgi_data, projections_all)
  
  
}

# Get summed values for each gas&sector combo, calculate % difference from 2005 levels

gen_proj_all_sm <- function(projections_ghgi, settings){
  
  projections_all_sm_v1 <- projections_ghgi %>% 
    group_by(proj_name, grouping, gas, usproj_sector, year) %>% 
    summarise(sum = sum(value),.groups='drop')
  
  projections_all_sm <- projections_all_sm_v1 %>%
    group_by(gas, usproj_sector) %>%
    mutate(pct_change_05 = round((sum/sum[year==2005]-1),2)) %>% 
    ungroup()
  
  write_csv(projections_all_sm, paste0('output/',settings$version,'/proj_tables/projections_all_sm.csv'))
  
  return(projections_all_sm)

}

# lulucf compilation for btr from data-extra big workbook

make_btr_lulucf_data_raw = function(lulucf_data_extra_xlsx,
                                    lulucf_btr_crosswalk,
                                    settings) {
  
  # pull in LULUCF TABLE FOR PIPELINE sheet from data-extra and transform to lulucf comp format
  data_extra_long = read_xlsx(lulucf_data_extra_xlsx, sheet = "LULUCF TABLE FOR PIPELINE") %>%
    pivot_longer(cols = 6:ncol(.),
                 names_to = "year",
                 values_to = "value") %>%
    filter(year > settings$base_year)
  
  # Create empty list
  projections <- list()
  
  # shorthand name
  xw = lulucf_btr_crosswalk
  
  # Loop through each row of projections
  for (i in 1:nrow(xw)) {
    row <- xw[i, ]
    
    proj_i_csc = data_extra_long %>%
      filter(model == row$csc_model & scenario == row$scenario)
    
    proj_i_ch4 = data_extra_long %>%
      filter(model == row$ch4_model & scenario == row$scenario & str_detect(variable, "CH4"))
    
    proj_i_n2o = data_extra_long %>%
      filter(model == row$n2o_model & scenario == row$scenario & str_detect(variable, "N2O"))
    
    proj_i = rbind(proj_i_csc, proj_i_ch4, proj_i_n2o) %>%
      mutate(lulucf_name = row$lulucf_name,
             grouping = row$grouping) %>%
      select(lulucf_name, grouping, everything())
    
    projections[[i]] = proj_i
    
  }
  
  lulucf_projections_all = bind_rows(projections)
  
  return(lulucf_projections_all)
  
}

# sum btr lulucf carbon stock change, ch4, and n2o breakouts into net total lulucf
make_btr_lulucf_net_total = function(lulucf_btr_data_raw_breakouts) {
  
  lulucf_btr_data_raw_sum = lulucf_btr_data_raw_breakouts %>%
    group_by(lulucf_name, scenario, region, unit, year) %>%
    summarise(value = sum(value),
              variable = "Emissions|LULUCF|Net Total") %>%
    rename(model = lulucf_name) %>%
    select(model, scenario, year, value, region, unit, variable)
  
  write_csv(lulucf_btr_data_raw_sum, "data-raw/lulucf/lulucf_btr_data_from_data-extra.csv")
  
}

check_combos_present = function(usproj_all, xw) {
  
  # unique model/scenario combos in usproj_all to match against
  all_combos = rbind(
    distinct((usproj_all %>% filter(model == "usproj")),model,scenario),
    distinct((usproj_all %>% filter(usproj_category %in% c("FFCTrn", "FFCCom", "FFCRes","FFCElc","FFCInd"))),model,scenario),
    distinct((usproj_all %>% filter(usproj_category == "LULUCF Sink")),model,scenario)
  )
  
  # unique model/scenario combos listed in the xw that need to be present in all_combos
  needed_combos = rbind(
    distinct((select(xw, usproj_scen) %>% mutate(model = "usproj") %>% rename(scenario=usproj_scen) %>% select(model,scenario))),
    distinct((select(xw,ffc_model,ffc_scen) %>% rename(model=ffc_model, scenario=ffc_scen) %>% select(model,scenario))),
    distinct((select(xw,lulucf_model,lulucf_scen) %>% rename(model=lulucf_model, scenario=lulucf_scen) %>% select(model,scenario)))
  )
  
  diff = needed_combos %>% anti_join(all_combos)
  
  if (nrow(diff) > 0) {
    print(diff)
    rlang::abort(message = paste("Model/scenario combinations in the crosswalk are not present in usproj_all. Please check that you've selected appropriate component model/scenario combinations in the crosswalk", sep = ""),
                 class = 'compilation_combos_check')
  }
  
}



