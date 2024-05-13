
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

add_ffc_lulucf = function(ffc_raw_data,lulucf_data,usproj_data_long,var_crosswalk,config) {
  
  usproj_no_ffc_lulucf_proj = usproj_data_long %>%
    # remove FFC and LULUCF projections, keep only historic data - *****EXCEPT KEEP FFCUST PROJECTIONS (not in crosswalk)*****
    filter(!(
      usproj_category %in% unique(var_crosswalk$usproj_category) &
        year > config$base_year)) %>%
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
#     if (year > config$base_year) {
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

map_proj_name_v2 = function(usproj_all, crosswalk_compilation, config, settings) {
  
  #Filter out non-national estimates
  proj_usa <- usproj_all %>% 
    filter(region == 'United States' &
             year > config$base_year)
  
  # Create empty list
  projections <- list()
  
  # shorthand name
  xw = crosswalk_compilation
  
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
