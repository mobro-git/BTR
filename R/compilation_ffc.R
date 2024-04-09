
# pull ffc co2 emissions data from model-runs and map to usproj accounting framework

get_ffc_model_runs = function(data_long_clean, var_crosswalk, usproj_data_long) {
  
  ffc_raw_data = data_long_clean %>% 
    filter(variable %in% unique(var_crosswalk$btr_var))
  
  ffc_mapped = ffc_raw_data %>%
    rename("btr_var" = "variable") %>%
    left_join(var_crosswalk, by = "btr_var") %>%
    select(names(usproj_data_long), btr_var)
  
}


# add ffc co2 emissions projections from model-runs to usproj_data_long

add_ffc_ghgi = function(ffc_raw_data,usproj_data_long,var_crosswalk,config) {
  
  usproj_no_ffc_proj = usproj_data_long %>%
    # remove FFC projections, keep only historic data - *****EXCEPT KEEP FFCUST PROJECTIONS (not in crosswalk)*****
    filter(!(
      usproj_category %in% unique(var_crosswalk$usproj_category) &
        year > config$base_year)) %>%
    mutate(btr_var = "") %>%
    rbind(ffc_raw_data)
    
}
