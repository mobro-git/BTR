
check_lts_comp_var = function(data_long_clean, summation_var,settings){
  vars = summation_var %>% 
    filter(variable %in% c("Emissions|CO2|Energy|Demand|Buildings|Total",
                           "Emissions|CO2|Energy|Demand|Industry and Fuel Production|Total",
                           "Emissions|CO2|Energy|Demand|Transportation|Total")) %>% 
    select(lower_level) %>% 
    rename(variable = lower_level)
  
    dat = data_long_clean %>% 
      filter(scenario =="wm") %>% 
      group_by(model,scenario,variable) %>% 
      summarise(value = max(value))
    
    vars2 = vars %>% 
      left_join(dat) %>% 
      complete(variable,model) %>% 
      filter(!is.na(model)) %>% 
      mutate(scenario = "wm",
             value = ifelse(!is.na(value), "reported", "unreported")) %>% 
      select(variable,model,value) %>% 
      spread(model,value) 
    
    write.csv(vars2,paste0("output/",settings$version,"/interim_data/check_lts_var.csv"), row.names = FALSE)
    
    return(vars2)
}

read_ghgi_tables <- function(ghgi_filepath) {
  ghgi_data <- read_csv(ghgi_filepath, skip = 1) %>%
    drop_na() %>% 
    filter(`End-Use Sector` %in% c('Transportation',
                                   'Industrial',
                                   'Residential',
                                   'Commercial')) %>%
    mutate(datasrc = ghgi_filepath)
}


ghgi_nrgco2_xw <- function(ghgi_data, data_long_clean) {
  
  ghgi_long <- ghgi_data %>%
    pivot_longer(cols = !c(`End-Use Sector`,datasrc), names_to = 'year', values_to = 'value') %>%
    mutate(year = as.numeric(year)) %>% 
    mutate(model = 'EPA-GHGI') %>%
    mutate(variable = case_when(`End-Use Sector` == 'Transportation' ~ "Emissions|CO2|Energy|Demand|Transportation|Total",
                                `End-Use Sector` == 'Industrial' ~ "Emissions|CO2|Energy|Demand|Industry and Fuel Production|Total",
                                `End-Use Sector` == 'Residential' ~ "Emissions|CO2|Energy|Demand|Buildings|Total",
                                `End-Use Sector` == 'Commercial' ~ "Emissions|CO2|Energy|Demand|Buildings|Total")) %>%
    group_by(year,variable,model,datasrc) %>%
    summarise(value = sum(value), .groups = 'drop') %>% 
    mutate(unit = "Mt CO2/yr",
           scenario = 'historic',
           region = 'United States') %>%
    select(names(data_long_clean))

  
}

