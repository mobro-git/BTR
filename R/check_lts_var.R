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
             value = ifelse(!is.na(value), "X", 0)) %>% 
      select(variable,model,value) %>% 
      spread(model,value) 
    
      write.csv(vars2,paste0("output/",settings$version,"/interim_data/check_lts_var.csv"), row.names = FALSE)
    
    return(vars2)
}
