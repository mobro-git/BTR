full_kaya_comparison <- function(past_kaya_no_emissions,past_proj,total_gross_emissions,data_long_clean) {
  test = past_kaya_no_emissions %>% 
    mutate(unit = ifelse(variable == "gdp",paste0(unit," - ",version),unit)) %>% 
    gather(year,value,6:66) %>% 
    select(-notes,-version) %>% 
    rbind(past_proj %>% 
            gather(year,value,2:20) %>% 
            mutate(variable = "emissions",
                   unit = "MTCO2e") %>% 
            rename(br_version = Projection)) %>% 
    mutate(scenario = "single") %>% 
    rbind(total_gross_emissions %>%
            filter(!proj_name %in% c("gcam_ira_lowseq","usrr_ira_lowseq","nems_ira_lowseq")) %>% 
            group_by(year) %>% 
            summarise(max = max(value),
                      min = min(value)) %>% 
            ungroup() %>% 
            filter(year %in% c(seq(1990,2022,1),seq(2025,2050,5))) %>% 
            gather(scenario,value,2:3) %>% 
            mutate(br_version = "2024 BR",
                   variable = "emissions",
                   unit = "MTCO2e")) %>% 
    rbind(data_long_clean %>% 
            filter(scenario == "wm",
                   variable == "Final Energy",
                   year %in% c(seq(2020,2050,5))) %>% 
            group_by(unit,year,variable) %>% 
            summarise(max = max(value),
                      min = min(value)) %>% 
            ungroup() %>% 
            gather(scenario,value,4:5) %>% 
            mutate(variable = "energy",
                   br_version = "2024 BR")) %>% 
    rbind(data_long_clean %>% 
            filter(scenario == "wm",
                   variable %in% c("GDP|MER", "Population"),
                   model == "OP-NEMS",
                   year %in% c(seq(2020,2050,5))) %>% 
            select(unit,year,variable,value) %>% 
            mutate(br_version = "2024 BR",
                   scenario = "max") %>% 
            rbind(data_long_clean %>% 
                    filter(scenario == "wm",
                           variable %in% c("GDP|MER", "Population"),
                           model == "OP-NEMS",
                           year %in% c(seq(2020,2050,5))) %>% 
                    select(unit,year,variable,value) %>% 
                    mutate(br_version = "2024 BR",
                           scenario = "min")) %>% 
            mutate(variable = gsub("GDP\\|MER","gdp",variable))) %>% 
    select(-unit) %>% 
    spread(variable,value)
    
  # pulls in the past br kaya stuff for energy, gdp, population from past_kaya_no_emissions -- DONE MZ
  # adds the total gross emissions variable from past_proj -- DONE MZ
  # adds the projected total gross emissions from total_gross_emissions -- DONE MZ
  # adds the projected energy from data_long_clean as a range ("Final Energy") -- DONE MZ
  # adds the projected gdp and pop from OP-NEMS (data_long_clean) -- DONE MZ
  # copies the historical energy, gdp, population from the br5 scenario to btr1 scenario
  # calculates the kaya identity variables
  
  ### Then tar_load() this target in btr_tables_figs and create the figure there
  
}