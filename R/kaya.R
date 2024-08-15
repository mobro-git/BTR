full_kaya_comparison <- function(past_kaya_no_emissions,past_proj,total_gross_emissions,data_long_clean) {

  BaseYear = 2015
  #Old BRs
  Old_kayas = past_kaya_no_emissions %>% 
    mutate(unit = ifelse(variable == "gdp",paste0(unit," - ",version),unit)) %>% 
    gather(year,value,6:66) %>% 
    select(-notes,-version,-unit) %>% 
    mutate(br_version = substr(br_version,1,4)) %>% 
    rbind(past_proj %>% 
            select(br_version = Report, year=Year,value = Emissions) %>% 
            mutate(variable = "emissions",
                   br_version = sub(".*_", "", br_version))) %>% 
    spread(variable,value) %>% 
    #calculate Kaya values
    mutate(EmissPerEnergy = emissions/energy,
           EnergyPerGDP = energy/gdp,
           GDPPerCap = gdp/Population) %>% 
    select(-emissions,-energy,-gdp) %>% 
    gather(variable,value,3:6) %>% 
    filter(br_version != 2010) %>% 
    mutate(scenario = "single") %>% 
    na.omit(EmissPerEnergy)
  
  #Current year BR
  
  #calculate growth rate of final Energy in model runs
  Grow_rate = data_long_clean %>% 
    filter(scenario == "wm",
           variable == "Final Energy",
           year %in% c(seq(2020,2050,5))) %>%
    left_join(data_long_clean %>% 
                filter(scenario == "wm",
                       variable == "Final Energy",
                       year == 2020) %>% 
                select(-year) %>% 
                rename(base = value)) %>% 
    mutate(GR = value/base) %>% 
    select(model,year,GR)
    
  #Apply Growth rate to 2022 BR"s 2020 to calculate project final energy
  BR2024_Data = Grow_rate %>% 
    cross_join(past_kaya_no_emissions %>% 
                gather(year,value,6:66) %>% 
                select(-notes,-version) %>% 
                filter(br_version == "2022 BR",
                       variable == "energy",
                       year ==2020) %>% 
                select(value)) %>% 
    mutate(new_value = value*GR) %>% 
    select(model,year,value = new_value) %>% 
    mutate(variable = "energy",
           br_version = "2024") %>% 
    filter(year>2020)%>% 
    #Add on historical GDP, Pop and Energy Data
    rbind(data.frame(model = c("GCAM","OP-NEMS","USREP-ReEDS")) %>% 
            cross_join(past_kaya_no_emissions %>% 
                         filter(br_version =="2022 BR"))  %>% 
            gather(year,value,7:67) %>% 
            select(-notes,-version, -unit) %>% 
            mutate(br_version = "2024") %>%
            filter(year < 2023)) %>% 
    #Add projected emissions
    rbind(total_gross_emissions %>%
            filter(proj_name %in% c("gcam_wm_lowseq","usrr_wm_lowseq","nems_wm_lowseq")) %>% 
            filter(year>2024) %>% 
            mutate(proj_name = gsub("gcam_wm_lowseq","GCAM", proj_name),
                   proj_name = gsub("usrr_wm_lowseq","USREP-ReEDS", proj_name),
                   proj_name = gsub("nems_wm_lowseq","OP-NEMS", proj_name)) %>% 
            filter(year %in% c(seq(2025,2050,5))) %>% 
            rename(model = proj_name) %>%
            #Add historical emissions from GHGI
            rbind(data.frame(model = c("GCAM","OP-NEMS","USREP-ReEDS")) %>% 
                    cross_join(total_gross_emissions %>%
                                 filter(proj_name %in% c("ghgi"))) %>% 
                    select(-proj_name)) %>% 
            mutate(br_version = "2024",
                   variable = "emissions") %>% 
            select(-source)) %>% 
    #Add projected GDP and Pop from NEMS %>% 
    rbind(data.frame(model = c("GCAM","OP-NEMS","USREP-ReEDS")) %>% 
            cross_join(data_long_clean %>% 
                         filter(scenario == "wm",
                                variable %in% c("GDP|MER", "Population"),
                                model == "OP-NEMS",
                                year %in% c(seq(2025,2050,5))) %>% 
                         select(unit,year,variable,value) %>% 
                         mutate(br_version = "2024")) %>% 
            mutate(variable = gsub("GDP\\|MER","gdp",variable)) %>% 
            #Convert 2018 Dollar to 2012 Dollar (https://data.bls.gov/cgi-bin/cpicalc.pl?cost1=1&year1=201801&year2=201201)
            mutate(value = ifelse(variable == "gdp", value*0.91,value)) %>% 
            select(-unit)) %>% 
    spread(variable,value) %>% 
    #calculate Kaya values
    mutate(EmissPerEnergy = emissions/energy,
           EnergyPerGDP = energy/gdp,
           GDPPerCap = gdp/Population) %>% 
    select(-emissions,-energy,-gdp) %>% 
    gather(variable,value,4:7) %>% 
    group_by(year,br_version,variable) %>% 
    #Get max and min for each year
    summarise(max = max(value),
              min = min(value)) %>% 
    ungroup() %>% 
    filter(year<2045) %>% 
    gather(scenario,value,4:5)
  
  
  kaya_data = Old_kayas %>%
    rbind(BR2024_Data) %>% 
    left_join(Old_kayas %>%
                rbind(BR2024_Data) %>% 
                filter(year == BaseYear) %>% 
                rename(base = value) %>% 
                select(-year)) %>% 
    mutate(indexed = value/base, 
           year = as.numeric(year))
    
    
  # pulls in the past br kaya stuff for energy, gdp, population from past_kaya_no_emissions -- DONE MZ
  # adds the total gross emissions variable from past_proj -- DONE MZ
  # adds the projected total gross emissions from total_gross_emissions -- DONE MZ
  # adds the projected energy from data_long_clean as a range ("Final Energy") -- DONE MZ
  # adds the projected gdp and pop from OP-NEMS (data_long_clean) -- DONE MZ
  # copies the historical energy, gdp, population from the br5 scenario to btr1 scenario
  # calculates the kaya identity variables
  
  ### Then tar_load() this target in btr_tables_figs and create the figure there
  
}
