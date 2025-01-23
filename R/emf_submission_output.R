
# creates and formats output xlsx for BTR energy and emissions submission to EMF 37

emf_submission_output = function(data_long_clean_no_hist, template, config, settings) {
  
  output = data_long_clean_no_hist %>% 
    filter(variable %in% unique(template$variable)) %>%
    select(-datasrc) %>%
    filter(scenario %in% config$scen_wm_sensitivities) %>%
    mutate(scenario = paste0("NT.IRA.Ref.",scenario)) %>%
    mutate(model = case_when(
      model == "GCAM" ~ "GCAM-BTR",
      model == "USREP-ReEDS" ~ "USREP-ReEDS-BTR",
      model == "OP-NEMS" ~ "NEMS-OP-BTR"
    )) %>%
    rename(Model = model,
           Scenario = scenario,
           Region = region,
           Variable = variable,
           Unit = unit) %>%
    pivot_wider(names_from = "year", values_from = "value")
  
  path = paste0('output/',settings$version,"/emf_submission/")
  create_folders(path)
  xlsx::write.xlsx(output, 
                   file = paste0(path,"biennial_transparency_report_NT_Ref_IRA.xlsx"),
                   sheetName = "data")
  
  return(output)
  
}