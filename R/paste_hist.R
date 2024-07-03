
# function to copy historic data for a subset of scenarios
paste_hist = function(data_long_clean_no_hist, ghgi_comp_tab, scens) {
  
  ghgi_new = list()
  
  for (i in 1:length(scens)) {
    ghgi_append = ghgi_comp_tab %>%
      mutate(scenario = scens[i])
    
    ghgi_new[[i]] = ghgi_append
  }
  
  ghgi_new_append = bind_rows(ghgi_new) %>%
    bind_rows(ghgi_comp_tab)
  
  all = bind_rows(data_long_clean_no_hist, ghgi_new_append)
  
  return(all)
  
}