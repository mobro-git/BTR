
make_var_xw = function(var_crosswalk_csv, crosswalk_usproj_categories) {
  
  var_xw = read_csv(var_crosswalk_csv) %>%
    left_join(crosswalk_usproj_categories, by = "usproj_category")
  
}