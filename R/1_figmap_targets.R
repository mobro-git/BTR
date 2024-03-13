
# creates list of files to be tracked in figmap_csv targets
figmap_mapping_values <- function(folder) {
  
  files = tibble(file = list.files(folder)) %>%
    mutate(file = paste0(folder,"/",file))
  files
  
}

# creates figmap csv paths based on mapping csv
figmap_csv_target <- function(file) {

  name = str_replace(file_path_sans_ext(file),"figure_mapping/","")
  name
  
}


