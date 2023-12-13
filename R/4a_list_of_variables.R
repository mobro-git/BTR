

create_graph_list <- function(config) {

  path = paste("./plot_mapping/round",config$round_num, sep="")

  files <- list.files(path, pattern = "\\.csv$")


  ### Everything except for Sankey ###
  files <- data.frame(files = files) %>%
    filter(!str_detect(files, "sankey")) %>%
    filter(!str_detect(files, "figmap")) %>%
    filter(!str_detect(files, "audit_sum")) %>%
    filter(!str_detect(files, "special_cases")) %>%
    pull(files)


  column_names = c("figure_type",	"figure_no", "title_name", "variable")


  data = data.frame()
  for (i in 1:length(files)) {
    dat <- read.csv(paste(path, files[i], sep = "/")) %>%
      select(column_names) %>%
      mutate(figure_no = as.character(figure_no)) %>%
      mutate(file_name = files[i])
    data <- bind_rows(data, dat)
  }

  calculated_vars = unlist(sapply(config$calculated_var, `[[`, "variable"))

  data <- data %>%
    mutate(variable_status = case_when( (variable %in% calculated_vars) ~ "calculated",
                                        TRUE ~ "template"))

  write.csv(data, "./output/round1/list_of_figure_variables.csv")

  data
}


