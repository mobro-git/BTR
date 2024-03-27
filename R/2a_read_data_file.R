
#' read_data_file
#'
#' @param filepath
#'
#' @return
#' @export

read_file_ext <- function(filepath) {
  filepath_ext <- fs::path_ext(filepath)

  raw <- if(filepath_ext %in% c("xls", "xlsx")) {
    if (length(readxl::excel_sheets(filepath) == 1)) {
      readxl::read_xlsx(filepath)
    }
    else {
      readxl::read_xlsx(filepath, sheet = "data", na = c("", "NA", "N/A"))
    }
  } else if (filepath_ext == "csv") {
    readr::read_csv(file = filepath, na = c("", "NA", "N/A"), col_types = cols())
  } else {
    stop("Unable to read file type.")
  }
}


read_raw_data_file <- function(filepath) {

  raw <-  read_file_ext(filepath)
  
  column_renames <- c(
    "model" = "Model",
    "scenario" = "Scenario",
    "region" = "Region",
    "variable" = "Variable",
    "unit" = "Unit",
    "unit" = "Units",
    "year" = "Year",
    "value" = "Value",
    "datasrc" = "file"
  )
  
  lowercase_names <- column_renames[column_renames %in% names(raw)]

  raw_renamed = raw %>%
    rename(!!! lowercase_names)
  
  raw_data_cols <- c("model",
                     "scenario",
                     "region",
                     "variable",
                     "unit")

  if(! all(raw_data_cols %in% names(raw_renamed))){
    stop(paste0(setdiff(raw_data_cols, names(raw_renamed)),' column missing from ',fs::path_file(filepath),". "))
  }
  
  raw %>%
    mutate(datasrc = fs::path_file(filepath))
}

