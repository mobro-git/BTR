
#' read_data_file
#'
#' @param filepath
#'
#' @return
#' @export
read_raw_data_file <- function(filepath) {

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
  
  raw_data_cols <- c("Model",
                     "Scenario",
                     "Region",
                     "Variable",
                     "Unit")

  if(! all(raw_data_cols %in% names(raw))){
    stop(paste0(setdiff(raw_data_cols, names(raw)),' column missing from ',fs::path_file(filepath)))
  }
  
  raw %>%
    mutate(datasrc = fs::path_file(filepath))
}

