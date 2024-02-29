
#' read_data_file
#'
#' @param filepath
#'
#' @return
#' @export
read_raw_data_file <- function(filepath) {

  filepath_ext <- fs::path_ext(filepath)

  raw <- if(filepath_ext %in% c("xls", "xlsx")) {
    readxl::read_xlsx(filepath, sheet = "data", na = c("", "NA", "N/A"))
  } else if(filepath_ext == "csv") {
    readr::read_csv(file = filepath, na = c("", "NA", "N/A"), col_types = cols())
  } else {
    stop("Unable to read file type.")
  }

  raw %>%
    mutate(datasrc = fs::path_file(filepath))
}


