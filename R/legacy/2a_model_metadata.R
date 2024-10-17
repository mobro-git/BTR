# model_metadata.R

#' Read csv model metadata
#'
#' @param filepath to metadata file
#'
#' read_csv_model_metadata("data-raw/metadata/model_meta.csv")
read_csv_model_metadata <- function(filepath) {
  readr::read_csv(
    filepath,
    col_types = readr::cols(
      model = col_character(),
      model_type = col_character()
    ))
}


################################ unused functions as of Feb 9 2022 ################################
#’ model_type
#' What model_type are the models?
#'
#' @param model character vector of models (names, or possibly versions?)
#'
#' TODO: what about multiple-type models?
#'
#' @return vector of model types the same length as model
#' @import vctrs
model_type <- function(model, model_metadata) {
  validate_model_metadata(model_metadata)

  #depends on uniqueness of model_type in model_metadata:
  left_join(
    as_tibble_col(model, column_name = "model"),
    model_metadata, by = "model"
  ) %>%
    pull(model_type)
}


validate_model_metadata <- function(model_metadata) {

  if(! all(c("model", "model_type") %in% names(model_metadata))) stop("model_metadata must have columns `model` and `model_type`")
  if(vctrs::vec_size(model_metadata) < vctrs::vec_size(distinct(model_metadata, model))) {stop("model_metadata must provide a unique model type for each model.")}

  invisible(model_metadata)
}

distinct_models_in_data <- function(data) {

  model_var_candidates <- c("Model", "model")
  model_var <- model_var_candidates[model_var_candidates %in% names(data)]
  if(length(model_var) != 1) stop("Data must have 1 column representing the model.")

  unique(data[[model_var]])
}


