
clean_text_for_md <- function(x) {
  res <- x %>%
    stringr::str_replace_all("_", "-") %>%
    stringr::str_replace_all("\\*", "x")

}

clean_text_for_filename <- function(x) {
  res <- x %>%
    stringr::str_replace_all("/", "_") %>%
    stringr::str_replace_all("\\|", "-")
  res
}

#' Helper to replace empty lists
#'
`%|||%` <- function (x, y)
{
  if (is_empty(x)) {
    y
  }
  else {
    x
  }
}


#' #' Return the scalar of a vector
#' #'
#' #' @export
#' one <- function(x) {
#'   if (length(x) == 1) x else
#'     if (length(x) == 0) dplyr:::default_missing(x) else
#'       stop("Input `x` to `one` function must not have length > 1")
#'
#' }

#' Return the scalar of a vector
#'
#' @examples
#' one(c(1))
#' one(numeric(0))
#' typeof(one(numeric(0)))
#' \dontrun {
#' one(c(1, 2))
#' }
#' @export
#'
one <- function(x) {
  if (length(x) == 1) x else
    # NA value typed the same as x
    if (length(x) == 0) x[NA] else
      stop("Input `x` to `one` function must not have length > 1")
}


#' Return a single value from a vector, for which a different condition holds
#'
#' Intended to help with EAV calculations that potentially have length 0, ruining data frame operations.
#' @param x a vector from which to return results
#' @param p a vector of booleans (or other `[` indices?) length of x
#' @export
`%forwhich%` <- function(x, p) {
  one(x[p])
}


#' Target factory to Make a Git-Stable Version of meta
#'
#' @param deps no-op NSE argument to facilitate listing a late dependent targets (so this is the last target)
#' @param path_meta_out length 1 character path where to store the stable meta file
#'
#' @examples
#' # in _targets.R
#' tar_meta_stable()
#'
tar_meta_stable <- function(
  name = meta_stable,
  deps = NULL,
  path_meta_out = fs::path(tar_config_get("store"), "meta", "meta-stable"),
  cue = targets::tar_cue("always")
) {

  # from targets::tar_target:
  name <- tar_deparse_language(substitute(name))
  tar_assert_chr(name)
  tar_assert_nzchar(name)

  # add deps into the command
  command <- rlang::expr({

    rlang::expr({{ deps }}) # TODO: better way to handle multiple deps?

    targets::tar_meta() %>%
      filter(name != "meta_stable") %>%
      select(-time, -seconds) %>%
      arrange(type, name) %>%
      write_delim(delim = "|", !!path_meta_out)

  })

  targets::tar_target_raw(
    name = name,
    command = command,
    cue = cue
  )
}


#' check_stop()
#'
#'
check_stop <- function(string) {
  stopifnot(exists(string))
}

