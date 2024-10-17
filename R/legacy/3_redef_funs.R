# redef_funs.R

# scenario information -------------

# variable information ---------
# get the var name for variables with matching info in the variable_info_lookup table
def_var_from_variable <- function(variable, variable_info_lookup) {

  tibble(
    variable = variable
  ) %>%
    left_join(
      variable_info_lookup, by = "variable"
    ) %>%
    pull(var)
}

def_from_variable <- function(variable, out_var, variable_info_lookup) {

  tibble(
    variable = variable
  ) %>%
    left_join(
      variable_info_lookup, by = "variable"
    ) %>%
    pull(!! out_var)
}

def_from_variable_factory <- function(variable_info_lookup) {

  function(variable, out_var) {
    tibble(
      variable = variable
    ) %>%
      left_join(
        variable_info_lookup, by = "variable"
      ) %>%
      pull(!! out_var)
  }
}

def_x_from_variable_factory <- function(out_var, variable_info_lookup) {

  out_var <- ensym(out_var)

  function(variable) {
    tibble(
      variable = variable
    ) %>%
      left_join(
        variable_info_lookup, by = "variable"
      ) %>%
      pull(!! out_var)
  }

}
