process_special_cases <- function(df, path) {
  special_cases <- read_csv(path, col_types = cols()) %>%
    mutate(higher_level = lower_level) %>%
    mutate_at(.vars = "higher_level",
              .funs = sub,
              pattern = "\\|[^\\|]*$",
              replacement = "")


  df = df %>% mutate(higher_level =
                       str_replace_all(dirname(str_replace_all(variable, "\\|", "/")),
                                       "/", "\\|"))

  df <- full_join(df, special_cases, by = c("higher_level", "variable" = "lower_level")) %>%
    mutate(group = replace(group, is.na(group), 1)) %>%
    filter(higher_level != ".")

  return(df)
}

