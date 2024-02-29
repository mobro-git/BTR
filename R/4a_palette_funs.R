
# colors to color map

find_color <- function(col_name, col_list) {

  temp <- case_when(
    str_detect(col_name, "^#") ~ col_name,
    col_name %in% names(col_list) ~ col_list[col_name],
    TRUE ~ NA_character_)

  names(temp) <- names(col_name)

  temp
}

scale_subpalette <- function(palettes, sub_pal, aesthetics = c("colour","fill"), ...) {

  pal_val <- palettes[[sub_pal]]

  if(is.null(pal_val)) {stop("Unrecognized palette name")}

  scale_fill_manual(values = pal_val,
                    aesthetics = aesthetics,
                    ...)

}

scale_subpalette2 <- function(sub_pal, ...) {

  pal_val <- sub_palettes[[sub_pal]]

  if(is.null(pal_val)) {stop("Unrecognized palette name")}

  list(
    scale_fill_manual(values = pal_val,
                      guide = guide_legend(override.aes = list(alpha = 0.2)),
                      ...),
    scale_color_manual(values = pal_val,
                       guide = guide_legend(override.aes = list(alpha = 1)),
                       ...)
  )

}
