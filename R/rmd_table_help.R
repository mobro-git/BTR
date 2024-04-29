# gt utility code, some style adapted from jthomasmock/gtExtras

#' Apply a blue theme used in NatCom/BR to a gt table
#'
#' @param gt_object An existing gt table object of class `gt_tbl`
#' @param ... Optional additional arguments to `gt::table_options()`
#' @return An object of class `gt_tbl`
#' @export
#'
gt_theme_nc_blue <- function(gt_object) {
  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))
  
  gt_obj_out <- gt_object %>%
    sub_missing(columns = everything(), missing_text = "---") %>%
    opt_table_font(
      font = c(
        google_font("Lato"),
        default_fonts()
      )) %>%
    
    tab_options(
      #table.width = px(600),
      table.font.size = 12,
      data_row.padding = px(5),
      heading.subtitle.font.weight = "normal",
      heading.title.font.weight = "bold",
      column_labels.font.weight = "bold",
      heading.title.font.size = 18,
      table.font.color.light = "#FFFFFF",
      heading.background.color = "#FFFFFF",
      heading.border.bottom.width = 0,
      table_body.hlines.color = "#007BA7",
      table_body.hlines.width = 1.5,
      table_body.vlines.color = "#007BA7",
      table_body.vlines.width = 1.5,
      table_body.border.top.color = "#FFFFFF",
      table_body.border.bottom.color = "#007BA7",
      table.border.bottom.width = 1.5,
      table.border.top.width = 1.5,
      table.border.right.color = "#007BA7",
      table.border.left.color = "#007BA7",
      row_group.font.weight = "bolder",
      row_group.background.color = "#e6edfc",
      row_group.border.top.color = "#007BA7",
      row_group.border.bottom.color = "#007BA7",
      row_group.border.left.color = "#007BA7",
      row_group.border.right.color = "#007BA7",
      row_group.border.right.width = 2,
      row_group.border.bottom.width = 2,
      row_group.border.top.width = 2,
      column_labels.background.color = "#007BA7",
      column_labels.text_transform = "capitalize",
      column_labels.border.lr.color = "#FFFFFF",
      column_labels.border.lr.width = 3,
      column_labels.border.top.width = 0,
      column_labels.border.bottom.width = 1,
      column_labels.border.bottom.color = "#FFFFFF",
      column_labels.vlines.width = 1.5,
      column_labels.vlines.color = "#007BA7",
      stub.font.weight = "normal",
      stub.border.color = "#007BA7",
      summary_row.background.color = "#e6edfc",
      summary_row.border.color = "#007BA7",
      grand_summary_row.background.color = "#e6edfc",
      grand_summary_row.border.color = "#007BA7",
      grand_summary_row.border.style = "normal",
      grand_summary_row.border.width = 3,
      heading.align = "left")
  
  gt_obj_out
}

# Wrapper for tables
table_basic <- function(df, groupname=NULL, rowname=NULL, title=NULL, subtitle=NULL) {
  gt_obj_out <-
    df %>%
    gt(rowname_col = rowname, groupname_col = groupname) %>%
    gt_theme_nc_blue()
  
  if(!is.null(title)) {
    gt_obj_out <- gt::tab_header(gt_obj_out, title = title)}
  if(!is.null(subtitle)) {
    gt_obj_out <- gt::tab_header(gt_obj_out, subtitle = subtitle)}
  
  gt_obj_out
}


table_basic_options <- function(gt_obj) {
  
  gt_obj %>%
    gt_theme_nc_blue()
}

gross_net <- function(gt_obj_in, groups=NULL, columns) {
  if (is.null(groups)) {
    g_str <- "Total Gross Emissions"
    n_str <- "Total Net Emissions"
  } else {
    g_str <- "Gross Emissions"
    n_str <- "Net Emissions"
  }
  
  fns = list()
  fns[[g_str]] = ~sum(.*(. >= 0.0), na.rm = TRUE)
  fns[[n_str]] = ~sum(., na.rm = TRUE)
  
  gt_obj_out <-
    gt_obj_in %>%
    summary_rows(groups = groups,
                 formatter = fmt_number, decimals = 1,
                 columns = columns,
                 fns = fns)
  
  return(gt_obj_out)
}

gross_only <- function(gt_obj_in, groups=NULL, columns) {
  if (is.null(groups)) {
    g_str <- "Total Gross Emissions"
  } else {
    g_str <- "Gross Emissions"
  }
  
  fns = list()
  fns[[g_str]] = ~sum(.*(. >= 0.0), na.rm = TRUE)
  
  gt_obj_out <-
    gt_obj_in %>%
    summary_rows(groups = groups,
                 formatter = fmt_number, decimals = 1,
                 columns = columns,
                 fns = fns)
  
  return(gt_obj_out)
}

net_only <- function(gt_obj_in, groups=NULL, columns) {
  if (is.null(groups)) {
    n_str <- "Total Net Emissions"
  } else {
    n_str <- "Net Emissions"
  }
  
  fns = list()
  fns[[n_str]] = ~sum(., na.rm = TRUE)
  
  gt_obj_out <-
    gt_obj_in %>%
    summary_rows(groups = groups,
                 formatter = fmt_number, decimals = 1,
                 columns = columns,
                 fns = fns)
  
  return(gt_obj_out)
}

net_by_group <- function(gt_obj_in, groups, columns, row_name, sector_name, scenario_name, sinks_name, grouping_cat = "sector") {
  temp_name <- "Temp"
  saved <- gt_obj_in
  sdf <- gt_obj_in["_stub_df"]
  sdfl <- gt_obj_in[["_stub_df"]]
  rgs <- gt_obj_in[["_row_groups"]]
  tib <- sdf[[1]]
  idxs <- list()
  
  for (g in groups) {idxs <- c(idxs, pull(tib[tib["group_id"] == g, "rownum_i"]))}
  
  gt_obj_in <- regroup_rows(gt_obj_in, idxs, temp_name = temp_name)
  gt_obj_in <-
    gt_obj_in %>% net_only(groups = temp_name, columns = columns)
  
  summ <- extract_summary(gt_obj_in)
  gt_obj_in <- saved
  rowdata <- dplyr::select(summ$summary_df_data_list[[temp_name]], -c(group_id, rowname))
  rowdata[grouping_cat] <- row_name
  rowdata["scenario"] <- scenario_name
  rowdata["emission"] <- sinks_name
  
  sdfl <- sdfl %>% add_row(rownum_i = nrow(sdfl) + 1, group_id = sinks_name,
                           rowname = row_name, group_label = sinks_name, built = NA)
  
  gt_obj_in[["_stub_df"]] <- sdfl
  gt_obj_in[["_data"]] <- gt_obj_in[["_data"]] %>% add_row(rowdata)
  
  return(gt_obj_in)
}

regroup_rows <- function(gt_obj_in, idxs, temp_name) {
  sdf <- gt_obj_in["_stub_df"]
  tib <- sdf[[1]]
  tib <-
    tib %>%
    mutate(group_id = case_when(rownum_i %in% idxs ~ temp_name,
                                TRUE ~ group_id)) %>%
    mutate(group_label = case_when(rownum_i %in% idxs ~ temp_name,
                                   TRUE ~ group_label))
  
  gt_obj_in["_stub_df"] <- list(tib)
  gt_obj_in[["_row_groups"]] <- unique(gt_obj_in["_stub_df"][[1]]$group_id)
  return(gt_obj_in)
}

full_sector_name <- function(sector) {
  unit <- sector
  unit <- gsub("LULUCF", "Land Use, Land Use Change, and Forestry", unit)
  unit <- gsub("IPPU", "Industrial Processes and Product Use", unit)
  return(unit)
}

format_fake_summary_rows <- function(gt_obj_in, colnames = c("Gross", "Net")) {
  gt_obj_out <-
    gt_obj_in %>%
    tab_style(locations = list(
      cells_stub(rows = contains(colnames)),
      cells_body(columns = everything(), rows = contains(colnames))
    ),
    style = list(
      cell_fill(color = "#e6edfc"),
      cell_borders(weight = px(2), color = "#007BA7", sides = c("top", "bottom"))
    ))
  
  return(gt_obj_out)
}

table_output_wrapper <- function(gt_obj_in, save_path = "tempdir") {
  if (save_path == "tempdir") {
    save_path = file.path(tempdir(), "table_output_wrapper_temp.png")
  }
  
  if (!isTRUE(getOption('knitr.in.progress'))) {
    gt::gtsave(gt_obj_in, save_path)
    return(gt_obj_in)
  }
  
  if (knitr::is_html_output()) {
    gt::gtsave(gt_obj_in, save_path)
    return(gt_obj_in)
  }
  
  cat(gt::as_rtf(gt_obj_in))
  
  if (!isTRUE(getOption('knitr.in.progress'))) {
    print(knitr::include_graphics(save_path))
  }
  
  img_out <- gtsave(gt_obj_in, save_path)
  return(img_out)
}
