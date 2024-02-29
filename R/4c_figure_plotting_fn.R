################################################################################
# pdf or png plot pre-check functions
################################################################################

approved_plot_type <- function(type) {
  if (!(type %in% c("stacked_bar", "ref_stacked_bar","diff_bar","time_series", "cone_uncertainty", "band","scatterplot","corrplot"))) {
    rlang::abort("Unknown plot type in pdf_plots fn. Please select stacked_bar, ref_stacked_bar, diff_bar, time_series, cone_uncertainty, band, corrplot, or scatterplot.")
  }

  TRUE
}


approved_facet_type <- function(dat_filtered) {
  if (unique(dat_filtered$figure_type) != "corrplot") {
  if (! ((length(unique(dat_filtered$type)) == 1) && (unique(dat_filtered$type) %in% c("grid", "wrap", "single")))) {
    rlang::abort("Invalid facet type. Please select grid, wrap, or single.")
    }
  }

  TRUE
}


#' check_dat_before_plotting
#'
#' This function checks whether there is any observation for dat,
#' whether the figure title corresponds to unique figure number
#' whether there are multiple units
#' @param dat
#'
#' @return
#' @export
#'
check_dat_before_plotting <- function(dat) {

  # No observation
  if (is.null(dat)) {
    FALSE
  } else {
    # multiple figure number corresponding to the same figure name
    if (length(unique(dat$figure_no)) != 1) {
      figure = unique(dat$title_name)
      rlang::abort(message = paste("There are multiple figures corresponding to figure", figure,
                                   ".Please recheck the code.", sep = ""))
    } else if (((length(unique(dat$unit))!=1) && (!unique(dat$figure_type) %in% c("scatterplot", "corrplot")))) {
      # multiple units
      rlang::warn(paste('there are multiple units for ', unique(dat$title_name), ". Figure not printed.", sep = ""))
      FALSE
    }
    else {
      TRUE
    }
  }
}


################################################################################
# pdf or png plot functions
################################################################################


#' pdf_plots
#'
#' This function produces pdf files
#' @param overall_path
#' @param df
#' @param presentation_name
#' @param type
#'
#' @return
pdf_plots <- function(overall_path, df, presentation_name, type, sub_palettes, config, sub, saveData = FALSE) {
  pdf(file=paste(overall_path, presentation_name, "_", type, sub, ".pdf",sep=""), width = 14, height = 6)
  print(paste("There are a total of ", length(unique(df$title_name)), " figures", sep = ""))

  if (saveData) {
    data_wb <- createWorkbook()
  }

  if(approved_plot_type(type)) {

    data_processing_fn = get(paste(type, "_figure_specific_data_processing", sep = ""))

    for (figure_num in unique(df$figure_no)) {

      dat = df %>%
          filter(figure_no == figure_num) %>%
          data_processing_fn(config)

      figure = unique(dat$title_name)
      print(figure)

      for (selected in unique(dat[[unique(dat$page_filter)]])) {
        dat_filtered = dat %>%
          filter(!!sym(unique(dat$page_filter)) == selected)

        if (approved_facet_type(dat_filtered) & check_dat_before_plotting(dat_filtered)) {
          plot_fn = get_plot_fn(type, unique(dat_filtered$type))

          if (saveData) {
            addWorksheet(data_wb, figure_num)
            writeData(wb = data_wb, sheet = as.character(figure_num), x = dat_filtered)
          }

          if(type != "corrplot") {
          plot = call_plot_fn(dat_filtered, figure, selected, sub_palettes, type, plot_fn)
          print(plot)
          } else {
            call_plot_fn(dat_filtered, figure, selected, sub_palettes, type, plot_fn)
          }
        }

      # for (selected_region in unique(dat$region)) {
      #   dat_filtered = dat %>%
      #     filter(region == selected_region)

        # if (approved_facet_type(dat_filtered) & check_dat_before_plotting(dat_filtered)) {
        #   plot_fn = get_plot_fn(type, unique(dat_filtered$type))
        #   plot = call_plot_fn(dat_filtered, figure, selected_region, sub_palettes, type, plot_fn)
        # }
      }
    }
  }

  if (saveData) {
    saveWorkbook(data_wb, file = paste(overall_path, presentation_name, "_", type, sub, ".xlsx",sep=""), overwrite = TRUE)
  }


  dev.off()
}



#' png_plots
#'
#' @param overall_path
#' @param df
#' @param presentation_name
#' @param type
#'
#' @return

png_plots <- function(overall_path, df, presentation_name, type, sub_palettes, config, sub, saveData = FALSE) {

  # Make sure plot type is sound; abort if not named correctly
  if(approved_plot_type(type)) {
    data_processing_fn = get(paste(type, "_figure_specific_data_processing", sep = ""))

    for (figure_num in unique(df$figure_no)) {

      dat = df %>%
        filter(figure_no == figure_num) %>%
        data_processing_fn(config)

      figure = unique(dat$title_name)


      for (selected in unique(dat[[unique(dat$page_filter)]])) {
        dat_filtered = dat %>%
          filter(!!sym(unique(dat$page_filter)) == selected)

        if (approved_facet_type(dat_filtered) & check_dat_before_plotting(dat_filtered)) {

          write.xlsx(dat_filtered, paste(overall_path, presentation_name, "_", type, sub, ".xlsx",sep=""))

          plot_fn = get_plot_fn(type, unique(dat_filtered$type))
          png(filename=paste(overall_path, type, "/",
                             str_replace_all(unique(dat$title_name), "\\|","_") , "_",
                             str_replace(selected, " ", "_"), sub,".png", sep = ""),
              width=1200, height=600)
          plot = call_plot_fn(dat_filtered, figure, selected, sub_palettes, type, plot_fn)

          print(plot)
          dev.off()

      # for (selected_region in unique(dat$region)) {
      #   dat_filtered = dat %>%
      #     filter(region == selected_region)

        # if (approved_facet_type(dat_filtered) & check_dat_before_plotting(dat_filtered)) {
        #   plot_fn = get_plot_fn(type, unique(dat_filtered$type))
        #   png(filename=paste(overall_path, type, "/", str_replace(selected_region, " ", "_"), "_",
        #                      str_replace_all(unique(dat$title_name), "\\|","_") , ".png", sep = ""),
        #       width=1200, height=600)
        #   plot = call_plot_fn(dat_filtered, figure, selected_region, sub_palettes, type, plot_fn)
        #
        #   print(plot)
        #   dev.off()
        }
      }
    }
  }
}


################################################################################
# Function to call Plot Functions w/ Different Arguments
################################################################################


#' get_plot_fn
#'
#' @param graph_type
#' @param graph_arrangement
#'
#' @return plot_fn

get_plot_fn <- function(graph_type, graph_arrangement) {
 if (graph_type == "diff_bar") {
    plot_fn = get(paste("stacked_bar", "_", graph_arrangement, "_fn", sep = ""))
 } else {
    plot_fn = get(paste(graph_type, "_", graph_arrangement, "_fn", sep = ""))
  }

  plot_fn
}


call_plot_fn <- function(df, figure, selected, sub_palettes, graph_type, plot_fn) {

  if (graph_type != "corrplot") {
  data_list = list(
    x = unique(df$x), y = unique(df$y),
    color = unique(df$color),
    shape = unique(df$shape), # Only usable in the scatter plot functions for the shape of the data indications (like triangle, etc.)
    facet = unique(df$facet1),
    facet1 = unique(df$facet1), facet2 = unique(df$facet2))

  mapping_list = list(
    xlab = unique(df$x), ylab= unique(df$unit),
    title = paste(figure, ": ", selected, sep = ""),
    model_color_palette = figure, palettes = sub_palettes,
    scales = unique(df$scales),
    position = unique(df$position))

  } else {

    data_list = NULL

    mapping_list = list(
      use = unique(df$use),
      method_cor = unique(df$method_cor),
      method_corrplot = unique(df$method_corrplot),
      diag = unique(df$diag)
    )
  }


  # corrplot
  if (graph_type == "corrplot") {
    plot = plot_fn(df = df, data_list = data_list, mapping_list = mapping_list)
  }

  # stacked bar
  else if (graph_type == "stacked_bar") {
    plot = plot_fn(df = df, data_list = data_list, mapping_list = mapping_list)
  }

  # diff bar
  else if (graph_type == "diff_bar") {
    data_list$y = "diff"
    plot = plot_fn(df = df, data_list = data_list, mapping_list = mapping_list)
    if (unique(df$line_request)) {
      plot = plot +
        geom_line(aes(y = .data[["diff_sum"]], linetype = "Net"))
    }

  }

  # time series
  else if (graph_type == "time_series") {
    plot = plot_fn(df = df, data_list = data_list, mapping_list = mapping_list)
  }

  # cone of uncertainty
  else if (graph_type == "cone_uncertainty"){

    if (unique(df$range) == "model") {
      subtitle = unique(df$scenario)
    } else if (unique(df$range) == "scenario") {
      subtitle = unique(df$model)
    }

    data_list['range'] = unique(df$range)
    mapping_list['title'] = paste(figure, "-", selected, " (", subtitle, ")", sep = "")
    plot = plot_fn(df = df, data_list = data_list, mapping_list = mapping_list)
  }

  # band
  else if (graph_type == "band"){

    data_list['range'] = "range"
    plot = plot_fn(df = df, data_list = data_list, mapping_list = mapping_list)

  }

  # scatterplot
  else if (graph_type == "scatterplot") {
    mapping_list['xlab'] = unique(df$xlab)
    mapping_list['ylab'] = unique(df$ylab)
    data_list['label'] = unique(df$label)
    mapping_list['text'] = unique(df$text)
    mapping_list['text_direction'] = unique(df$text_direction)
    plot = plot_fn(df = df, data_list = data_list, mapping_list = mapping_list)
  }

  # other: abort
  else {
    rlang::abort("unknown graph type in call_plot_fn.")
  }

  plot
}


