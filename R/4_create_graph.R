
create_graph <- function(presentation_title, presentation_plot_type, config, emf_data_long, figmap,
                         pdfGraphs = TRUE, pngGraphs = FALSE, sub = "", saveData = FALSE) {
  #  create folders
  overall_path = paste("./output/", presentation_title, "/", sep = "")

  if (pngGraphs) {
    subfolders = c("", presentation_plot_type)
    create_folders(sapply(subfolders, function(x) {paste(overall_path, x, sep = "")}))
  } else {
    create_folders(overall_path)
  }

  #  graphs

  # select the key variables, flip values, and merge with specific figure requests
  df <- preliminary_data_processing_for_plotting(emf_data_long, figmap)

  # assign color palettes
  if (!presentation_plot_type %in% c("corrplot","sankey")) {
    subpalettes = create_subpalettes(figmap, config)
  } else {
    subpalettes = NULL
  }

  if (pdfGraphs) {
    # full processing based on figure requests + create pdf of plots
    pdf_plots(overall_path, df, presentation_title, presentation_plot_type, subpalettes, config, sub, saveData)
  }

  if (pngGraphs) {
    # full processing based on figure requests + create png of plots
    png_plots(overall_path, df, presentation_title, presentation_plot_type, subpalettes, config, sub, saveData)
  }

}

