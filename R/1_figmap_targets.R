
# creates figmap csv paths based on mapping csv
figmap_csv_path <- function(fig_subject, fig_type) {

  paste0("plot_mapping/", fig_subject, "_", fig_type, ".csv")

}
