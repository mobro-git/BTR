# # ggsankey_fn <- function(presentation_title, presentation_plot_type, config, emf_data_long, plot_list) {
# #
# #   plot_list <- read_csv(plot_list)
# #
# #   standard_sankey_cols <- c("figure_type","figure_no","title_name","regions","models","years",
# #                             "scenarios","source","source_var","target","target_var","link_var")
# #
# #   # if(! all(standard_sankey_cols %in% names(plot_list))) {
# #   #   rlang::abort(paste("Missing at least one standard column in the sankey plot mapping csv. Present:",
# #   #                      standard_sankey_cols %in% names(plot_list), sep = ""),
# #   #                class = 'plot_mapping_csv')
# #   # }
# #
# #   overall_path = paste("./output/round",config$round_num,"/", presentation_title, "/", sep = "")
# #   type = presentation_plot_type
# #
# #   for (i in unique(plot_list$figure_no)) {
# #
# #     selected_plot_list <- plot_list %>%
# #       filter(figure_no == i)
# #
# #
# #     for (column in c("models", "years", "regions", "scenarios")) {
# #       if(exists(as.character(unique(selected_plot_list[[column]])), config)) {
# #         assign(paste("selected_", column, sep = ""), config[[unique(selected_plot_list[[column]])]])
# #       } else {
# #         assign(paste("selected_", column, sep = ""), unique(selected_plot_list[[column]]))
# #       }
# #     }
# #
# #
# #     # filter_data
# #     for (selected_model in selected_models) {
# #       for (selected_year in selected_years) {
# #         for (selected_scenario in selected_scenarios) {
# #           for (selected_region in selected_regions) {
# #             df <- emf_data_long %>%
# #               filter(model == selected_model) %>%
# #               filter(year == selected_year) %>%
# #               filter(scenario == selected_scenario) %>%
# #               filter(region == selected_region)
# #
# #               df = df %>%
# #                 filter(variable %in% selected_plot_list$link_var) %>%
# #                 left_join(selected_plot_list, by = c("variable" = "link_var"))
# #
# #
# #               if (nrow(df) != 0) {
# #                 links <- data.frame(
# #                   source = df$source,
# #                   target = df$target,
# #                   value = df$value
# #                 )
# #
# #                 # From these flows we need to create a node data frame: it lists every entities involved in the flow
# #                 nodes <- data.frame(
# #                   name=c(as.character(links$source),
# #                          as.character(links$target)) %>% unique()
# #                 )
# #
# #                 # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
# #                 links$IDsource <- match(links$source, nodes$name)-1
# #                 links$IDtarget <- match(links$target, nodes$name)-1
# #
# #                 #links$color <- sub(' .*', '',nodes[links$source + 1, 'name'])
# #
# #                 # Make the Network
# #                 p <- sankeyNetwork(Links = links, Nodes = nodes,
# #                                    Source = "IDsource", Target = "IDtarget",
# #                                    Value = "value", NodeID = "name",
# #                                    sinksRight=TRUE,
# #                                    fontFamily = "Georgia",
# #                                    fontSize = 30,
# #                                    height = 1000,
# #                                    width = 1500,
# #                                    LinkGroup = 'source')
# #
# #                 path = paste(overall_path, type, "/",
# #                              str_replace_all(unique(df$title_name), "\\|","_"), "_",
# #                              selected_model, "_",
# #                              selected_year, "_",
# #                              selected_scenario, "_",
# #                              selected_region, sep = "")
# #
# #                 saveNetwork(p, paste(path, ".html", sep = ""))
# #                 # saveWidget(p, paste(path, ".html", sep = ""))
# #
# #                 # library(webshot)
# #                 # webshot(paste(path, ".html", sep = ""),
# #                 #         paste(path, ".png", sep = ""), vwidth = 1000, vheight = 900)
# #                 #
# #
# #               }
# #           }
# #         }
# #       }
# #     }
# #   }
# # }
#
# # install.packages("remotes")
# # remotes::install_github("davidsjoberg/ggsankey")
# library(ggsankey)
# # install.packages("ggplot2")
# library(ggplot2)
# # install.packages("dplyr")
# library(dplyr) # Also needed
#
# # install.packages("remotes")
# # remotes::install_github("davidsjoberg/ggsankey")
# library(ggsankey)
#
# df <- mtcars %>%
#   make_long(cyl, vs, am, gear, carb)
# print(df)
# ggplot(df, aes(x = x,
#                next_x = next_x,
#                node = node,
#                next_node = next_node,
#                fill = factor(node),
#                label = node)) +
#   geom_sankey(flow.alpha = 0.5, node.color = 1) +
#   geom_sankey_label(size = 3.5, color = 1, fill = "white") +
#   scale_fill_viridis_d(option = "A", alpha = 0.95) +
#   theme_sankey(base_size = 16)
#
#
#
#
#
#
#
