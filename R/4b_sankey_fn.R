sankey_fn <- function(presentation_title, presentation_plot_type, config, emf_data_long, plot_list) {

  plot_list <- read_csv(plot_list, col_types = cols())

  standard_sankey_cols <- c("figure_type","figure_no","title_name","regions","models","years",
                            "scenarios","source","source_var","target","target_var","link_var")

  # if(! all(standard_sankey_cols %in% names(plot_list))) {
  #   rlang::abort(paste("Missing at least one standard column in the sankey plot mapping csv. Present:",
  #                      standard_sankey_cols %in% names(plot_list), sep = ""),
  #                class = 'plot_mapping_csv')
  # }

  overall_path = paste("./output/round",config$round_num,"/", presentation_title, "/", sep = "")
  type = presentation_plot_type

  for (i in unique(plot_list$figure_no)) {

    selected_plot_list <- plot_list %>%
      filter(figure_no == i)


    for (column in c("models", "years", "regions", "scenarios")) {
      if(exists(as.character(unique(selected_plot_list[[column]])), config)) {
        assign(paste("selected_", column, sep = ""), config[[unique(selected_plot_list[[column]])]])
      } else {
        assign(paste("selected_", column, sep = ""), unique(selected_plot_list[[column]]))
      }
    }


    # filter_data
    for (selected_model in selected_models) {
      for (selected_year in selected_years) {
        for (selected_scenario in selected_scenarios) {
          for (selected_region in selected_regions) {
            df <- emf_data_long %>%
              filter(model == selected_model) %>%
              filter(year == selected_year) %>%
              filter(scenario == selected_scenario) %>%
              filter(region == selected_region)

              df = df %>%
                filter(variable %in% selected_plot_list$link_var) %>%
                left_join(selected_plot_list, by = c("variable" = "link_var"))




              if (nrow(df) != 0) {
                links <- data.frame(
                  source = df$source,
                  target = df$target,
                  value = df$value
                )

                # From these flows we need to create a node data frame: it lists every entities involved in the flow
                nodes <- data.frame(
                  name=c(as.character(links$source),
                         as.character(links$target)) %>% unique()
                )

                # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
                links$IDsource <- match(links$source, nodes$name)-1
                links$IDtarget <- match(links$target, nodes$name)-1

                # Color mapping from color_map in 4a_theme.R

                # links$color <- sub(' .*', '',nodes[links$source + 1, 'name'])

                # # Add a 'group' column to the nodes data frame:
                # nodes$group <- as.factor(c("a","a","a","a","a","b","b","b"))
                #
                # # Give a color for each group:
                # my_color <- 'd3.scaleOrdinal() .domain(["a", "b"]) .range(["#69b3a2", "steelblue"])'
                my_color <- col2hex(color_map)
                # my_color <- 'd3.scaleOrdinal() .domain(["a", "b"]) .range(["#69b3a2", "steelblue"])'
                #
                # setDomain = {
                #   color.domain(my_color);
                #   return color.domain();
                # }

                print(head(my_color))
                print(head(nodes))
                print(head(links))

                # Make the Network
                p <- sankeyNetwork(Links = links, Nodes = nodes,
                                   Source = "IDsource", Target = "IDtarget",
                                   Value = "value", NodeID = "name",
                                   sinksRight=TRUE,
                                   fontFamily = "Georgia",
                                   fontSize = 30,
                                   height = 1000,
                                   width = 1500,
                                   LinkGroup = 'source',
                                   colourScale=my_color,
                                   NodeGroup="name")

                p <- htmlwidgets::prependContent(p,
                                                 htmltools::tags$h1(paste(str_replace_all(unique(df$title_name), "\\|","_") , ": ",
                                                                          selected_model, "-",
                                                                          selected_year, "-",
                                                                          selected_scenario, "-",
                                                                          selected_region)))

p <- htmlwidgets::onRender(p, '
      function(el) {
        var nodeWidth = this.sankey.nodeWidth();
        var links = this.sankey.links();

        links.forEach((d, i) => {
          var startX = d.source.x + nodeWidth;
          var endX = d.target.x;

          var startY = d.source.y + d.sy + d.dy / 2;
          var endY = d.target.y + d.ty + d.dy / 2;

          d3.select(el).select("svg g")
            .append("text")
            .attr("text-anchor", "middle")
            .attr("alignment-baseline", "middle")
            .attr("x", startX + ((endX - startX) / 2))
            .attr("y", startY + ((endY - startY) / 2))
            .text(d.value);
        })
      }
    ')

                path = paste(overall_path, type, "/",
                             str_replace_all(unique(df$title_name), "\\|","_"), "_",
                             selected_model, "_",
                             selected_year, "_",
                             selected_scenario, "_",
                             selected_region, sep = "")

                saveNetwork(p, paste(path, ".html", sep = ""))
                # saveWidget(p, paste(path, ".html", sep = ""))

                # library(webshot)
                # webshot(paste(path, ".html", sep = ""),
                #         paste(path, ".png", sep = ""), vwidth = 1000, vheight = 900)
                #

              }
          }
        }
      }
    }
  }
}


# overview_sankey = sankey_fn("overview", "sankey", config, emf_data_long, figmap_csv_overview_sankey)
