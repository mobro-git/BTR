
net_zero_bar <- function(scen, fig_title, presentation_title, subpalettes, data_long, reference, config,
                         factor = FALSE,
                         save = TRUE,
                         print = TRUE,
                         width = NA,
                         height = NA) {

  modeled <- data_long %>%
    filter(year == 2050 & scenario %in% scen) %>%
    mutate(type = "mod")

  df <- rbind(reference,modeled)

  if(factor == FALSE) {
    modeled_plot <- ggplot() +
      geom_col(data = df %>% filter(type == "ref"),
               aes(x = model, y = value, fill = variable),
               position = "stack", color = "black", size = 1.5) +
      geom_col(data = df %>% filter(type == "mod"),
               aes(x = model, y = value, fill = variable),
               position = "stack") +
      theme_emf() +
      theme(axis.text.x = element_text(angle = 90, hjust=1)) +
      labs(title = fig_title,
           y="Mt CO2", x = "Model") +
      geom_hline(yintercept=0, color = "black", size=.75) +
      scale_subpalette(subpalettes, "CO2 Emissions")
  }

  if(factor == TRUE) {
    df <- df %>%
      mutate(scenario = factor(scenario, levels = scen))

    modeled_plot <- ggplot() +
      geom_col(data = df %>% filter(type == "ref"),
               aes(x = model, y = value, fill = variable),
               position = "stack", color = "black", size = 1.5) +
      geom_col(data = df %>% filter(type == "mod"),
               aes(x = model, y = value, fill = variable),
               position = "stack") +
      theme_emf() +
      theme(axis.text.x = element_text(angle = 90, hjust=1)) +
      labs(title = fig_title,
           y="Mt CO2", x = "Model") +
      geom_hline(yintercept=0, color = "black", size=.75) +
      facet_grid(~scenario) +
      scale_subpalette(subpalettes, "CO2 Emissions")
  }

  if(save == TRUE) {
  ggsave(
    filename = paste(fig_title,".png",sep=""),
    plot = modeled_plot,
    width = width,
    height = height,
    device = "png",
    path = paste("output/round",config$round_num,"/",presentation_title,"/net_zero_bar",sep=""))
  }

  if(print == TRUE) {modeled_plot}

}

