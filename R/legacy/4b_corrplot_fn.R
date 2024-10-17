corrplot_lower_fn <- function(df, data_list, mapping_list) {

  dat_wide = df %>%
    ungroup() %>%
    select(model,scenario,year,region,variable_rename,value) %>%
    pivot_wider(names_from = "variable_rename", values_from = "value") %>%
    select(-model,-scenario,-year,-region)

  correlations <- cor(dat_wide,
                      use = mapping_list$use,
                      method = mapping_list$method_cor)

  plot <- corrplot(correlations,
                   method = mapping_list$method_corrplot,
                   type = 'lower',
                   tl.cex = 0.6,
                   tl.srt=70,
                   diag = mapping_list$diag)

  plot

}

