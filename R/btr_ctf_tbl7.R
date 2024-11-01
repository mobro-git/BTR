
make_btr_ctf_table = function(lulucf_data_extra_xlsx,
                                    lulucf_btr_crosswalk,
                                    projections_all_sm,
                                    settings,
                                    config) {
  
  sectors <- projections_all_sm %>%
    filter(grouping %in% c('wm_sens', 'ghgi'),
           year %in% config$table_ctf) %>% 
    group_by(proj_name,usproj_sector, year) %>%
    summarise(value = sum(sum),.groups='drop') %>%
    rename(category = usproj_sector)
  
  sectors_ctf <- sectors %>%
    group_by(category, year) %>%
    summarise(max = max(value),
              min = min(value)) %>%
    mutate(category = case_when(category == "IPPU" ~ "Industrial Processes and Product Use",
                                category == "LULUCF Sink" ~ "LULUCF",
                                TRUE~category))
  
  gases <- projections_all_sm %>%
    filter(grouping %in% c('wm_sens', 'ghgi'),
           year %in% config$table_ctf,
           !gas == 'LULUCF Sink') %>% 
    group_by(proj_name,gas, year) %>%
    summarise(value = sum(sum),.groups='drop')
  
  gases_ctf <- gases %>%
    mutate(gas = case_when(gas == "CO2" ~ "CO2 emissions excluding net CO2 from LULUCF",
                           gas == "CH4" ~ "CH4 emissions excluding CH4 from LULUCF",
                           gas == "N2O" ~ "N2O emissions excluding N2O from LULUCF",
                           TRUE~gas)) %>%
    rename(category = gas) %>%
    group_by(category, year) %>%
    summarise(max = max(value),
              min = min(value))
  
  gases_excl_lulucf <- gases %>%
    filter(gas %in% c("CO2",
                      "CH4",
                      "N2O")) %>%
    group_by(gas, year) %>%
    summarise(max = max(value),
              min = min(value))%>%
    rename(category = gas)
  
  lulucf_data = read_xlsx(lulucf_data_extra_xlsx, sheet = "LULUCF TABLE FOR PIPELINE") %>%
    pivot_longer(cols = 6:ncol(.),
                 names_to = "year",
                 values_to = "value") %>%
    drop_na(value) %>%
    filter(year %in% config$table_ctf) %>%
    group_by(variable, year) %>%
    summarise(max = max(value),
              min = min (value)) %>%
    rename(category = variable) %>%
    mutate(category = case_when(category == "Emissions|LULUCF|Carbon Stock Change" ~ "CO2",
                                category == "Emissions|LULUCF|CH4" ~ "CH4",
                                category == "Emissions|LULUCF|N2O" ~ "N2O"
                                ),
           year = as.numeric(year))
  
  gases_incl_lulucf <- gases_excl_lulucf %>%
    rbind(lulucf_data) %>%
    group_by(category, year) %>%
    summarise(min = sum(min),
              max = sum(max)) %>%
    mutate(category = case_when(category == "CO2" ~ "CO2 emissions including net CO2 from LULUCF",
                           category == "CH4" ~ "CH4 emissions including CH4 from LULUCF",
                           category == "N2O" ~ "N2O emissions including N2O from LULUCF",
                           TRUE~category))
    
  
  total_w_lulucf <- sectors %>%
    group_by(proj_name, year) %>%
    summarise(value = sum(value)) %>%
    mutate(category = "Total with LULUCF") %>%
    select(names(sectors)) %>%
    group_by(category, year) %>%
    summarise(max = max(value),
              min = min(value))
  
  total_wo_lulucf <- sectors %>%
    filter(!category == 'LULUCF Sink') %>%
    group_by(proj_name, year) %>%
    summarise(value = sum(value)) %>%
    mutate(category = "Total without LULUCF") %>%
    select(names(sectors)) %>%
    group_by(category, year) %>%
    summarise(max = max(value),
              min = min(value))
  
  ctf_tbl_df <- sectors_ctf %>%
    rbind(gases_ctf,
          gases_incl_lulucf,
          total_w_lulucf,
          total_wo_lulucf)

#####
  
  proj_years <- c('2025','2030','2035','2040')
  hist_years <- c(settings$base_year)
  proj_col_order <- c('2025_low',
                      '2025_high',
                      '2030_low',
                      '2030_high',
                      '2035_low',
                      '2035_high',
                      '2040_low',
                      '2040_high')
  
  df_low <- ctf_tbl_df %>% 
    select(category,year,min) %>%
    pivot_wider(names_from = 'year',
                values_from = 'min')
  
  
  df_high <- ctf_tbl_df %>% 
    select(category,year,max) %>%
    pivot_wider(names_from = 'year',
                values_from = 'max')
  
  
  
  
  df_merged <- merge(df_low[, c('category',proj_years)],df_high[, c('category',proj_years)], by = 'category', suffix = c('','_high'))
  
  
  for(year in proj_years) {
    colnames(df_merged)[colnames(df_merged)==year] <- paste0(year,'_low')
    colnames(df_merged)[colnames(df_merged) == paste0(year,'_high')] <- paste0(year,'_high')
  }
  
  df_merge_final <- df_low %>% select(category, `2022`) %>% left_join(df_merged %>%
    select(category,all_of(proj_col_order)))
  
  category_order <- c(
    "Energy",
    "Transportation",
    "Industrial Processes and Product Use",
    "Agriculture",
    "LULUCF",
    "Waste",
    "CO2 emissions including net CO2 from LULUCF",
    "CO2 emissions excluding net CO2 from LULUCF",
    "CH4 emissions including CH4 from LULUCF",
    "CH4 emissions excluding CH4 from LULUCF",
    "N2O emissions including N2O from LULUCF",
    "N2O emissions excluding N2O from LULUCF",
    "HFCs",
    "PFCs",
    "SF6",
    "NF3"
  )
  
  ctf_table_df_clean <- df_merge_final %>% mutate_if(is.numeric, round)
  
  ctf_table_df_clean[] <- lapply(ctf_table_df_clean, function(x) gsub("-","(", x))
  ctf_table_df_clean[] <- lapply(ctf_table_df_clean, function(x) ifelse(grepl("\\(", x), paste0(x, ")"), x))
  
  ctf_table_df_clean <- ctf_table_df_clean[order(match(ctf_table_df_clean$category, category_order)), ]
  

  
  merged = ctf_table_df_clean %>%
    mutate(
      `2025` = case_when(
        `2025_low` == `2025_high` ~ `2025_high`,
        `2025_low` != `2025_high` ~ paste0(`2025_low`," - ",`2025_high`)),
      `2030` = case_when(
        `2030_low` == `2030_high` ~ `2030_high`,
        `2030_low` != `2030_high` ~ paste0(`2030_low`," - ",`2030_high`)),
      `2035` = case_when(
        `2035_low` == `2035_high` ~ `2035_high`,
        `2035_low` != `2035_high` ~ paste0(`2035_low`," - ",`2035_high`)),
      `2040` = case_when(
        `2040_low` == `2040_high` ~ `2040_high`,
        `2040_low` != `2040_high` ~ paste0(`2040_low`," - ",`2040_high`))
    ) %>%
    select(-contains("_")) %>%
    rename(Category = category)
  
  return(merged)
  
}
