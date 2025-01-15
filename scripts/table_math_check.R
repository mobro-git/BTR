## projections_all

gas = projections_all %>%
  filter(proj_name == "gcam_wm_lowcostre_highfuelcost_gtm")%>%
  group_by(year,gas) %>%
  summarise(value = sum(value)) %>%
  filter(year %in% config$fives_proj_sm) %>%
  pivot_wider(names_from = "year", values_from = "value")

gas_total = projections_all %>%
  filter(proj_name == "gcam_wm_lowcostre_highfuelcost_gtm")%>%
  group_by(year,gas) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  filter(year %in% config$fives_proj_sm) %>%
  group_by(year) %>%
  summarise(value=sum(value)) %>%
  mutate(gas = "total") %>%
  pivot_wider(names_from = "year", values_from = "value") %>%
  select(gas,`2025`,`2030`,`2035`,`2040`)

gas_all = rbind(gas,gas_total)

sector = projections_all %>%
  filter(proj_name == "gcam_wm_lowcostre_highfuelcost_gtm")%>%
  group_by(year,usproj_sector) %>%
  summarise(value = sum(value)) %>%
  filter(year %in% config$fives_proj_sm) %>%
  pivot_wider(names_from = "year", values_from = "value")

sector_total = projections_all %>%
  filter(proj_name == "gcam_wm_lowcostre_highfuelcost_gtm")%>%
  group_by(year,usproj_sector) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  filter(year %in% config$fives_proj_sm) %>%
  group_by(year) %>%
  summarise(value=sum(value)) %>%
  mutate(usproj_sector = "total") %>%
  pivot_wider(names_from = "year", values_from = "value") %>%
  select(usproj_sector,`2025`,`2030`,`2035`,`2040`)

sector_all = rbind(sector,sector_total)

explore = projections_all %>%
  filter(proj_name == "gcam_wm_lowcostre_highfuelcost_gtm")

unique_gas_sources_correct_table = unique(
  (explore %>% mutate(concat = paste(usproj_sector,gas,usproj_category, sep="_")))$concat) 

## projections_ghgi

gas = projections_ghgi %>%
  filter(proj_name == "gcam_wm_lowcostre_highfuelcost_gtm")%>%
  group_by(year,gas) %>%
  summarise(value = sum(value)) %>%
  filter(year %in% config$fives_proj_sm) %>%
  pivot_wider(names_from = "year", values_from = "value")

gas_total = projections_ghgi %>%
  filter(proj_name == "gcam_wm_lowcostre_highfuelcost_gtm")%>%
  group_by(year,gas) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  filter(year %in% config$fives_proj_sm) %>%
  group_by(year) %>%
  summarise(value=sum(value)) %>%
  mutate(gas = "total") %>%
  pivot_wider(names_from = "year", values_from = "value") %>%
  select(gas,`2025`,`2030`,`2035`,`2040`)

gas_all = rbind(gas,gas_total)

sector = projections_ghgi %>%
  filter(proj_name == "gcam_wm_lowcostre_highfuelcost_gtm")%>%
  group_by(year,usproj_sector) %>%
  summarise(value = sum(value)) %>%
  filter(year %in% config$fives_proj_sm) %>%
  pivot_wider(names_from = "year", values_from = "value")

sector_total = projections_ghgi %>%
  filter(proj_name == "gcam_wm_lowcostre_highfuelcost_gtm")%>%
  group_by(year,usproj_sector) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  filter(year %in% config$fives_proj_sm) %>%
  group_by(year) %>%
  summarise(value=sum(value)) %>%
  mutate(usproj_sector = "total") %>%
  pivot_wider(names_from = "year", values_from = "value") %>%
  select(usproj_sector,`2025`,`2030`,`2035`,`2040`)

sector_all = rbind(sector,sector_total)

explore = projections_ghgi %>%
  filter(proj_name == "gcam_wm_lowcostre_highfuelcost_gtm")

unique_gas_sources_proj_ghgi_table = unique(
  (explore %>% mutate(concat = paste(usproj_sector,gas,usproj_category, sep="_")))$concat) 


