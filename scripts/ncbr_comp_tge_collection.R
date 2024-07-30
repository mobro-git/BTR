
source('packages.R')

car2014 <- read_csv('data-extra/ARChanges/car2014_data_AR5.csv')

tge_car2014 <- car2014 %>%
  filter(gas == 'Total',
         category == 'GrossTotal') %>%
  rename(value = Emissions) %>% 
  select(publication,Year,value)

tge_car2014_wide <- tge_car2014 %>%
  pivot_wider(names_from = Year)

write_csv(tge_car2014_wide, 'data-extra/ARChanges/tge_car2014_wide.csv')



br2016 <- read_csv('data-extra/ARChanges/br2016_data_AR5.csv')

tge_br2016 <- br2016 %>%
  filter(gas == 'Total',
         category == 'GrossTotal') %>%
  rename(value = Emissions) %>% 
  select(publication,Year,value)

tge_br2016_wide <- tge_br2016 %>%
  pivot_wider(names_from = Year)

write_csv(tge_br2016_wide, 'data-extra/ARChanges/br2016_wide.csv')
