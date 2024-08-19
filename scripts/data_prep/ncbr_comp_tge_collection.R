
source('packages.R')

all_tge_ar5 = read_csv("data-raw/ncbr_comparison/AllReports_AllARs_usproj.csv") %>% 
  filter(GWP == "AR5") 

tge_2014 = all_tge_ar5 %>%
  filter(Report == "car_2014" & 
           category == "GrossTotal" & 
           gas == "Total") %>%
  select(Report,GWP,unit,Year,Emissions)

tge_2016 = all_tge_ar5 %>%
  filter(Report == "br_2016" &
           scenario == "WM") %>%
  group_by(Report,GWP,unit,Year) %>%
  summarise(Emissions = sum(Emissions, na.rm = TRUE))

tge_2021 = all_tge_ar5 %>%
  filter(Report == "br_2021" &
           category == "GrossTotal" &
           scenario %in% c("historical","WM")) %>%
  group_by(Report,GWP,unit,Year) %>%
  summarise(Emissions = sum(Emissions, na.rm = TRUE))

tge_2022 = all_tge_ar5 %>%
  filter(Report == "br_2022" &
           category == "GrossTotal" &
           scenario %in% c("historical","WM")) %>%
  group_by(Report,GWP,unit,Year) %>%
  summarise(Emissions = sum(Emissions, na.rm = TRUE))

ggplot() +
  geom_line(data = tge_2014, aes(x = Year, y = Emissions, color = Report, group = Report)) +
  geom_line(data = tge_2016, aes(x = Year, y = Emissions, color = Report, group = Report)) +
  geom_line(data = tge_2021, aes(x = Year, y = Emissions, color = Report, group = Report)) +
  geom_line(data = tge_2022, aes(x = Year, y = Emissions, color = Report, group = Report)) +
  ylim(c(3000,9000)) 

tge_all_ar5_sm = rbind(tge_2014,tge_2016,tge_2021,tge_2022) %>%
  mutate(
    variable = "Total Gross GHG Emissions",
    notes = "Data pulled from data-raw/ncbr_comparison/AllReports_AllARs_usproj.csv in BTR repo. AllReports_AllARs_usproj.csv created in the usproj repo")

write_csv(tge_all_ar5_sm, "data-raw/ncbr_comparison/total_gross_ghg_ncbr_comparisons_ar5.csv")
