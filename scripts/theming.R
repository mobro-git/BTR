## elements to add to figures

xup <- theme(axis.text.x = element_text(vjust = 12))
xup_light <- theme(axis.text.x = element_text(vjust = 10))
slantx <- theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))
bottom1 <- theme(legend.position = "bottom", legend.title = element_blank())
nolegend <- theme(legend.position='none')
smlegend <- theme(
  legend.title = element_text(size = 9),
  legend.text  = element_text(size = 9)
)


blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x=element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

# Color Palette

master_palette <- c(
  "very dark blue" = "#003052",
  "light orange" = "#FDD56D",
  "very dark teal" = "#2A4849",
  "dark salmon" = "#E6544D",
  "mauve" = "#8F507F",
  "dark gray blue" = "#595764",
  "teal blue" = "#0388B3",
  "teal green" = "#2EAD96",
  "very dark olive" = "#414833",
  "light salmon" = "#F28063",
  "dark cobalt" = "#442F91",
  "cerulean blue" = "#2b59c3",
  "dark cornflower" = "#253c78",
  "cinnamon" = "#d36582",
  "cream" = "#ffeecf",
  "light brown" = "#c9a690",
  "mauve" = "#6d2e46",
  "copper" = "#a26769",
  "gray pink" = "#d5b9b2",
  "dark gray pink" = "#AF8D84",
  "darkest gray pink" = "#80665F",
  "gray green" = "#5f7470",
  "aqua" = "#2ab7ca",
  "black" = "#000000",
  "MIT Red" = "#af0220",
  "m.teal" = "#148896",
  "m.blue" = "#044980",
  "m.green" = "#149664",
  "dark m.green" = "#02643E",
  "m.red" = "#E51A32",
  "m.orange" = "#E5671A",
  "m.pink" = "#E51A98",
  "m.yellow" = "#DAC125",
  "dark m.yellow" = "#DAAB25",
  "m.neongreen" = "#99DA25",
  "m.burnorange" = "#DA6625",
  "m.indigo" = "#7539C6",
  "m.violet" = "#BC39C6",
  "m.cobalt" = "#3943C6",
  "light blue" = "#9CA3F9",
  "lts green" = "#96BBA4",
  "brown" = "#BC8F73",
  "dark brown" = "#825335",
  "slate" = "#767684",
  "light slate" = "#9A9AB5",
  "dark slate" = "#36363B",
  "brown red" = "#733D3D",
  "dark brown red" = "#481B1B",
  "light purple" = "#B390B6",
  "dark gray" = "#363636",
  "dark gray red" = "#6B5050",
  "dark gray purple" = "#66506B",
  "dark gray green" = "#506B5E",
  "dark gray blue" = "#505A6B",
  "dark gray yellow" = "#686B50",
  "mid gray" = "#676767",
  "mid gray red" = "#723535",
  "mid gray purple" = "#653572",
  "mid gray green" = "#357238",
  "mid gray blue" = "#353B72",
  "mid gray yellow" = "#6F7235",
  "light gray" = "#DEDEDE",
  "normal gray" = "#A1A1A1",
  "bright yellow" = "#FFF362",
  "darker yellow" = "#FFEB00",
  "dark dark yellow" = "#D5C500",
  "darkest yellow" = "#AA9C00",
  "bright purple" = "#C788FF",
  "darker purple" = "#A761E5",
  "dark dark purple" = "#6A23A7",
  "darkest purple" = "#450081",
  "bright green" = "#75FF8E",
  "darker green" = "#3CE259",
  "dark dark green" = "#16B231",
  "darkest green" = "#007515",
  "bright blue" = "#74A3FF",
  "darker blue" = "#477ADD",
  "dark dark blue" = "#1F4FAB",
  "darkest blue" = "#002A7B",
  "hue1" = "#4E5357",
  "hue2" = "#5B7C93",
  "hue3" = "#5A9DCB",
  "hue4" = "#40A4EA",
  "hue5" = "#0097FF",
  
  "maroonD" = "#800000",
  "brownD" = "#9a6324",
  "oliveD" = "#808000",
  "tealD" = "#469990",
  "navyD" = "#000075",
  "redD" = '#e6194b',
  "orangeD" = '#f58231',
  "yellowD" = '#ffe119',
  "limeD" = '#bfef45',
  "greenD" = '#3cb44b',
  "cyanD" = '#42d4f4',
  "blueD" = '#4363d8',
  "purpleD" = "#911eb4",
  "magentaD" = '#f032e6',
  "pinkD" = "#fabed4",
  "apricotD" = "#ffd8b1",
  "beigeD" = "#fffac8",
  "mintD" = "#9FEDB6",
  "lavenderD" = "#dcbeff"
)

# Color Mapping - labels matched to hex codes

color_map <- c(
  # scenarios
  "wm" = "teal blue",
  "With Measures" = "teal blue",
  "2024 BTR, Sens." = "mauve",
  "wm_sens" = "mauve",
  "2024 Policy Baseline" = "teal blue",
  "Historical" = "black",
  "leep_IRA" = "redD",
  "IRA" = "hue1",
  "LEEP" = "redD",
  "Long-Term Strategy" = "lts green",
  
  # scenarios - full names
  "Reference" = "black",
  "High Fuel Cost" = "yellowD",
  "Low Fuel Cost" = "dark salmon",
  "Advanced Technology" = "darker green",
  "High Fuel Cost, Low Cost Renewables" = "teal blue",
  
  # models
  "OP-NEMS" = "dark dark blue",
  "GCAM" = "dark dark yellow",
  "USREP-ReEDS" = "MIT Red",
  "EPA-GHGI" = "light orange",
  
  # fuels
  "Fossil" = "very dark blue",
  "Non-Electricity" = "teal green",
  "Biogas" = "apricotD",
  "Biomass" = "m.violet",
  "Biomass Liquids" = "m.violet",
  "Biomass Solids" = "m.indigo",
  "Coal" = "very dark blue",
  "Gas" = "brown",
  "Oil" = "brown red",
  "Solar" = "m.yellow",
  "Wind" = "m.green",
  "Hydro" = "m.cobalt",
  "Geothermal" = "maroonD",
  "Ocean" = "m.blue",
  "Nuclear" = "#E01616",
  "Storage" = "gray pink",
  "Battery" = "bright green",
  "PSH" = "very dark blue",
  "Total Solar" = "m.yellow",
  "Solar from CSP" = "very dark blue",
  "Solar from PV" = "m.violet",
  "Total Wind" = "m.green",
  "Offshore Wind" = "very dark blue",
  "Onshore Wind" = "m.violet",
  "Peak summer electricity load" = "very dark blue",
  "Peak winter electricity load" = "m.violet",
  
  # technology
  "Fossil w/ CCS" = "dark cobalt",
  "Fossil w/o CCS" = "dark gray blue",
  "Biomass w/o CCS" = "m.indigo",
  "Biomass w/ CCS" = "m.violet",
  "Coal w/ CCS" = "slate",
  "Coal w/o CCS" = "dark slate",
  "Gas w/ CCS" = "brown",
  "Gas w/o CCS" = "dark brown",
  "Oil w/ CCS" = "brown red",
  "Oil w/o CCS" = "dark brown red",
  "Solar CSP" = "dark m.yellow",
  "Solar PV" = "m.yellow",
  "Storage Battery" = "gray pink",
  "Storage PSH" = "dark gray pink",
  "Storage Other" = "darkest gray pink",
  "Wind Offshore" = "m.green",
  "Wind Onshore" = "dark m.green",
  "Other" = "light purple",
  
  "Electrolysis" = "teal blue",
  "Ethanol" = "cinnamon",
  "Photoelectrochemical" = "dark m.yellow",
  
  "Renewables" = "teal green",
  "Electricity" = "teal blue",
  "Hydrogen" = "m.burnorange",
  "Non-Fossil Combustion" = "#EA9C9C",
  "Biomass and H2" = "teal green",
  "Synthetic Gas" = "cinnamon",
  "Synthetic Liquids" = "copper",
  "Synthetic Fuels" = "copper",
  
  "Fuel Cell" = "teal green",
  "NG Blend" = "cinnamon",
  
  "Biomass gassification w/ CCS" = "dark cobalt",
  "Biomass gassification w/o CCS" = "dark gray blue",
  "Coal gassification to hydrogen plants w/ CCS" = "m.indigo",
  "Hydrogen-by-electrolysis plants" = "m.violet",
  "Ethanol SMR w/o CCS" = "slate",
  "Gas SMR w/ CCS" = "gray pink",
  "Gas SMR w/o CCS" = "brown",
  "Photoelectrochemical w/o CCS" = "dark brown",
  "Thermochemical w/o CCS" = "m.yellow",
  
  #sectors
  "Total" = "black",
  "AFOLU" = "m.green",
  "LULUCF" = "dark dark green",
  "LULUCF Sink" = "dark dark green",
  "Biofeedstock" = "teal green",
  "Biofuels" = "teal green",
  "BECCS" = "m.violet",
  "Fossil CCS" = "brown",
  "Chemical Feedstocks" = "very dark teal",
  "Refining" = "very dark teal",
  "Petroleum Refining" = "very dark blue",
  "Industrial Processes" = "slate",
  "Energy" = "teal blue",
  "Energy & Industrial Processes" = "teal blue",
  "Buildings" = "light orange",
  "Transportation" = "dark salmon",
  "Industry" = "m.green",
  "Heat" = "m.red",
  "Energy Supply (pos)" = "m.violet",
  "Energy Supply (neg)" = "teal green",
  "Final Energy" = "m.violet",
  "Primary Energy" = "black",
  "Residential" = "teal blue",
  "Commercial" = "light orange",
  "EInt Mfg" = "light orange",
  "NonEInt Mfg" = "teal blue",
  "Non Mfg" = "m.green",
  
  "Total Energy Demand" = "teal green",
  "Fossil Energy Demand" = "light slate",
  "Electricity Demand" = "teal blue",
  "CO2 Emissions" = "dark salmon",
  "Residual Emissions" = "teal blue",
  
  #emissions accounting
  "Net CO2" = "very dark teal",
  "Gross Positive CO2" = "dark salmon",
  "CO2 + Fossil CCS" = "light orange",
  "BECCS (neg)" = "teal green",
  "BECCS + LULUCF (neg)" = "dark dark green",
  "BECCS + LULUCF + DAC (neg)" = "darkest green",
  
  # transportation,
  "Freight" = "m.red",
  "Offroad" = "teal green",
  "Passenger" = "teal blue",
  "Pipeline" = "very dark teal",
  "Aviation" = "light orange",
  "Rail" = "dark salmon",
  "Road" = "m.indigo",
  "Shipping" = "m.green",
  "Diesel" = "dark m.yellow",
  "Gasoline" = "m.neongreen",
  "Jet Fuel" = "m.burnorange",
  
  # emissions
  "Direct" = "teal blue",
  "Indirect" = "teal green",
  "*Indirect" = "teal green",
  "CO2" =  "teal blue",
  "Non-CO2" = "dark salmon",
  "CH4" = "teal blue",
  "Non-CO2" = "dark salmon",
  "F-Gases" = "light orange",
  "Land Sink"= "teal green",
  "CDR" = "very dark blue",
  "DAC" = "very dark blue",
  "Net-GHG" = "#B2B3BD",
  
  # geography
  "Asia*" = "dark m.yellow",
  "Canada*" = "very dark teal",
  "EU*" = "m.green",
  "Latin America*" = "m.indigo",
  "OECD: IPCC SR15" = "normal gray",
  "US: IPCC AR6" = "light gray",
  "US: EMF 37" = "dark salmon",
  "US/Canada*" = "teal blue",
  
  # welfare
  "Welfare" = "teal blue",
  "Quintile 1" = "hue1",
  "Quintile 2" = "hue2",
  "Quintile 3" = "hue3",
  "Quintile 4" = "hue4",
  "Quintile 5" = "hue5",
  
  # NDC Targets
  "17% Below 2005" = "dark m.yellow",
  "26-28% Below 2005" = "m.green", 
  "50-52% Below 2005" = "m.indigo",
  
  # NCBR Vintages
  "2024 BTR High" = "teal blue",
  "2024 BTR Low" = "teal blue",
  "2024 BTR" = "teal blue",
  '2022 NC' = 'm.green',
  '2022 BR' = 'm.green',
  '2021 NC' = 'teal green',
  '2021 CAR' = 'teal green',
  '2021 BR' = 'teal green',
  '2016 BR' = 'dark salmon',
  '2016 CAR' = 'dark salmon',
  '2014 NC' = 'light orange',
  '2014 CAR' = 'light orange',
  '2010 NC' = 'very dark blue',
  '2010 CAR' = 'very dark blue',
  '2006 NC' = 'hue1',
  '2006 CAR' = 'hue1',
  'ghgi' = "black",
  
  "btr_2024" = "teal blue",
  'br_2022' = 'm.green',
  'br_2021' = 'teal green',
  'br_2016' = 'dark salmon',
  'car_2014' = 'light orange',
  'nc_2010' = 'very dark blue',
  'car_2010' = 'very dark blue',
  'nc_2006' = 'hue1',
  'car_2006' = 'hue1',
  
  "2024 Policy Baseline" = "teal blue",
  "2024 Policy Baseline, No Sens." = "hue1",
  "2022 Biennial Report" = 'm.green',
  "2021 Biennial Report" = 'teal green',
  "2016 Biennial Report" = 'dark salmon',
  "2014 Climate Action Report" = 'light orange',
  

  # Kaya Variables
  "EmissPerEnergyIn" = "m.green",
  "EnergyPerGDPIn" = "teal blue",
  "GDPPerCapIn" = "dark salmon",
  "PopulationIn" = "very dark blue",
  
  # kaya
  "Population" = "teal blue",
  "GDP/Capita" = "dark m.yellow",
  "Energy/GDP" = "dark salmon",
  "Emissions/Energy" = "m.green",
  "GDP / Capita" = "dark m.yellow",
  "Energy / GDP" = "dark salmon",
  "Emissions / Energy" = "m.green",
  
  # Sector Analysis
  "Long-Term Strategy" = "m.green",
  "Median Value" = "teal blue",
  
  #BRVS
  "2023 BR Voluntary Supplement: All Models" = "dark dark yellow",
  "2023 BR Voluntary Supplement - BTR Subset" = "dark dark yellow",
  "2023 BR Voluntary Supplement" = "dark dark yellow"
  


  
  ) %>%
  find_color(master_palette)

sub_palettes <- list(
  `test` = c(
    "MARKAL-NETL" = "MARKAL-NETL",
    "NATEM" = "NATEM",
    "TEMPO" = "TEMPO",
    "US-REGEN" = "US-REGEN"
  )
) %>%
  map( ~ find_color(.x, color_map))
