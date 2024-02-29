
# Themes

theme_emf <- function(fig_size = "auto") {

  if(fig_size == "auto") {
    theme <- theme_light() +
      theme(text = element_text(size = 12),
            legend.background = element_rect(fill="white",color=NA),
            legend.title=element_blank(),
            legend.position="right",
            plot.margin = margin(8,10,8,8),
            axis.text.y = element_text(size=9),
            axis.text.x = element_text(size=9),
            #axis.line = element_line(color="black"),
            axis.title.y = element_text(face="bold",vjust=2),
            strip.text.x = element_text(size = 10, color = "black",face = "bold"),
            strip.text.y = element_text(size = 9, color = "black",face = "bold"),
            strip.background = element_rect(fill=NA, size=1))#C5CFE3
  }
  else if(fig_size == "small") {
    theme <- theme_light() +
      theme(text = element_text(size = 11),
            plot.subtitle = element_text(face="bold",size = 10,hjust=0.5),
            axis.text.y = element_text(size=10),
            axis.title.y = element_text(size=10,face="bold",vjust=2))
    theme
  }}

xup <- theme(axis.text.x = element_text(vjust = 12))
xup_light <- theme(axis.text.x = element_text(vjust = 10))
slantx <- theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))
bottom1 <- theme(legend.position = "bottom", legend.title = element_blank())
nolegend <- theme(legend.position='none')

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

  # official RTI LEEP Report color palette
  "Lsalmon" = "#DD614A",
  "Lteal" = "#6a959b",
  "Lblue" = "#2A546C",
  "Lgold" = "#AE7F2C",
  "Lpurple" = "#8B507E",
  "Lgreen" = "#488C49",
  "Lmagenta" = "#af499b",
  "Lnavy" = "#25266b",
  "Laqua" = "#11897d",
  "Lsky" = "#0083ca",
  "Lorange" = "#f15a22",
  "Lindigo" = "#524fa1",
  "Lpink" = "#b72467",
  "Lbrown" = "#876123",
  "black" = "black",
  "Ldeepbrown" = "#3c2415",
  "Ldarkbrown" = "#603913",
  "Ldarkgray" = "#58595b",
  "Lgray" = "#808285",
  "Ldarkred" = "#be1e2d",
  "Lred" = "#ed1c24",

  # old colors
  "light salmon" = "#F28063",
  "teal blue" = "#0388B3"
  )

# Color Mapping - labels matched to hex codes

color_map <- c(
  # scenarios
  "Historic" = "black",
  "Historical" = "black",
  "IRA" = "Lsky",
  "Reference" = "Lsalmon",
  "No IRA" = "Lsalmon",
  "IRA.High" = "Lteal",
  "IRA.Low" = "Lsky",

  # "IRA" = "teal blue",
  # "Reference" = "light salmon",
  # "No IRA" = "light salmon",

  # models
  "NEMS-EIA" = "black", #AEO 2023 outputs from OnLocation
  "EIA"= "black",
  "EIA-LTS"= "black",
  "EIA-STEO"= "black",
  "EPA-GHGI" = "black",
  "EPS-EI" = "Lsalmon",
  "GCAM-CGS" = "Lteal",
  "GCAM-LTS" = "Lteal",
  "GCAM-PNNL" = "Lblue",
  "Haiku-RFF" = "Lgold",
  "IPM-EPA" = "Lpurple",
  "IPM-NRDC" = "Lgreen",
  "MARKAL-NETL" = "Lmagenta",
  "NEMS-RHG" = "Lnavy",
  "NEMS-OP" = "Laqua",
  "NEMS-OP-LTS" = "Laqua",
  "REGEN-EPRI" = "Lorange",
  "RIO-REPEAT" = "Lindigo",
  "ReEDS-NREL" = "Lpink",
  "Scout-LEEP" = "Lbrown",
  "USREP-ReEDS" = "Lsky",

  # technology
  "Biomass w/o CCS" = "Lindigo",
  "Biomass w/ CCS" = "Lpink",
  "Coal" = "Ldarkbrown",
  "Coal w/ CCS" = "Ldeepbrown",
  "Coal w/o CCS" = "Ldarkbrown",
  "Natural Gas" = "Lgray",
  "Gas w/ CCS" = "Ldarkgray",
  "Gas w/o CCS" = "Lgray",
  "Geothermal" = "Lblue",
  "Hydro" = "Lgreen",
  "Hydrogen" = "Ldarkred",
  "Nuclear" = "Lsalmon",
  "Oil" = "Lmagenta",
  "Petroleum" = "Lmagenta",
  "Solar" = "Lgold",
  "Wind" = "Lsky",
  "Storage" = "Lnavy",
  "Other" = "Lteal",
  "Renewables" = "Lgreen",

  # sectors
  "Total" = "black",
  "Transportation: Direct" = "Lteal",
  "Transportation: Indirect" = "Lblue",
  "Industry: Direct" = "Lgreen",
  "Industry: Indirect" = "Laqua",
  "Industry: Process" = "Lgray",
  "Buildings: Direct" = "Lgold",
  "Buildings: Indirect" = "Lbrown",
  "Transportation" = "Lblue",
  "Industry" = "Lgreen",
  "Buildings" = "Lgold",

  # industrial emissions
  "Food Processing" = "Lteal", ### light, Lnavy for heavy
  "Other Light Industry" = "Laqua",
  "Cement" = "Lsalmon",
  "Other Heavy Industry" = "Lgold",
  "Iron and Steel" = "Lgreen",
  "Paper" = "Lpink",
  "Refining" = "Lindigo",
  "Chemicals" = "Ldarkred",
  "Light" = "Lteal",
  "Heavy" = "Lnavy",

  # transportation methods
  "Passenger Cars" = "Lblue",
  "LD Trucks" = "Lsky",
  "MHD Trucks" = "Lgreen",
  "Aircraft" = "Lgold",
  "Water" = "Lnavy",
  "Pipelines" = "Lmagenta"
  ) %>%
  find_color(master_palette)

sub_palettes <- list(
  `test` = c(
    "ADAGE"  = "ADAGE" ,
    "AnyMOD" = "AnyMOD"
  )
) %>%
  map( ~ find_color(.x, color_map))


