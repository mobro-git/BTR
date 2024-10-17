## Unit conversion code from usproj repo 03/31/2022

# Unit conversion references:
# USDA: https://www.ers.usda.gov/webdocs/publications/41880/33132_ah697_002.pdf?v=0
# IPCC AR5 WG3 metric & methodology: https://www.ipcc.ch/site/assets/uploads/2018/02/ipcc_wg3_ar5_annex-ii.pdf
# TODO: promote to a vignette?

#' Code to create needed conversions for user-defined units and map unit names to achieve consistency between all model results
#'
#' To introduce a new base unit or conversion constant, use the command units::install_unit
#' Note that units cannot end or start in numbers, and cannot contain spaces or special characters
#' Instead of creating conversions for each unit, create a conversion constant (or use an existing constant below)
#'
#' @import units
#'
install_extra_units <- function() {

  # Conversion constants
  units::install_unit("dozen", "12 1", "Dozen")
  units::install_unit("hundred", "1e2 1", "Hundred")
  units::install_unit("thousand", "1e3 1", "Thousand")
  units::install_unit("million", "1e6 1", "Million")
  units::install_unit("billion", "1e9 1", "Billion")
  units::install_unit("trillion", "1e12 1", "Trillion")
  units::install_unit("quadrillion", "1e15 1", "Quadrillion")

  # Population
  # units has base unit 'count' which may be equiv here
  units::install_unit("head")
  units::install_unit("person")
  units::install_unit("capita") # remove? or conversion constant of person?
  units::install_unit("number") # remove?

  # Petroleum and natural gas
  ong_names <- c("event", "blowdown event", "blowout", "well",
                 "venting well", "oil well", "HF oil well", "hdr",
                 "heater treaters", "heater", "device", "tank", "floating roof tank",
                 "valve", "separator", "sep", "dehydrator","desiccant dehydrator",
                 "desiccant", "controller", "vessel", "pump", "active pump",
                 "compressor", "PRV", "station", "pig station", "pump station",
                 "complex", "Major complex", "Minor complex", "loading",
                 "AGRU", "AGR", "plant", "service", "flare", "flared gas")
  ong_symbols <- c("events", "blowdown events", "blowouts", "wells",
                   "venting wells", "oil wells", "HF oil wells", "hdrs",
                   "HT", "heaters", "devices", "tanks", "floating roof tanks",
                   "valves", "separators", "seps", "dehydrators", "desiccant dehydrators",
                   "desiccants", "controllers", "vessels", "pumps", "active pumps",
                   "compressors", "PRVs", "stations", "pig stations", "pump stations",
                   "complexes", "Major complexes", "Minor complexes", "loadings",
                   "AGRUs", "AGR units", "plants", "services", "flares", "Flared gas")
  units::install_unit(ong_names, "1 count", ong_symbols)
  units::install_unit("MMHPhr", "1e6 horsepower per hour", "MMHP-hr")
  units::install_unit("MMHP", "1e6 horsepower")
  units::install_unit("gas plants", "1 plant")
  units::install_unit("mile main", "1 mile")
  units::install_unit("miles crude pipeline", "1 mile")
  units::install_unit("large cmprs", "1 compressor")
  units::install_unit("# complexes", "1 complex")
  units::install_unit("loaded", "unitless")
  units::install_unit("produced water", "unitless")
  units::install_unit("hvy. crude", "unitless")
  units::install_unit("lt. crude", "unitless")
  units::install_unit("Refinery Feed", "unitless", "refinery feed")

  # Weight
  units::install_unit("hundredweight", "100 pounds") # see USDA
  units::install_unit("bales") # variation by crop and standard

  # Emissions
  units::remove_unit("kt") # in default units database, kt is a symbol for knots. After removal, kt is recognized as kilotonnes ('k' prefix with symbol 't' for metric tonnes)

  units::install_unit("CO2e", "unitless")
  units::install_unit("tCO2e", "t CO2e")
  units::install_unit("MMTCO2e", "1e6 tCO2e")
  units::install_unit("gCO2e", "g CO2e")
  units::install_unit("MMT", "1e6 tonnes")

  # under this approach, any of the following can be understood by the system:
  # c("MMTCO2e", "TgCO2e", "MtCO2e", "million tonnes CO2e")

  # Area
  units::install_unit("MSI")

  # Energy
  units::install_unit("kWh")
  units::install_unit("MMBtu", "1e6 Btu")
  units::install_unit("MMbbl", "1e6 barrels")
  units::install_unit("mbbl", "1e3 barrels", "Mbbl")
  units::install_unit("MMcf", "1e6 ft^3", "MMscf")
  units::install_unit("cf", "1 ft^3", "cubic feet")
  units::install_unit("tcf", "1e3 ft^3")
  units::install_unit("Mcf", "1e6 ft^3")
  units::install_unit("Bcf", "1e9 ft^3")
  units::install_unit("Tcf", "1e12 ft^3")


  # TODO: I am thinking the following emissions units should be removed. In general, GWP conversions are too complicated for the units framework. While CO2e is a unit in which any substance can be measured, the others are not generally used that way.

  #units::install_unit("N2O") # is this being used for N2O or N fert?
  #units::install_unit("BOD")


  # Finances
  # TODO: in general, conversions between dollar years need their own approach, probably not handled through the units package
  units::install_unit("dollars")
  units::install_unit("cents", "0.01 dollars")
  #units::install_symbolic_unit("USD2010_")


  # called from onLoad, this object ensures called just once per session
  extra_units_installed <<- TRUE

}


#' Standardize Unit Names by Converting Aliases to Units package names
#'
#' Applies string substitutions to the units in a units column to ensure that the units can be
#' parsed by the units package. This is currently done as a step in handle_units, but it will
#' be moved into data-loading functions in the future.
#'
#' @import units
#'
unit_name_standardize <- function(unit) {

  unit <- case_when(
    unit %in% MMTCO2e_equiv_units ~ MMTCO2e_equiv_units[[1]],
    unit %in% kt_equiv_units ~ kt_equiv_units[[1]],
    TRUE ~ unit)

  unit <- gsub(" per ", "/", unit)
  unit <- gsub("Number", "number", unit)
  unit <- gsub("N", "N2O", unit) # TODO: is this N fertilizer or N2O?
  unit <- gsub("cwt.", "hundredweight", unit)
  unit <- gsub("short ton", "ton", unit)
  unit <- gsub("fraction", "percent/hundred", unit)
  unit <- gsub(" ", "*", unit)
  unit <- gsub("-", "*", unit)
  unit <- gsub("Kg", "kg", unit)
  unit <- gsub(" of ", "*", unit)

  str_replace_all(unit, coll(units_package_name_aliases))
}

units_package_name_aliases <- c(
  "quads" = "quadrillion Btu",
  "short ton" = "ton"
  # tonne is metric, ton is U.S.
)

#' Takes a dataframe with a value column and a units column and creates a combined column,
#' replacing the original value column. This column contains either units objects or mixed_units
#' objects, depending on the number of different units in the original units column.
#'
#' @import units
#'
handle_units <- function(df, rename_fun = unit_name_standardize) {
  value <- df$value
  unit <- do.call(rename_fun, list(df$unit))
  units_options(set_units_mode = "standard")
  if (length(unique(unit)) > 1) {
    value <- mixed_units(value, unit)
  } else {
    value <- set_units(value, value=unit[[1]])
  }

  df$value <- value
  select(df, -unit) # remove unit column now that value is a unit object
}

#' Takes a dataframe with a value column made up of units objects or mixed_units objects
#' and splits it into two columns, one with units information and one with value information
#'
#' @import units
#'
flatten_units <- function(df) {
  valunit <- df$value
  if (class(valunit)[1] == "units") {
    value <- drop_units(valunit)
    value_length <- length(value)
    unit_str <- deparse_unit(valunit)
    unit <- rep(c(unit_str), value_length)
    df$value <- value
    df$unit <- unit
  } else if  (class(valunit)[1] == "mixed_units") {
    value <- drop_units(valunit)
    value_length <- length(value)
    unit <- sapply(valunit, deparse_unit)
    df$value <- value
    df$unit <- unit
  }
  df
}



#' #' install_extra_units
#' #' install needed units
#' #'
#' #' Code to create needed conversions for user-defined units and map unit names to achieve consistency between all model results
#' #'
#' #' To introduce a new unit, use the command units::install_symbolic_unit
#' #' To create a conversion between a pair of units, use the command units::install_conversion_constant(unit1, unit2, conversionrate)
#' #' Note that units cannot end or start in numbers, and cannot contain spaces or special characters
#' #'
#' #' @import units
#' #'
#'
#' TODO: convert to units::install_unit, install_conversion_constant is deprecated in newest version of units package
#'
#' install_extra_units <- function() {
#'
#'   # dimensionless
#'   units::install_unit("thousand", "1e3 1", "Thousand")
#'   units::install_unit("million", "1e6 1", "Million")
#'   units::install_unit("billion", "1e9 1", "Billion")
#'   units::install_unit("quadrillion", "1e15 1", "Quadrillion")
#'
#'   units::install_conversion_constant("per", "1", 10^-1)
#'
#'   # Energy units
#'   units::install_conversion_constant('MMBtu','Btu', 10^6)
#'   units::install_conversion_constant('kwh','watthour', 10^3)
#'   units::install_conversion_constant('bkwh','kwh', 10^9)
#'   units::install_conversion_constant('short_tons_coal','Btu', 18875000)
#'   units::install_conversion_constant('cubic_feet','Btu', 1037) # From EIA: https://www.eia.gov/tools/faqs/faq.php?id=45&t=8
#'   units::install_conversion_constant('barrelnew','GJ', 6.011708) # From EIA calculator (https://www.eia.gov/energyexplained/units-and-calculators/energy-conversion-calculators.php)
#'   # the units package is not allowing the conversion between barrels and GJ, but since both are individually recognized by the package, we cannot define a conversion between them
#'
#'   # Emissions
#'   units::remove_unit("kt") # in default units database, kt is a symbol for knots. After removal, kt is recognized as kilotonnes ('k' prefix with symbol 't' for metric tonnes)
#'
#'   units::install_symbolic_unit('Million_ton_CO2_yr', dimensionless = TRUE)
#'
#'   # Monetary units and conversions
#'   units::install_symbolic_unit('cent', dimensionless = FALSE)
#'   units::install_conversion_constant('cent','cents', 1)
#'   units::install_conversion_constant('cent','dollar', 1/100)
#'   units::install_conversion_constant('dollars','dollar', 1)
#'   units::install_conversion_constant('dollar', 'US_2018_Dollars', 1) # units cannot end or start in numbers
#'   units::install_conversion_constant('US_2012_Dollars', 'US_2018_Dollars', 106.10422/101.51688) # Producer Price Index: Total Consumer Goods, from FRED PITGCG01USA661N
#'   units::install_conversion_constant('US_2016_Dollars', 'US_2018_Dollars', 106.10422/98.47496) # Producer Price Index: Total Consumer Goods, from FRED PITGCG01USA661N
#'   units::install_conversion_constant('US_2019_Dollars', 'US_2018_Dollars', 106.10422/106.43044) # Producer Price Index: Total Consumer Goods, from FRED PITGCG01USA661N
#'   units::install_conversion_constant('US_2019_cents', 'US_2019_Dollars', 1/100)
#'
#'
#'   extra_units_installed <<- TRUE
#'
#' }
#'
#' # Just once per session so you can devtools::load_all() more than once per session without it yelling at you
#' if(!exists("extra_units_installed")) install_extra_units()
#'
#'
#' ################################### unused functions ###################################
#'
#'
#' #' Convert Units
#' #'
#' #' @param from_values numeric vector
#' #' @param from_units character vector, same length as from_values
#' #' @param to_units character vector, same length as from_values
#' #'
#' #' Units are represented as character vectors, will be searched for within `units` package
#' #' and custom units defined for this analysis.
#' #'
#' #' Steps:
#' #' 0) Check that all from and to units are recognizable and convertible
#' #' 1) Translate the character representations to representations recognized by `units` for source and destination units
#' #' 2) Make units objects, translate to new units
#' #' 3) Untranslate and simplify objects
#' #'
#' convert_units_numeric <- function(from_value, from_unit, to_unit) {
#'
#'   # build translation to units recognized by `units` package
#'   translate_source_to_units_unit <- tibble(
#'     from_unit = from_unit,
#'     to_unit = to_unit
#'   ) %>%
#'     distinct(from_unit, to_unit) %>%
#'     mutate(from_units_unit = str_replace_all(from_unit, coll(units_package_names)),
#'            to_units_unit = str_replace_all(to_unit, coll(units_package_names))) %>%
#'
#'     mutate(conv_factor = as.numeric(set_units(mixed_units(1, from_units_unit), to_units_unit)))
#'
#'   # apply conversion factors
#'   res_data <- tibble(
#'     from_unit = from_unit,
#'     to_unit = to_unit,
#'     from_value = from_value
#'   ) %>%
#'     left_join(translate_source_to_units_unit, by = c("from_unit", "to_unit")) %>%
#'     mutate(to_value = from_value * conv_factor)
#'
#'   res_data$to_value
#' }
#'
#' # Map unit names to allow comparison between the same variables from different models
#'
#' # Named vector to make changes withing the character strings of the 'to' and 'from' unit columns
#' # to be compatible with units package either directly or to be converted using 'map_to_units_package'
#'
#' #' Convert unit name aliases (character vector form) to standardized units package versions
#' #'
#' #' @param x character vector of unit names
#' #'
#' unit_name_standardize <- function(x) {
#'   str_replace_all(x, coll(units_package_names))
#' }
#'
#' units_package_names <- c(
#'   '2019 $/b' = 'US_2019_Dollars/barrelnew',
#'   'US$2018' = 'US_2018_Dollars',
#'   '2012  $' = 'US_2012_Dollars',
#'   'chained 2012 dollars (seasonally-adjusted annual rate)' = 'US_2012_Dollars',
#'   '2019 $/st' = 'US_2019_Dollars/short_ton',
#'   '2019 $' = 'US_2019_Dollars',
#'   '2019 cents' = 'US_2019_cents',
#'   'kilowatt hour' = 'kWh',
#'   'quads' = 'quadrillion btu',
#'   'BkWh' = 'billion kwh',
#'   'million Btu' = 'MMBtu',
#'   'MMmt CO2' = 'Million_ton_CO2_yr',
#'   'Mt CO2/yr' = 'Million_ton_CO2_yr',
#'   'million metric tons CO' = 'Million_ton_CO2_yr',
#'   'metric tons' = 'metric_tons',
#'   'cubic feet' = 'cubic_feet',
#'   'short tons' = 'short_tons',
#'   '/yr' = '',
#'   'per' = '/'
#' )
#'
#' # rename units & eia names -> template names
#' map_to_emf_template <- c(
#'   "US_2018_Dollars/GJ"      = "US$2018/GJ",
#'   "MMmt CO2"                = "Mt CO2/yr",
#'   "million metric tons CO2" = "Mt CO2/yr",
#'   "Million_ton_CO2_yr"      = "Mt CO2/yr",
#'   "EJ"                      = "EJ/yr",
#'   "Billion_US_2018_Dollars" = "billion US$2018/yr",
#'   "US_2018_Dollars/GJ"      = "US$2018/GJ")
#'
