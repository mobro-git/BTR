# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) 

source("packages.R")

# Set target options:
tar_option_set(
  packages = c("dplyr","readr","tidyverse") # packages to make available to targets
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

