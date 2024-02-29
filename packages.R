# packages.R

# targets packages
library(targets)
library(tarchetypes)

# general code
library(here)
library(tidyverse)

library(conflicted)
conflict_prefer("filter", "dplyr")

# figure-specific
library(patchwork) # combining figures
library(ggh4x) # setting facet-specific axes with facetted_pos_scales()
library(ggrepel) # ggplot label repel for better spacing

