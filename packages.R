library(conflicted)

library(targets)
library(tarchetypes)
library(tidyverse)
library(magrittr)
library(lubridate)
library(here)
library(kableExtra)
library("scales")
library("rmarkdown")

library("sf")
library("mapview")
library("rnaturalearth")
library("rnaturalearthdata")

library(cowplot)

#clean dataset
library(janitor)

# Turnover
library(codyn)

conflict_prefer("filter", "dplyr")

# For brms
conflict_prefer("col_factor", "scales")
conflict_prefer("ar", "brms")
conflict_prefer("col_factor", "scales")
conflict_prefer("discard", "scales")
conflict_prefer("extract", "tidyr")
conflict_prefer("group_rows", "dplyr")
conflict_prefer("lag", "stats")

library(rmarkdown)
