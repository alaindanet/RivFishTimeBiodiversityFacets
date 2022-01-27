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
library("rnaturalearth")
library("rnaturalearthdata")
library(terra)

library(cowplot)
library(viridis)

#clean dataset
library(janitor)

# Turnover
library(codyn)
library(vegan)


conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

# For brms
conflict_prefer("col_factor", "scales")
conflict_prefer("ar", "brms")
conflict_prefer("col_factor", "scales")
conflict_prefer("discard", "scales")
conflict_prefer("extract", "tidyr")
conflict_prefer("group_rows", "dplyr")
conflict_prefer("lag", "dplyr")

library(rmarkdown)

# parallel
library(future)