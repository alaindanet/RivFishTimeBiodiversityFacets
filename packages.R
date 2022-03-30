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
sf_use_s2(use_s2 = FALSE)
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

# date
library(slider)

library(rmarkdown)

# parallel
library(future)

# Statistics
library(INLA)
library(inlatools)
library(glmmTMB)

library(easystats)
library(ggeffects)

# cluster
library(tclust)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

# For brms
#conflict_prefer("chisq.test", "stats")
#conflict_prefer("fisher.test", "stats")
conflict_prefer("col_factor", "scales")
conflict_prefer("ar", "brms")
conflict_prefer("col_factor", "scales")
conflict_prefer("discard", "scales")
conflict_prefer("extract", "tidyr")
conflict_prefer("group_rows", "dplyr")
conflict_prefer("lag", "dplyr")
#conflict_prefer("inset", "magrittr")
#conflict_prefer("rescale", "scales")
#conflict_prefer("run", "future")
#conflict_prefer("simplify", "purrr")
#conflict_prefer("src", "dplyr")
#conflict_prefer("stamp", "lubridate")

