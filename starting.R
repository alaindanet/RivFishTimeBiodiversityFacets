library("devtools")
library(magrittr)
library("here")
library(tflow)
options(devtools.name = "Alain Danet",
  devtools.desc.author = "person('Alain', 'Danet',
  email='alain.danet@ilstu.edu', role = c('aut', 'cre'))",
  devtools.desc.license = "MIT + file LICENSE"
)

devtools::create(".")
use_testthat()
use_vignette("intro")
use_travis()
use_package_doc()
use_cran_comments()
use_readme_rmd()
use_mit_license()

# Routine
attachment::att_amend_desc()
devtools::check(vignettes = FALSE)



######################
#  Set data folder  #
######################
use_data_raw()

# Copy the address of the Lise shared drive
machine_login <- Sys.info()["login"]
if (machine_login == "alain") {
  server_mounted_location <- "/run/user/1000/gvfs/smb-share:server=caslab.ad.ilstu.edu,share=bio/Comte/" 
} else if (machine_login == "ahdanet") {
  server_mounted_location <- "L://"
}

# Local storage dir:
dir.create(here("inst", "extdata"), recursive = TRUE)

# Create a symbolic link toward RivFishTime data here
# So I do not have to copy them
characteristics <- here("inst",
    "extdata",
    "GlobalTimeSeries_characteristics_1232021.csv")

R.utils::createLink(
  link = characteristics,
  target = paste0(
    server_mounted_location,
    "RivFishTIME/house_version/GlobalTimeSeries_characteristics_1232021.csv"
    ),
  overwrite = TRUE)

# Test
readr::read_csv2(characteristics)

# Same
timeseries <- here("inst",
    "extdata",
    "GlobalTimeSeries_database_1232021.csv")

R.utils::createLink(
  link = timeseries,
  target = paste0(
    server_mounted_location,
    "RivFishTIME/house_version/GlobalTimeSeries_database_1232021.csv"
    ),
  overwrite = TRUE)

readr::read_lines_raw(timeseries)


use_git_ignore("inst/extdata", directory = ".")

# For the shell

# git add remote origin git@github.com:alaindanet/RivFishTimeBiodiversityFacets.git

# functions that return variable names
use_r(name = "variable_shortcut")

#
use_r(name = "summary_distribution")

library(tflow)
use_rmd("ab-raw-data")
use_rmd("ac-data-filtering")
use_rmd("ad-temporal-trends")
use_rmd("ae-modelling-temporal-trends")
use_rmd("aca-community-structure")

use_test("turnover")
use_r("neutral_model")
use_rmd("af-explain-high-turnover")
use_rmd("xx-meeting-report")

# download river data

riveratlas_shp_url <- "https://figshare.com/ndownloader/files/20087237"
destfile_riveratlas <- "L:/ENV_LAYERS/river_atlas_v10_shp.zip"
download.file(
  url = riveratlas_shp_url,
  destfile = destfile_riveratlas,
  method="auto", 
  quiet = FALSE,
  mode = "wb",
  cacheOK = TRUE
)
unzip(destfile_riveratlas, exdir = sub(".zip", "", destfile_riveratlas))

# Download water temperature data

water_temperature_url <- "https://zenodo.org/record/1468408/files/waterTemperature_Global_monthly_1979-2014.nc?download=1"
destfile_water_temperature <- "L:/ENV_LAYERS/waterTemperature_Global_monthly_1979-2014.nc"
download.file(
  url = water_temperature_url,
  destfile = destfile_water_temperature,
  method = "auto",
  quiet = FALSE,
  mode = "wb",
  cacheOK = TRUE
)
unzip(destfile_riveratlas, exdir = sub(".zip", "", destfile_riveratlas))

## Create symbolic link
files <- c("RiverATLAS_v10_shp", "waterTemperature_Global_monthly_1979-2014.nc")

links <- paste0(here("inst", "extdata", files))
#targets <- paste0(server_mounted_location, "ENV_LAYERS/", c(files))
#for windows
targets <- paste0("L:/", "ENV_LAYERS/", c(files))

for (i in seq_along(files)) {
  R.utils::createLink(
    link = links[i],
    target = targets[i],
    overwrite = TRUE
  )
}


eu_shp <- here("inst", "extdata", "RiverATLAS_v10_shp.lnk") %>%
  R.utils::filePath(., expandLinks = "any") %>%
  list.files(., full.names = TRUE) %>%
  .[stringr::str_detect(., "eu.shp")]
sf::st_layers(eu_shp, do_count = TRUE)



use_r("spatial_match")
use_r("format_env_data")
use_r("spamm_helper")

# DL Chelsa data
source(here("R", "load_raw_data.R"))
download_chelsa(overwrite = FALSE)

use_r("glmmTMB_models")
use_r("pca_methods")
use_r("glmmTMB_helpers")
use_rmd("xx-meeting-report")
use_rmd("ag-biodiversity-facets-support")
use_r("plot_model")
use_rmd("ah-clust-tps")
use_r("clustering_methods")

# Template for scientific paper
install.packages("distill")

use_r("plot_paper")


# Create symbolic links for tedesco data

## Create symbolic link
files <- c("Tedesco_2017", "Script_NativeStatusTedesco.R")


links <- here("inst", "extdata", files)
targets <- paste0(server_mounted_location, "ENV_LAYERS/", c(files))
#for windows
targets <- paste0("L:/", "ENV_LAYERS/", c(files))
links <- paste0("L:/", "alain/RivFishTimeBiodiversityFacets/inst/extdata/", c(files))

for (i in seq_along(files)) {
  R.utils::createLink(
    link = links[i],
    target = targets[i],
    method = ifelse(machine_login == "ahdanet", "windows-shortcut", "unix-symlink"),
    overwrite = TRUE
  )
}
shell(sprintf('mklink "%s" "%s"', 
              normalizePath(links[1], mustWork = FALSE),
              normalizePath(targets[1])
))


# Tmp figure script
file.create(here("doc", "tmp_figures.R"))

# Conceptual figures 
use_rmd("xxa-conceptual-figures")

#
