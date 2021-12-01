library("devtools")
library("here")
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
#zaj#####################
use_data_raw()

# Copy the address of the Lise shared drive
server_mounted_location <- "/run/user/1000/gvfs/smb-share:server=caslab.ad.ilstu.edu,share=bio/Comte/" 

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

