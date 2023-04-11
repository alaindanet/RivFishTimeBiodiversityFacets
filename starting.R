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

# Package installation
pck <- c("conflicted", "targets", "tarchetypes", "tidyverse", "magrittr",
  "lubridate", "here", "kableExtra", "scales", "rmarkdown", "sf",
  "rnaturalearth", "rnaturalearthdata", "terra", "cowplot", "viridis",
  "janitor", "codyn", "vegan", "slider", "rmarkdown", "future", "INLA",
  "inlatools", "glmmTMB", "easystats", "ggeffects", "tclust")

install.packages(
  pck,
  repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"),
  dep=TRUE
)


######################
#  Set data folder  #
######################
use_data_raw()

# Copy the address of the Lise shared drive
machine_login <- Sys.info()["login"]
if (machine_login == "alain") {
  server_mounted_location <- "/run/user/1000/gvfs/smb-share:server=caslab.ad.ilstu.edu,share=bio/Comte/"
} else if (machine_login == "ahdanet") {
  server_mounted_location <- "L:/"
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
# Figure papers
use_rmd("xxb-conceptual-figures")

# Lise
use_rmd("lise")
usethis::use_git_ignore("doc/lise*")
###################

use_rmd("inla_play")

dir_lise <- paste0(server_mounted_location, "alain/RivFishTimeBiodiversityFacets/for_lise/")
files <- list.files(dir_lise)[stringr::str_detect(list.files(dir_lise), "Comments")]
# Lise comments on invasive data
links <- here("inst", "extdata", files)
targets <- paste0(dir_lise, files)
for (i in seq_along(files)) {
  R.utils::createLink(
    link = links[i],
    target = targets[i],
    method = ifelse(machine_login == "ahdanet", "windows-shortcut", "unix-symlink"),
    overwrite = TRUE
  )
}

######################
#  USGS exotic data  #
######################
# Download water temperature data
exo_url <- "https://nas.er.usgs.gov/ipt/archive.do?r=nas&v=1.236"
destfile_exo <- "L:/ENV_LAYERS/USGSNonindigenousAquaticSpeciesdatabase.zip"
download.file(
  url = exo_url,
  destfile = destfile_exo,
  method = "auto",
  quiet = FALSE,
  mode = "wb",
  cacheOK = TRUE
)
unzip(destfile_exo, exdir = sub(".zip", "", destfile_exo))

R.utils::createLink(
  link = here("inst", "extdata", "USGSNonindigenousAquaticSpeciesdatabase"),
  target = paste0(server_mounted_location, "ENV_LAYERS/USGSNonindigenousAquaticSpeciesdatabase"),
  method = ifelse(machine_login == "ahdanet", "windows-shortcut", "unix-symlink"),
  overwrite = TRUE
)

# For katie
tar_load(at_mv_avg_roll)

ti <- at_mv_avg_roll
write_csv(ti, "~/Documents/post-these/isu/sYNGEO_Func_Sync_V2/input_data/Env/mv_avg_roll_air_tmp.csv")

######################
#  Updated database  #
######################

updated_database_file <- paste0(server_mounted_location, "RivFishTIME/updatedVersion/GlobalTimeSeries_time_series_updated_02120222.csv")

R.utils::createLink(
  link = here("inst", "extdata", "GlobalTimeSeries_time_series_updated_02120222.csv"),
  target = updated_database_file,
  method = ifelse(machine_login == "ahdanet", "windows-shortcut", "unix-symlink"),
  overwrite = TRUE
)

use_rmd(target_name = "ac-check-rivfishtime-update")

usethis::use_git_ignore("doc/map_*")

###########################
#  Get link to basin shp  #
###########################

basin_atlas_folder <- paste0(server_mounted_location, "ENV_LAYERS/basin_atlas_v10_shp/BasinATLAS_v10_shp")

R.utils::createLink(
  link = here("inst", "extdata", "BasinATLAS_v10_shp"),
  target = basin_atlas_folder,
  method = ifelse(
    machine_login == "ahdanet",
    "windows-shortcut", "unix-symlink"
    ),
  overwrite = TRUE
)

########################
#  Create a directory  #
########################

dir.create("paper")
system(paste0("touch ", here("paper", "outline_wordstack.txt")))
system(paste0("touch ", here("paper", "TODO")))
system(paste0("touch ", here("paper", "story_summary.txt")))

system(paste0("touch ", here("talk", "jasm2022.Rmd")))

######################
#  Convert to PDF  #
######################

remotes::install_github(
  "jhelvy/xaringanBuilder",
  dependencies = TRUE,
  force = TRUE
)
library(xaringanBuilder)
# For xaringan builder
#https://github.com/jhelvy/xaringanBuilder
Sys.setenv(PAGEDOWN_CHROME = "/usr/bin/chromium")
Sys.setenv(CHROMOTE_CHROME = "/usr/bin/chromium")
build_pdf(
  input = here::here("talk/jasm2022.html"),
  output_file = here::here("talk/jasm2022_test2.pdf"),
  complex_slides = TRUE,
  partial_slides = TRUE,
  delay = 1,
  keep_intermediates = TRUE
)

########################################
#  Compute coef of changes by decades  #
########################################
use_rmd("global-rate-changes")

##############################################
#  Extraction of old rivfishtime for Julian  #
##############################################
tar_load(old_timeseries)
origin_old_timeseries <- old_timeseries %>%
  distinct(siteid, origin)
save(origin_old_timeseries, file = here::here("data", "origin_old_timeseries.rda"))

load(here::here("data", "origin_old_timeseries.rda"))
load(here::here("data", "filtered_us_rivfishtime.rda"))

ti <- filtered_us_rivfishtime$location %>%
  dplyr::left_join(origin_old_timeseries, by = "siteid")
stopifnot(all(!is.na(ti$origin)))
# all good
filtered_us_rivfishtime_origin <- filtered_us_rivfishtime
filtered_us_rivfishtime_origin$location <- filtered_us_rivfishtime_origin$location %>%
  dplyr::left_join(origin_old_timeseries, by = "siteid")
save(filtered_us_rivfishtime_origin, file = here::here("data", "filtered_us_rivfishtime_origin.rda"))

##################################
#  Importing Sweden liming data  #
##################################

liming_site_folder <- paste0(
  server_mounted_location,
  "ENV_LAYERS/wetransfer_gis-data-liming-sweden_2022-05-12_2134/lst.LST_nkdb_kalkningsobjekt")

R.utils::createLink(
  link = here("inst", "extdata", "liming_data_site_sweden"),
  target = liming_site_folder,
  method = ifelse(
    machine_login == "ahdanet",
    "windows-shortcut", "unix-symlink"
    ),
  overwrite = TRUE
)

liming_stream_folder <- paste0(
  server_mounted_location,
  "ENV_LAYERS/wetransfer_gis-data-liming-sweden_2022-05-12_2134/lst.LST_nkdb_malomraden_vattendrag")

R.utils::createLink(
  link = here("inst", "extdata", "liming_data_stream_sweden"),
  target = liming_stream_folder,
  method = ifelse(
    machine_login == "ahdanet",
    "windows-shortcut", "unix-symlink"
    ),
  overwrite = TRUE
)

###################################
#  Get all lime data as shp file  #
###################################
tar_load(c(lime_data_site, lime_data_stream, filtered_dataset_modelling))
swe <- filtered_dataset_modelling$location %>%
  filter(country == "SWE")
loc <- swe %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 3006)
dir.create(here::here("data", "sweden"))
st_write(loc, here::here("data", "sweden", "sweden_site_rivfishtime.shp"))
st_write(lime_data_site, here::here("data", "sweden", "liming_data_site.shp"))
st_write(lime_data_stream,
  here::here("data", "sweden", "liming_data_stream.shp"))

R.utils::createLink(
  link = here("inst", "extdata", "liming_data_site_sweden"),
  target = paste0(server_mounted_location, "ENV_LAYERS/futurestreams"),
  method = ifelse(machine_login == "ahdanet", "windows-shortcut", "unix-symlink"),
  overwrite = TRUE
)


#########################
#  Basic stats helpers  #
#########################
use_r("basic_stat")

#######################
#  Outline wordstack  #
#######################
use_rmd("outline_wordstack")

dir.create(here("paper", "review"))

######################
#  Set up trackdown  #
######################

library(trackdown)
upload_file(
  file = here::here("paper/methods.Rmd"),
  gfile = NULL,
  gpath = "trackdown/biodiv_trends_anthropogenic_pressures",
  shared_drive = NULL,
  hide_code = TRUE,
  path_output = here::here("paper/methods.pdf"),
  rich_text = TRUE,
  rich_text_par = NULL,
  force = FALSE
)

update_file(
  file = here::here("paper/methods.Rmd"),
  gfile = NULL,
  gpath = "trackdown/biodiv_trends_anthropogenic_pressures",
  shared_drive = NULL,
  hide_code = TRUE,
  path_output = here::here("paper/methods.pdf"),
  rich_text = TRUE,
  rich_text_par = NULL,
  force = FALSE
)

ti <- c("story_summary", "outline_wordstack", "figures",
  "supplementary_figures", "methods")
for (i in seq_along(ti)) {
  upload_file(
    file = here::here(paste0("paper/", ti[i], ".Rmd")),
    gfile = NULL,
    gpath = "trackdown/biodiv_trends_anthropogenic_pressures",
    shared_drive = NULL,
    hide_code = TRUE,
    path_output = here::here(paste0("paper/", ti[i], ".pdf")),
    rich_text = TRUE,
    rich_text_par = NULL,
    force = FALSE
  )
}

ti <- c("figures", "supplementary_figures", "methods", "main_text")
for (i in seq_along(ti)) {
  update_file(
    file = here::here(paste0("paper/", ti[i], ".Rmd")),
    gfile = NULL,
    gpath = "trackdown/biodiv_trends_anthropogenic_pressures",
    shared_drive = NULL,
    hide_code = TRUE,
    path_output = here::here(paste0("paper/", ti[i], ".pdf")),
    rich_text = TRUE,
    rich_text_par = NULL,
    force = FALSE
  )
}

######################
#  Link folder riv  #
######################

rivfishtime_folder <- paste0(server_mounted_location, "RivFishTIME")

R.utils::createLink(
  link = here("inst", "extdata", "RivFishTIME"),
  target = rivfishtime_folder,
  method = ifelse(
    machine_login == "ahdanet",
    "windows-shortcut", "unix-symlink"
    ),
  overwrite = TRUE
)

######################
#  main text trackdown  #
######################

library(trackdown)
upload_file(
  file = here::here("paper/main_text.Rmd"),
  gfile = NULL,
  gpath = "trackdown/biodiv_trends_anthropogenic_pressures",
  shared_drive = NULL,
  hide_code = TRUE,
  path_output = here::here("paper/main_text.pdf"),
  rich_text = TRUE,
  rich_text_par = NULL,
  force = FALSE
)

update_file(
  file = here::here("paper/figures.Rmd"),
  gfile = NULL,
  gpath = "trackdown/biodiv_trends_anthropogenic_pressures",
  shared_drive = NULL,
  hide_code = TRUE,
  path_output = here::here("paper/figures.pdf"),
  rich_text = TRUE,
  rich_text_par = NULL,
  force = FALSE
)

##########################
#  Coverage rivfishtime  #
##########################

use_rmd("coverage_rivfishtime")

########################################
#  New waterflow and temperature data  #
########################################

hist_date <- c("1979-01-07_to_1985-12-30", "1986-01-07_to_1995-12-30", "1996-01-07_to_2005-12-30")
# Download water temperature data
water_temp_url <- paste0(
  "https://geo.public.data.uu.nl/vault-futurestreams/research-futurestreams%5B1633685642%5D/original/waterTemp/hist/E2O/waterTemp_weekAvg_output_E2O_hist_",
  hist_date,
  ".nc"
)
dir.create("L:/ENV_LAYERS/futurestreams")
destfile_wt <- paste0(
  "L:/ENV_LAYERS/futurestreams/waterTemp_weekAvg_output_E2O_hist_",
  hist_date, ".nc"
  )
for (i in seq_along(hist_date)) {
  download.file(
    url = water_temp_url,
    destfile = destfile_wt,
    method = "auto",
    quiet = FALSE,
    mode = "wb",
    cacheOK = TRUE
  )
}

R.utils::createLink(
  link = here("inst", "extdata", "futurestreams"),
  target = paste0(server_mounted_location, "ENV_LAYERS/futurestreams"),
  method = ifelse(machine_login == "ahdanet", "windows-shortcut", "unix-symlink"),
  overwrite = TRUE
)
