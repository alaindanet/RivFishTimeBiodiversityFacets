library(targets)
library(tarchetypes)


tar_make()

future::plan(future::multisession, workers = min(future::availableCores() - 1, 10))
tar_make_future()

tar_meta()
tar_visnetwork()

tar_load(c(riveratlas_shp_files, filtered_dataset))
tar_load(snapped_site_river, snapped_site_river)


tar_load(riveratlas_site)
riveratlas_site$siteid

source(file = "start_rmd.R")
tar_load(water_temperature_file)
