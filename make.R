library(targets)
library(tarchetypes)

tar_make()

tar_meta()
tar_visnetwork()

tar_load(c(riveratlas_shp_files, filtered_dataset))
tar_load(snapped_site_river, snapped_site_river)


tar_load(riveratlas_site)
riveratlas_site$siteid

source(file = "start_rmd.R")
river <- sf::read_sf(shp_file)
ti<-reduce(snapped_site_river[map_lgl(snapped_site_river, ~!all(is.na(.x)))], rbind)