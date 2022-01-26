library(targets)
library(tarchetypes)

tar_make()

tar_meta()
tar_visnetwork()
tar_load(c(riveratlas_shp_files, filtered_dataset))

tar_load(snapped_site_river)
reduce(snapped_site_river, rbind)

snapped_site_river[[1]]$riverid[!is.na(snapped_site_river[[1]]$riverid)]

tar_load(riveratlas_site)

ti <- reduce(riveratlas_site, rbind)

source(file = "start_rmd.R")
target_extract_riveratlas_info(
  river_shp_files = map_chr(riveratlas_shp_files, ~get_full_file_name(filename = .x)),
  snap_list = snapped_site_river)
