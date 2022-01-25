library(targets)
library(tarchetypes)

tar_make()

tar_meta()
tar_visnetwork()
tar_load(c(riveratlas_shp_files, filtered_dataset))

tar_load(snapped_site_river)
reduce(snapped_site_river, rbind)

snapped_site_river[[1]]$riverid[!is.na(snapped_site_river[[1]]$riverid)]
