library(targets)
library(tarchetypes)

tar_make()

tar_meta()
tar_visnetwork()
tar_load(c(riveratlas_shp_files, filtered_dataset))

debugonce(target_snap_site_to_river)
debugonce(st_snap_points)
debugonce(match_river_site)
target_snap_site_to_river(
  river_shp_filepath = riveratlas_shp_files[1],
  site_sf = filtered_dataset$location %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326),
  proj_crs = 4087,
  length_chunk = 200)

