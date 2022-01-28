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
tar_load(names = c(water_temperature_file, filtered_dataset))
debug(extract_water_temperature_values)
extract_water_temperature_values(raster_path = water_temperature_file,
                                 site = filtered_dataset$location %>%
                                   st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
                                   )

