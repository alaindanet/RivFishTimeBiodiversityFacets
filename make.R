library(targets)
library(tarchetypes)

tar_make()

tar_meta()
tar_visnetwork()

tar_load(c(filtered_dataset, measurement))

tar_load(c(site_desc_loc, filtered_op_protocol, op_protocol))


source("packages.R")
source(here::here("R", "misc.R"))
source_dir(here("R"))

eu_shp <- here("inst", "extdata", "RiverATLAS_v10_shp") %>%
  list.files(., full.names = TRUE) %>%
  .[stringr::str_detect(., "eu.shp")]
sf::st_layers(eu_shp , do_count = TRUE)
sf::read_sf(eu_shp, query = "SELECT Count(FID) AS number_of_rows FROM RiverATLAS_v10_eu")

tictoc::tic()
riveratlas_slice <- sf::read_sf(eu_shp, query = "SELECT * FROM RiverATLAS_v10_eu WHERE FID = 1")
tictoc::toc()
#313.564 sec elapsed

col_riveratlas <- colnames(riveratlas_slice)
stream_charac <- col_riveratlas[str_detect(col_riveratlas, "[A-Z]")]

# environmental variable begins by three character
str_sub(col_riveratlas[!col_riveratlas %in% stream_charac], 1, 3)

tictoc::tic()
test <- sf::read_sf(eu_shp, query = "SELECT MAIN_RIV FROM RiverATLAS_v10_eu")
tictoc::toc()
#188.71 sec elapsed

tar_target()
loc <- get_site_location_as_sf()
st_bbox(loc)
st_crs(loc) == st_crs(test)
ti <- st_crop(test,
  xmin =  st_bbox(loc)["xmin"],
  xmax = st_bbox(loc)["xmax"],
  ymin = st_bbox(loc)["ymin"],
  ymax = st_bbox(loc)["ymax"]
)
