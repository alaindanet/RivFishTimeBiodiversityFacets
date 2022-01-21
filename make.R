library(targets)
library(tarchetypes)

tar_make()

tar_meta()
tar_visnetwork()

tar_load(c(filtered_dataset, measurement))

tar_load(c(site_desc_loc, filtered_op_protocol, op_protocol))

filtered_dataset$location %>%
  filter(country == "AUS")
