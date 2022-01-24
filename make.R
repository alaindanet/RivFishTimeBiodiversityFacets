library(targets)
library(tarchetypes)

tar_make()

tar_meta()
tar_visnetwork()

tar_load(c(filtered_dataset, measurement))

tar_load(c(site_desc_loc, filtered_op_protocol, op_protocol))

filtered_dataset$location %>%
  filter(country == "AUS")

box = c(xmin = 0, ymin = 0, xmax = 1, ymax = 1)
pol = st_sfc(st_buffer(st_point(c(.5, .5)), .6))
pol2 = st_sfc(st_buffer(st_point(c(.5, .5)), .3))
pol_sf = st_sf(a=1, geom=pol)
st_buffer(pol_sf, 10)
plot(pol)
plot(st_crop(pol, box), add = TRUE)
plot(pol2, col = "red", add = TRUE)
plot(st_crop(pol, st_bbox(pol2)), col = "blue", add = TRUE)


