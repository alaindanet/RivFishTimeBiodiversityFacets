library(targets)
library(tarchetypes)


tar_make()

future::plan(future::multisession, workers = min(future::availableCores() - 1, 10))
tar_make_future()

tar_meta()
tar_visnetwork()

tar_load(c(riveratlas_shp_files, filtered_dataset))
tar_load(snapped_site_river, snapped_site_river)


tar_load(c(riveratlas_site,slp_env))
riveratlas_site$siteid

source(file = "start_rmd.R")
tar_load(rigal_slp_df)

tar_load(inla_rich)
dist_check <- inlatools::fast_distribution_check(inla_rich)
plot(dist_check)
summary(inla_rich)

tar_load(inla_abun)
dist_check <- inlatools::distribution_check(inla_rich)
plot(dist_check)
summary(inla_rich)
