library(targets)
library(tarchetypes)

tar_make()

tar_make_future(workers = min(future::availableCores() - 1, 24),
                names = !c(starts_with("beta_"), "ah_clust_tps", "pred_gaussian"))

tar_meta()
tar_visnetwork()

tar_make(names = bp_cl_dist)


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
