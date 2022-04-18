library(targets)
library(tarchetypes)

tar_make()

tar_make_future(
  workers = min(future::availableCores() - 1, 24),
  names = !c(starts_with("beta_"), "chao_hillnb_cov80", "ah_clust_tps",
             "pred_gaussian", "filtered_data_watch", "trends_report")
  )

tar_meta()
tar_visnetwork()

tar_make(names = meeting_report)
tar_make(names = filtered_dataset)
tar_make(names = starts_with("gaussian_inla_exo_prior"))
tar_make_future(names = at_mv_avg_roll, workers = future::availableCores() - 1)

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
