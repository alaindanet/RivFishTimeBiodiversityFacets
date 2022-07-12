library(targets)
library(tarchetypes)

tar_make()

exclusion_vector <- c("chao_hillnb_cov80", "ah_clust_tps", "test_autocor_tmb",
             "biodiversity_facets_support",
             "pred_gaussian", "filtered_data_watch", "trends_report",
             "water_temperature", "raw_data_watch")

tar_make_future(
  workers = min(future::availableCores() - 1, 24),
  names = !c(starts_with("beta_"), !!exclusion_vector)
  )

tar_make_future(
  workers = min(future::availableCores() - 1, 8),
  names = !c(starts_with("beta_"), !!exclusion_vector)
  )

tar_meta()
tar_visnetwork()

tar_make(names = bib)

tar_make(names = r2)
tar_make(names = tab_waic)
tar_make(names = hft_total_summary)
tar_make(names = starts_with("pred_data"))
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
