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
