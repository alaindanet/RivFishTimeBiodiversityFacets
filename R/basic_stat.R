
log_beta_to_perc_rate <- function (x) {
  (exp(x) - 1) * 100
}

compute_trends_meaningful_units <- function (
  x = NULL, resp = NULL, time = log(10 + 1)) {

  if (resp %in% c("log_total_abundance", "log_chao_richness")) {
    return(log_beta_to_perc_rate(x) * time)

  } else if (resp %in% c("jaccard_dis_scaled", "turnover_scaled",
      "nestedness_scaled", "hillebrand_dis_scaled", "appearance_scaled",
      "disappearance_scaled")) {
    return(x * time)
  } else if (resp %in% c("perc_exo_sp", "perc_exo_abun")) {
    return(x * time)
  } else {
    stop("undefined resp")
  }

}
