
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

r2_sb <- function(
  epsilon_full = NULL,
  epsilon_null = NULL,
  std_intercept_null = NULL,
  std_intercept_full = NULL
) {

  eps_var <- c(full = epsilon_full^2, null = epsilon_null^2)
  var_interp_null <- sum(map_dbl(std_intercept_null, ~.x^2))
  var_interp_full <- sum(map_dbl(std_intercept_full, ~.x^2))

  1 - ( (eps_var["full"] - var_interp_full) / (eps_var["null"] + var_interp_full) )

}

r2_mvp <- function(
  var_pred = NULL,
  epsilon = NULL,
  std_intercept = NULL,
  type = "all"
  ) {

  var_interp <- sum(map_dbl(std_intercept, ~.x^2))

  out <- list(
    marginal = var_pred / (var_pred + var_interp + epsilon^2),
    conditional = (var_pred  + var_interp) / (var_pred + var_interp + epsilon^2)
    )

  if (type == "all") {
    return(out)
  } else if (type == "marginal") {
    return(out[["marginal"]])
  } else if (type == "conditional") {
    return(out[["conditional"]])
  }

}
