#' Make SPDE object for INLA or spaMM
make_spde <- function(loc = filtered_dataset$location) {
  coords_loc <- cbind(loc$longitude, loc$latitude)
  
  mesh <- inla.mesh.2d(
    loc = coords_loc, max.edge = c(1, 15),
    cutoff = 0.01
  )
  spde_loc <- inla.spde2.matern(mesh = mesh, alpha = 2, constr = TRUE)
  # Build index
  indexs_loc <- inla.spde.make.index("s", spde_loc$n.spde)
  # Project gaussian random field to vertices
  A <- inla.spde.make.A(mesh = mesh, loc = coords_loc)
  
  return(A)
}

get_hpdmarginal_inla <- function(
  inla_mod = NULL,
  type = "fixed",
  p = c(.80, .90, 0.95)) {

  if (type == "fixed") {
    m <- inla_mod$marginals.fixed
    mi <- inla_mod$summary.fixed
  } else if (type == "rand") {
    m <- inla_mod$marginals.hyperpar
    mi <- inla_mod$summary.hyperpar
  }
  output <- map_dfr(m,
    ~inla.hpdmarginal(marginal = .x, p = c(.80, .90, 0.95)) %>%
    as.data.frame() %>%
    rownames_to_column("ci_level"),
  .id = "term"
  )

  ## Add mean
  output <- output %>%
    left_join(
      mi%>%
        rownames_to_column("term") %>%
        as_tibble %>%
        select(term, mean),
      by = "term"
    )
 

  if (type == "rand") {
    output[c("low", "high")] <- map(output[c("low", "high")], tau_to_sigma)
  }

  return(output)
}

plot_uniform_quantile_inla <- function(mod_inla = NULL) {
  pit <- sort(ne$cpo$pit)

  tb <- tibble(
    pit = pit,
    uniquant = (seq_along(pit)) / (length(pit)+1)
  )

  tb %>%
  ggplot(aes(x = uniquant, y = pit)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0) +
    labs(x = "uniform quantiles", y = "Sorted PIT values")
}

tau_to_sigma <- function(x) {
  1 / sqrt(x)
}

sigma_to_tau <- function(x) {
  1 / (x^2)
}

get_formula_inla_no_tps <- function(resp = NULL, drivers = TRUE, tau_prior = NULL) {

  fixed_min <- "log1_year_nb"

  if (drivers) {

    fixed_part <- paste0(fixed_min, " + ",
      "riv_str_rc1 + hft_ix_c93 + hft_ix_c9309_log2_ratio +
      log1_year_nb : riv_str_rc1 +
      log1_year_nb : hft_ix_c93 +
      log1_year_nb : hft_ix_c9309_log2_ratio +
      log1_year_nb : riv_str_rc1 : hft_ix_c93 +
      log1_year_nb : riv_str_rc1 : hft_ix_c9309_log2_ratio +
      log1_year_nb : hft_ix_c93 : hft_ix_c9309_log2_ratio")
  } else {
    fixed_part <- fixed_min
  }

  if (!is.null(tau_prior)) {
    rand_part <-
      paste0(
        "f(intercept_main_bas, model = 'iid', hyper = ", tau_prior, ") +
        f(main_bas1, log1_year_nb, model = 'iid', hyper = ", tau_prior, ") +
        f(intercept_main_bassiteid, model = 'iid', hyper = ", tau_prior, ") +
        f(siteid1, log1_year_nb, model = 'iid', hyper = ", tau_prior, ")"
      )
  } else {
    rand_part <-
      paste0(
        "f(intercept_main_bas, model = 'iid') +
        f(main_bas1, log1_year_nb, model = 'iid') +
        f(intercept_main_bassiteid, model = 'iid') +
        f(siteid1, log1_year_nb, model = 'iid')")
  }

  form <- paste0(resp, " ~\n", fixed_part, " +\n", rand_part)
  as.formula(form)
}

get_formula_inla_abun <- function(resp = NULL, drivers = TRUE, tau_prior = NULL) {

  fixed_min <- "log1_year_nb + unitabundance + log1_year_nb : unitabundance "

  if (drivers) {

    fixed_part <- paste0(fixed_min, " + ",
      "riv_str_rc1 + hft_ix_c93 + hft_ix_c9309_log2_ratio +
    log1_year_nb : riv_str_rc1 +
    log1_year_nb : hft_ix_c93 +
    log1_year_nb : hft_ix_c9309_log2_ratio +
    log1_year_nb : riv_str_rc1 : hft_ix_c93 +
    log1_year_nb : riv_str_rc1 : hft_ix_c9309_log2_ratio +
    log1_year_nb : hft_ix_c93 : hft_ix_c9309_log2_ratio")

  } else {
    fixed_part <- fixed_min
  }

  if (!is.null(tau_prior)) {
    rand_part <-
      paste0(
        "f(intercept_main_bas, model = 'iid', hyper = ", tau_prior, ") +
        f(main_bas1, log1_year_nb, model = 'iid', hyper = ", tau_prior, ") +
        f(intercept_main_bassiteid, model = 'iid', hyper = ", tau_prior, ") +
        f(siteid1, log1_year_nb, model = 'iid', hyper = ", tau_prior, ")"
      )
  } else {
    rand_part <-
      paste0(
        "f(intercept_main_bas, model = 'iid') +
        f(main_bas1, log1_year_nb, model = 'iid') +
        f(intercept_main_bassiteid, model = 'iid') +
        f(siteid1, log1_year_nb, model = 'iid')")
  }

  form <- paste0(resp, " ~\n", fixed_part, " +\n", rand_part)
  as.formula(form)

}

get_formula_inla_tps <- function(resp = NULL, drivers = TRUE, tau_prior = NULL) {

  fixed_min <- "0 +
    log1_year_nb"

  if (drivers) {

    fixed_part <- paste0(fixed_min, " + ",
      " log1_year_nb : riv_str_rc1 +
      log1_year_nb : hft_ix_c93 +
      log1_year_nb : hft_ix_c9309_log2_ratio +
      log1_year_nb : riv_str_rc1 : hft_ix_c93 +
      log1_year_nb : riv_str_rc1 : hft_ix_c9309_log2_ratio +
      log1_year_nb : hft_ix_c93 : hft_ix_c9309_log2_ratio")

  } else {
    fixed_part <- fixed_min
  }

  if (!is.null(tau_prior)) {
    rand_part <-
      paste0(
        "f(intercept_main_bas, model = 'iid', hyper = ", tau_prior, ") +
        f(main_bas1, log1_year_nb, model = 'iid', hyper = ", tau_prior, ") +
        f(intercept_main_bassiteid, model = 'iid', hyper = ", tau_prior, ") +
        f(siteid1, log1_year_nb, model = 'iid', hyper = ", tau_prior, ")"
      )
  } else {
    rand_part <-
      paste0(
        "f(intercept_main_bas, model = 'iid') +
        f(main_bas1, log1_year_nb, model = 'iid') +
        f(intercept_main_bassiteid, model = 'iid') +
        f(siteid1, log1_year_nb, model = 'iid')")
  }

  form <- paste0(resp, " ~\n", fixed_part, " +\n", rand_part)
  as.formula(form)

  form <- paste0(resp,
    "~
    0 +
    log1_year_nb +
    log1_year_nb : riv_str_rc1 +
    log1_year_nb : hft_ix_c93 +
    log1_year_nb : hft_ix_c9309_log2_ratio +
    log1_year_nb : riv_str_rc1 : hft_ix_c93 +
    log1_year_nb : riv_str_rc1 : hft_ix_c9309_log2_ratio +
    log1_year_nb : hft_ix_c93 : hft_ix_c9309_log2_ratio +
    f(main_bas, log1_year_nb, model = 'iid') +
    f(siteid:main_bas, log1_year_nb, model = 'iid')"
  )
  as.formula(form)
}

fun_int_env_formula_inla <- function(x = NULL, drivers = TRUE, tau_prior = NULL) {
  tar_load(c(tps_var, log_rich_var))

  if (x %in% tps_var) {
    return(get_formula_inla_tps(resp = x, drivers = drivers, tau_prior = tau_prior))
  } else if (x %in% log_rich_var) {
    return(get_formula_inla_no_tps(resp = x, drivers = drivers, tau_prior = tau_prior))
  } else if (x == "log_total_abundance") {
    return(get_formula_inla_abun(resp = x, drivers = drivers, tau_prior = tau_prior))
  } else {
    stop("no defined variables")
  }
}

get_random_effect_inla <- function(
  inla_mod = NULL,
  effect = NULL,
  exponentiate = FALSE) {

  output <- inla_mod$summary.random[[effect]] %>%
    as_tibble() %>%
    rename(
      quant0.025 = `0.025quant`,
      quant0.975 = `0.975quant`,
      quant0.5 = `0.5quant`
    ) %>% 
    select(-kld)

  if (exponentiate) {
    output <- output %>%
      mutate(across(where(is.double), ~exp(. - 1)))
  }

  if (str_detect(effect, "siteid:main_bas")) {
    colnames(output)[colnames(output) == "ID"] <- "siteid"
  } else if (str_detect(effect, "main_bas")) {
    colnames(output)[colnames(output) == "ID"] <- "main_bas"
  }
  return(output)
}

HC.prior  = "expression:
  sigma = exp(-theta/2);
  gamma = 25;
  log_dens = log(2) - log(pi) - log(gamma);
  log_dens = log_dens - log(1 + (sigma / gamma)^2);
  log_dens = log_dens - log(2) - theta / 2;
  return(log_dens);
"
