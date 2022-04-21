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
  output <- map_dfr(m, function(x) {
    if (!any(x == "Inf")) {

     inla.hpdmarginal(marginal = x, p = c(.80, .90, 0.95)) %>%
    as.data.frame() %>%
    rownames_to_column("ci_level")
    } else {
      tibble(
        ci_level = c("level:0.80", "level:0.90", "level:0.95"),
        low = NA,
        high = NA)
    }

  }
,
  .id = "term"
  )

  ## Add mean
  output <- output %>%
    left_join(
      mi %>%
        rownames_to_column("term") %>%
        as_tibble %>%
        select(term, mean),
      by = "term"
    )
 

  if (type == "rand") {
    output[c("low", "mean", "high")] <- map(output[c("low", "mean", "high")], tau_to_sigma)
  }

  return(output)
}

plot_uniform_quantile_inla <- function(mod_inla = NULL) {
  pit <- sort(mod_inla$cpo$pit)

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

}

fun_int_env_formula_inla <- function(x = NULL, drivers = TRUE, tau_prior = NULL) {
  tar_load(c(tps_var, log_rich_var))

  if (x %in% tps_var) {
    return(get_formula_inla_tps(resp = x, drivers = drivers, tau_prior = tau_prior))
  } else if (x %in% c(log_rich_var, "perc_exo_sp", "perc_exo_abun")) {
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

get_re_prediction_inla <- function(
  inla_mod = NULL,
  effect = "siteid1",
  trend_class = TRUE,
  exponentiate = FALSE,
  modelling_data = NULL) {

  re <- inla_mod$summary.random[[effect]] %>%
    as_tibble() %>%
    rename(
      quant0.025 = `0.025quant`,
      quant0.975 = `0.975quant`,
      quant0.5 = `0.5quant`
    ) %>% 
    select(-kld)

  re_name <- str_extract(effect, "siteid|main_bas")
  colnames(re)[colnames(re) == "ID"] <- re_name
    
  
  # cannot correct basin abundance estimation totally because different unit
  if (any(str_detect(inla_mod$names.fixed, "unitabundance")) & re_name == "siteid") {
    
    unit_abun_coef <- get_hpdmarginal_inla(inla_mod) %>%
      filter(str_detect(term, "log1_year_nb:unitabundance")) %>%
      mutate(term = str_replace(term, "log1_year_nb:unitabundance", "")) %>%
      distinct(term, mean) %>%
      rename(unitabundance = term, int_log1_year_nb = mean)
    
    re_unit <- modelling_data[, c(siteid, "unitabundance")] %>%
      distinct(siteid, unitabundance)
    
    re <- re %>%
      left_join(re_unit, by = "siteid") %>%
      left_join(unit_abun_coef, by = "unitabundance") %>%
      mutate_at(c("mean", "quant0.025", "quant0.5", "quant0.975", "mode"), ~.x + int_log1_year_nb) %>%
      select(-unitabundance, int_log1_year_nb)
  }
  

  if(trend_class) {
    re <- re %>%
      mutate(
        trend_class = case_when(
          quant0.025 > 0 & quant0.975 > 0 ~ "increase",
          quant0.025 < 0 & quant0.975 < 0 ~ "decrease",
          sign(quant0.025) * sign(quant0.975) == -1 ~ "stable",
          TRUE ~ "NA"
        )
      )

  }


  if (exponentiate) {
    re <- re %>%
      mutate(across(where(is.double), ~exp(. - 1)))
  }

  return(re)
}

target_inla_re_pred <- function(
    mod_list = NULL,
    modelling_data = NULL,
    effect = "siteid1",
    trend_class = TRUE,
    exponentiate_log_resp = TRUE) {
  
  mod_list %>%
    mutate(random_site = map2(mod, response,
      get_re_prediction_inla(inla_mod = .x,
        modelling_data = modelling_data,
        effect = effect,
        trend_class = trend_class,
        exponentiate = ifelse(
          str_detect(.y, "log_") & exponentiate_log_resp,
          TRUE,
          FALSE) 
          ))) %>%
    select(-mod)
}

HC.prior  = "expression:
  sigma = exp(-theta/2);
  gamma = 25;
  log_dens = log(2) - log(pi) - log(gamma);
  log_dens = log_dens - log(1 + (sigma / gamma)^2);
  log_dens = log_dens - log(2) - theta / 2;
  return(log_dens);
"
