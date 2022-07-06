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

get_formula_inla_no_tps_year_nb <- function(resp = NULL, drivers = TRUE, tau_prior = NULL) {

  fixed_min <- "year_nb"

  if (drivers) {

    fixed_part <- paste0(fixed_min, " + ",
      " year_nb : riv_str_rc1 +
      year_nb : hft_ix_c93 +
      year_nb : hft_ix_c9309_log2_ratio +
      year_nb : riv_str_rc1 : hft_ix_c93 +
      year_nb : riv_str_rc1 : hft_ix_c9309_log2_ratio +
      year_nb : hft_ix_c93 : hft_ix_c9309_log2_ratio")

  } else {
    fixed_part <- fixed_min
  }

  if (!is.null(tau_prior)) {
    rand_part <-
      paste0(
        "f(intercept_main_bas, model = 'iid', hyper = ", tau_prior, ") +
        f(main_bas1, year_nb, model = 'iid', hyper = ", tau_prior, ") +
        f(intercept_main_bassiteid, model = 'iid', hyper = ", tau_prior, ") +
        f(siteid1, year_nb, model = 'iid', hyper = ", tau_prior, ")"
      )
  } else {
    rand_part <-
      paste0(
        "f(intercept_main_bas, model = 'iid') +
        f(main_bas1, year_nb, model = 'iid') +
        f(intercept_main_bassiteid, model = 'iid') +
        f(siteid1, year_nb, model = 'iid')")
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
        f(siteid1, log1_year_nb, model = 'iid')"
      )
  }

  form <- paste0(resp, " ~\n", fixed_part, " +\n", rand_part)
  as.formula(form)

}

get_formula_inla_abun_year_nb <- function(resp = NULL, drivers = TRUE, tau_prior = NULL) {

  fixed_min <- "0 + year_nb + unitabundance + year_nb : unitabundance "

  if (drivers) {

    fixed_part <- paste0(fixed_min, " + ",
      " year_nb : riv_str_rc1 +
      year_nb : hft_ix_c93 +
      year_nb : hft_ix_c9309_log2_ratio +
      year_nb : riv_str_rc1 : hft_ix_c93 +
      year_nb : riv_str_rc1 : hft_ix_c9309_log2_ratio +
      year_nb : hft_ix_c93 : hft_ix_c9309_log2_ratio")

  } else {
    fixed_part <- fixed_min
  }

  if (!is.null(tau_prior)) {
    rand_part <-
      paste0(
        "f(intercept_main_bas, model = 'iid', hyper = ", tau_prior, ") +
        f(main_bas1, year_nb, model = 'iid', hyper = ", tau_prior, ") +
        f(intercept_main_bassiteid, model = 'iid', hyper = ", tau_prior, ") +
        f(siteid1, year_nb, model = 'iid', hyper = ", tau_prior, ")"
      )
  } else {
    rand_part <-
      paste0(
        "f(intercept_main_bas, model = 'iid') +
        f(main_bas1, year_nb, model = 'iid') +
        f(intercept_main_bassiteid, model = 'iid') +
        f(siteid1, year_nb, model = 'iid')")
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

get_formula_inla_tps_year_nb <- function(resp = NULL, drivers = TRUE, tau_prior = NULL) {

  fixed_min <- "0 +
    year_nb"

  if (drivers) {

    fixed_part <- paste0(fixed_min, " + ",
      " year_nb : riv_str_rc1 +
      year_nb : hft_ix_c93 +
      year_nb : hft_ix_c9309_log2_ratio +
      year_nb : riv_str_rc1 : hft_ix_c93 +
      year_nb : riv_str_rc1 : hft_ix_c9309_log2_ratio +
      year_nb : hft_ix_c93 : hft_ix_c9309_log2_ratio")

  } else {
    fixed_part <- fixed_min
  }

  if (!is.null(tau_prior)) {
    rand_part <-
      paste0(
        "f(intercept_main_bas, model = 'iid', hyper = ", tau_prior, ") +
        f(main_bas1, year_nb, model = 'iid', hyper = ", tau_prior, ") +
        f(intercept_main_bassiteid, model = 'iid', hyper = ", tau_prior, ") +
        f(siteid1, year_nb, model = 'iid', hyper = ", tau_prior, ")"
      )
  } else {
    rand_part <-
      paste0(
        "f(intercept_main_bas, model = 'iid') +
        f(main_bas1, year_nb, model = 'iid') +
        f(intercept_main_bassiteid, model = 'iid') +
        f(siteid1, year_nb, model = 'iid')")
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

fun_int_env_formula_inla_year_nb <- function(x = NULL, drivers = TRUE, tau_prior = NULL) {
  tar_load(c(tps_var, log_rich_var))

  if (x %in% tps_var) {
    return(get_formula_inla_tps_year_nb(resp = x, drivers = drivers, tau_prior = tau_prior))
  } else if (x %in% c(log_rich_var, "perc_exo_sp", "perc_exo_abun")) {
    return(get_formula_inla_no_tps_year_nb(resp = x, drivers = drivers, tau_prior = tau_prior))
  } else if (x == "log_total_abundance") {
    return(get_formula_inla_abun_year_nb(resp = x, drivers = drivers, tau_prior = tau_prior))
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
  correct_abun_re = FALSE,
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
  if (correct_abun_re & any(str_detect(inla_mod$names.fixed, "unitabundance")) & re_name == "siteid") {
    
    unit_abun_coef <- get_hpdmarginal_inla(inla_mod) %>%
      filter(str_detect(term, "log1_year_nb:unitabundance")) %>%
      mutate(term = str_replace(term, "log1_year_nb:unitabundance", "")) %>%
      distinct(term, mean) %>%
      rename(unitabundance = term, int_log1_year_nb = mean)
    
    re_unit <- modelling_data %>%
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
    trend_class = TRUE) {
  
  mod_list %>%
    mutate(random_site = map2(mod, response,
      ~get_re_prediction_inla(inla_mod = .x,
        modelling_data = modelling_data,
        effect = effect,
        trend_class = trend_class))) %>%
    select(-mod)

}

get_ajusted_re_inla <- function(
  re_pred = gaussian_inla_no_drivers_re_pred,
  effect = gaussian_inla_no_drivers_effects,
  modelling_data = modelling_data,
  resp_to_keep = clust_var
) {

  fixed_trend <- effect %>%
    distinct(response, term, mean)

  fixed_trend_int_unitabundance <- fixed_trend %>%
    filter(
      response == "log_total_abundance",
      str_detect(term, "log1_year_nb:")) %>%
  mutate(unitabundance = str_replace(term,
      "log1_year_nb:unitabundance", "")) 

  inla_site <- re_pred %>%
    filter(response %in% resp_to_keep) %>%
    unnest(cols = c(random_site)) %>%
    left_join(modelling_data %>%
      distinct(siteid, unitabundance),
    by = "siteid"
    ) %>%
    left_join(fixed_trend %>%
      filter(term == "log1_year_nb") %>%
      select(response, fixed_log1_year_nb = mean),
    by = "response"
    ) %>%
    left_join(fixed_trend_int_unitabundance %>%
      select(response, int_unit =  mean, unitabundance),
    by = c("response", "unitabundance")
    ) %>%
    mutate(int_unit = ifelse(is.na(int_unit), 0, int_unit)) %>%
    mutate_at(
      c("mean", "quant0.025", "quant0.5", "quant0.975", "mode"),
      ~.x + fixed_log1_year_nb + int_unit
    )

  return(inla_site)

}

get_cor_biais_inla_tmb_re <- function(
  inla_re =  site_no_drivers_inla,
  tmb_re = site_no_drivers
) {

  inla_re <- inla_re[rownames(inla_re) %in% rownames(tmb_re), ]
  tmb_re <- tmb_re[rownames(tmb_re) %in% rownames(inla_re), ]

  inla_re <- inla_re[match(row.names(inla_re), row.names(tmb_re)), ] %>%
    na.omit()
  tmb_re <- tmb_re[match(row.names(tmb_re), row.names(inla_re)), ] %>%
    na.omit()

  stopifnot(all(row.names(inla_re) == row.names(tmb_re)))

  map_dfr(
    setNames(colnames(inla_re), colnames(inla_re)),
    ~tibble(
      corr_pearson = cor(inla_re[[.x]], tmb_re[[.x]]),
      med_biais_inla_tmb = median(inla_re[[.x]] - tmb_re[[.x]])
      ),
    .id = "response"
  )
}

get_pred_data <- function(
  list_parameter = list(
    log1_year_nb = log(c(0, 10, 20) + 1),
    hft_ix_c93 = with(modelling_data, c(
        min(hft_ix_c93), quantile(hft_ix_c93, probs = .25),
        median(hft_ix_c93), quantile(hft_ix_c93, probs = .75),
        max(hft_ix_c93)
)
      ),
    riv_str_rc1 = with(modelling_data, c(
        min(riv_str_rc1), quantile(riv_str_rc1, probs = .25),
        median(riv_str_rc1), quantile(riv_str_rc1, probs = .75),
        max(riv_str_rc1)
      )
      ),
    hft_ix_c9309_log2_ratio = c(-2, -1, 0, 1, 2, 4)
    ),
  na_var = c(
    "siteid1", "intercept_main_bassiteid",
    "intercept_main_bas", "main_bas1",
    facet_var
  )
  ) {

  na_var_list <- setNames(purrr::map(na_var, ~NA), na_var)
  expand.grid(c(list_parameter, na_var_list)) %>%
    as_tibble()

}

get_pred_inla <- function(
  inla_mod = NULL,
  dataset = NULL,
  pred_data = NULL
  ) {
  fitted_values <- inla_mod$summary.fitted[
    c(nrow(dataset) + 1):nrow(inla_mod$summary.fitted),
    c("mean", "0.025quant", "0.975quant")
    ] %>%
      rename(
      quant0.025 = `0.025quant`,
      quant0.975 = `0.975quant`
      )
  cbind(fitted_values, pred_data) %>%
    as_tibble()
}

get_pred_list_plot <- function(
  pred_data = NULL,
  response = NULL,
  control = list(
    log1_year_nb = pred_data_explanation$log1_year_nb["0"],
    riv_str_rc1 = pred_data_explanation$riv_str_rc1["median"],
    hft_ix_c9309_log2_ratio = pred_data_explanation$hft_ix_c9309_log2_ratio["0"],
    hft_ix_c93 = pred_data_explanation$hft_ix_c93["median"]
  )
  ) {
  
  for (i in names(control)) {
   pred_data[[i]] <- round(pred_data[[i]], 2)
  }

  p_year_hft9309 <- pred_data %>%
    filter(
      riv_str_rc1 == round(control$riv_str_rc1, 2),
      hft_ix_c93 == round(control$hft_ix_c93, 2)
      ) %>%
    ggplot(aes(x = log1_year_nb, y = mean)) +
    geom_line(aes(color = as.factor(hft_ix_c9309_log2_ratio))) +
    geom_ribbon(
      aes(
        ymin = quant0.025, ymax = quant0.975,
        fill = as.factor(hft_ix_c9309_log2_ratio)
        ),
      alpha = .2
      ) +
    labs(
      fill = "Human footprint change (1993-2009)",
      color = "Human footprint change (1993-2009)",
      x = "Log (Year nb + 1)",
      y = paste0("Predicted values of ", response)
    )

  p_year_hft93 <- pred_data %>%
    filter(
      riv_str_rc1 == round(control$riv_str_rc1, 2),
      hft_ix_c9309_log2_ratio == round(control$hft_ix_c9309_log2_ratio, 2)
      ) %>%
    ggplot(aes(x = log1_year_nb, y = mean)) +
    geom_line(aes(color = as.factor(hft_ix_c93))) +
    geom_ribbon(
      aes(
        ymin = quant0.025, ymax = quant0.975,
        fill = as.factor(hft_ix_c93)), alpha = .2) +
    labs(
      fill = "Human footprint (1993)",
      color = "Human footprint (1993)",
      x = "Log (Year nb + 1)",
      y = paste0("Predicted values of ", response)
    )

  p_year_riv <- pred_data %>%
    filter(
      hft_ix_c93 == round(control$hft_ix_c93, 2),
      hft_ix_c9309_log2_ratio == round(control$hft_ix_c9309_log2_ratio, 2)
      ) %>%
    ggplot(aes(x = log1_year_nb, y = mean)) +
    geom_line(aes(color = as.factor(riv_str_rc1))) +
    geom_ribbon(
      aes(
        ymin = quant0.025, ymax = quant0.975,
        fill = as.factor(riv_str_rc1)), alpha = .2) +
    labs(
      fill = "PCA axis related to stream gradient",
      color = "PCA axis related to stream gradient",
      x = "Log (Year nb + 1)",
      y = paste0("Predicted values of ", response)
    )

  p_riv_hft93 <- pred_data %>%
    filter(
      log1_year_nb == round(control$log1_year_nb, 2),
      hft_ix_c9309_log2_ratio == round(control$hft_ix_c9309_log2_ratio, 2)
      ) %>%
    ggplot(aes(x = riv_str_rc1, y = mean)) +
    geom_line(aes(color = as.factor(hft_ix_c93))) +
    geom_ribbon(
      aes(
        ymin = quant0.025, ymax = quant0.975,
        fill = as.factor(hft_ix_c93)), alpha = .2) +
    labs(
      fill = "Human footprint (1993)",
      color = "Human footprint (1993)",
      x = "PCA axis related to stream gradient",
      y = paste0("Predicted values of ", response, " (at T = 10 years)")
    )

  p_riv_hft9309 <- pred_data %>%
    filter(
      log1_year_nb == round(control$log1_year_nb, 2),
      hft_ix_c93 == round(control$hft_ix_c93, 2)
      ) %>%
    ggplot(aes(x = riv_str_rc1, y = mean)) +
    geom_line(aes(color = as.factor(hft_ix_c9309_log2_ratio))) +
    geom_ribbon(
      aes(
        ymin = quant0.025, ymax = quant0.975,
        fill = as.factor(hft_ix_c9309_log2_ratio)), alpha = .2) +
    labs(
      fill = "Human footprint change (1993-2009)",
      color = "Human footprint change (1993-2009)",
      x = "PCA axis related to stream gradient",
      y = paste0("Predicted values of ", response, " (at T = 10 years)")
    )

  p_hft93_hft9309 <- pred_data %>%
    filter(
      log1_year_nb == round(control$log1_year_nb, 2),
      riv_str_rc1 == round(control$riv_str_rc1, 2)
      ) %>%
    ggplot(aes(x = hft_ix_c93, y = mean)) +
    geom_line(aes(color = as.factor(hft_ix_c9309_log2_ratio))) +
    geom_ribbon(
      aes(
        ymin = quant0.025, ymax = quant0.975,
        fill = as.factor(hft_ix_c9309_log2_ratio)), alpha = .2) +
    labs(
      fill = "Human footprint change (1993-2009)",
      color = "Human footprint change (1993-2009)",
      x = "Human footprint (1993)",
      y = paste0("Predicted values of ", response, " (T = 10 years)")
    )

  list(
    year_hft9309 = p_year_hft9309,
    year_hft93 = p_year_hft93,
    year_riv = p_year_riv,
    riv_hft93 = p_riv_hft93,
    riv_hft9309 = p_riv_hft9309,
    hft93_hft9309 = p_hft93_hft9309
  )
}

HC.prior  = "expression:
  sigma = exp(-theta/2);
  gamma = 25;
  log_dens = log(2) - log(pi) - log(gamma);
  log_dens = log_dens - log(1 + (sigma / gamma)^2);
  log_dens = log_dens - log(2) - theta / 2;
  return(log_dens);
"
