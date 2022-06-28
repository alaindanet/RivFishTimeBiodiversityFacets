#' Compare the fixed coefficients of models
#'
#' @param model_list list of model
#' 
plot_model_comp_coeff <- function(model_list = NULL) {

  comp_intercept <- compare_models(model_list, effects = "fixed")
  comp_intercept %>%
    as_tibble() %>%
    select(Parameter, starts_with("CI_"), starts_with("Coefficient")) %>%
    pivot_longer(-Parameter, names_to = "names", values_to = "values") %>%
    separate(col = names, into  = c("type", "response"), sep = "\\.") %>%
    pivot_wider(names_from = "type", values_from = "values") %>%
    filter(!Parameter %in% "(Intercept)", !str_detect(Parameter, "unitabundance")) %>%
    filter(!response %in% c("species_nb", "log_species_nb", "total_abundance", "total_abundance_scaled")) %>%
    ggplot(
      aes(y = Parameter, x = Coefficient, color = response, xmin = CI_low,
        xmax = CI_high)) +
    geom_pointrange(position = position_dodge(width = .2))
}

###########################################################################
#                            INLA                             #
###########################################################################

plot_credible_interval <- function (dataset) {

  # Borrowed from
  # https://github.com/roelvanklink/Final-insect-abundance-changes/blob/master/Full%20INLA%20analysis%20FINAL%20cleaned%2020191230.R 
  dataset %>%
  ggplot() +
    geom_errorbar(aes(x = Unit, ymin = X0.025quant, ymax = X0.975quant, color = Realm),
      alpha = 0.5,
      size = 1, width = 0, position = position_dodge(width = 0.7)
    ) +
    geom_errorbar(aes(x = Unit, ymin = X0.05quant, ymax = X0.95quant, color = Realm),
      alpha = 0.75,
      size = 2, width = 0, position = position_dodge(width = 0.7)
    ) +
    geom_errorbar(aes(x = Unit, ymin = X0.1quant, ymax = X0.9quant, color = Realm),
      alpha = 1,
      size = 3, width = 0, position = position_dodge(width = 0.7)
    ) +
    geom_point(aes(x = Unit, y = mean, shape = Realm),
      size = 2.5, position = position_dodge(width = 0.7),
      color = "black", fill = "black", alpha = 1
    ) +
    #scale_color_manual(values = col.scheme.realm) +
    #scale_fill_manual(values = col.scheme.realm) +
    #scale_shape_manual(values = shps) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_flip()

}

plot_obs_fitted_inla <- function(
  mod_inla = NULL,
  dataset = NULL,
  resp = NULL,
  pred_nrows = NULL,
  return_df = FALSE
  ) {

  fitted_values <- mod_inla$summary.fitted.values[["mean"]]

  if (!is.null(pred_nrows)) {
    row_selection <- length(fitted_values) - pred_nrows
    fitted_values <-
      fitted_values[1:row_selection]
  }

  obs_fit <- tibble(
    response = resp,
    siteid = dataset[["siteid"]],
    year = dataset[["year"]],
    fit = fitted_values,
    obs = dataset[[resp]]
  )

  if (return_df) {
    return(obs_fit)
  }

  obs_fit %>%
    ggplot(aes(x = fit, y = obs)) +
    geom_point(alpha = .5, size = 2) +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    labs(x = "Fitted values", y = "Observed values")
}

plot_posterior_gaussian_sd <- function(inla_mod = NULL) {

  hyper_tb <-
    inla_mod$marginals.hyperpar[["Precision for the Gaussian observations"]]
  sigma_dist <- inla.tmarginal(tau_to_sigma, hyper_tb) %>%
    as_tibble

  sigma_dist %>%
    ggplot(aes(x = x, y = y)) +
    geom_line() +
    labs(x = expression(sigma), y = expression(paste("P(", sigma, " | Data)")))
}

plot_posterior_random <- function(inla_mod = NULL, scales = "free", ncol = 4) {

  hyper_tb <- inla_mod$marginals.hyperpar

  dist_term <- purrr::map_dfr(hyper_tb,
    ~inla.tmarginal(tau_to_sigma, .x) %>%
    as_tibble(),
  .id = "term")

  dist_term %>%
    mutate(term = str_replace_all(term, get_model_term_replacement())) %>%
    ggplot(aes(x = x, y = y)) +
    geom_line() +
    facet_wrap(vars(term), ncol = ncol, scales = scales) +
    labs(x = expression(sigma), y = expression(paste("P(", sigma, " | Data)")))
}

plot_posterior_fixed <- function(inla_mod = NULL, scales = "free", ncol = 4) {

  sum_fix <- inla_mod$marginals.fixed

  dist_term <- purrr::map_dfr(sum_fix,
    ~inla.smarginal(.x) %>%
    as_tibble(),
  .id = "term")

  dist_term %>%
    mutate(term = str_replace_all(term, get_model_term_replacement())) %>%
    ggplot(aes(x = x, y = y)) +
    geom_line() +
    facet_wrap(vars(term), ncol = ncol, scales = scales) +
    labs(x = expression(beta), y = expression(paste("P(", beta, " | Data)")))
}

plot_inla_fixed_effect <- function(
  dataset = NULL,
  scale_color = NULL,
  term_level = NULL,
  xaxis_title = FALSE,
  yaxis_title = FALSE,
  legend_present = FALSE,
  my_position_dodge = .9,
  point_size = 3,
  scale_width_bar = .5
  ) {

  if (is.null(term_level)) {
    term_level <- c(
      "Log (Year nb + 1)",
      "PCA1\nstream gradient",
      "Human footprint\n(1993)",
      "Log2 Human footprint\nratio (2009/1993)",
      "Log (Year nb + 1):\nPCA1\nstream gradient",
      "Log (Year nb + 1):\nHuman footprint\n(1993)",
      "Log (Year nb + 1):\nLog2 Human footprint\nratio (2009/1993)",
      "Log (Year nb + 1):\nPCA1\nstream gradient:\nHuman footprint\n(1993)",
      "Log (Year nb + 1):\nPCA1\nstream gradient:\nLog2 Human footprint\nratio (2009/1993)",
      "Log (Year nb + 1):\nHuman footprint\n(1993):\nLog2 Human footprint\nratio (2009/1993)"
          )
  }

  p <- dataset %>%
    mutate(
      term = factor(term, levels = term_level)
    ) %>%
    ggplot(aes(
        y = term, x = mean,
        xmin = low, xmax = high,
        color = response,
        size = width_bar * scale_width_bar, width = 0)
      ) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_y_discrete(limits = rev) +
    geom_blank() +
    geom_errorbar(
      alpha = 0.5,
      position = position_dodge(width = my_position_dodge),
      show.legend = FALSE
      ) +
    geom_point(
      aes(x = mean, y = term, color = response),
      alpha = 1, size = point_size,
      position = position_dodge(width = my_position_dodge)
      )

    if (!xaxis_title) {
      p <- p +
        theme(axis.title.x = element_blank())
    }

    if (!is.null(scale_color)) {
      p <- p +
        scale_color_manual(values = scale_color,
          name = "Community metrics")
    } else {
      p <- p +
        scale_color_brewer(palette = "RdYlBu")
    }

    if (!yaxis_title) {
      p <- p +
        theme(axis.title.y = element_blank())
    }

    if (!legend_present) {
      p <- p +
        theme(legend.position = "none")
    }

    return(p)
}

format_inla_model_list <- function(
  x = gaussian_inla_prior_std,
  response_to_skip = c(
    "species_nb", "log_species_nb", "species_nb_tps_scaled",
    "chao_richness_tps_scaled", "total_abundance",
    "total_abundance_scaled", "total_abundance_tps"),
  prob = c(.80, .90, .95)
  ) {

  x %>%
    filter(!response %in% response_to_skip) %>%
    mutate(
      hpd_fixed = map(
        mod,
        ~get_hpdmarginal_inla(
          inla_mod = .x,
          type = "fixed",
          p = prob
          )
        ),
      hpd_random = map(
        mod,
        ~get_hpdmarginal_inla(
          inla_mod = .x,
          type = "rand",
          p = prob
        )
      )
      ) %>%
    select(-mod) %>%
    select(-hpd_random) %>%
    unnest(hpd_fixed) %>%
    mutate(
      facet = case_when(
        str_count(term, ":") == 1 ~ "interaction",
        str_count(term, ":") == 2 ~ "dbl_interaction",
        TRUE ~ "main"
        ),
      var_type = case_when(
        response %in% c(
          "log_total_abundance",
          "log_chao_richness",
          "perc_exo_sp",
          "perc_exo_abun"
          ) ~
        "quantity",
      TRUE ~ "dissimilarity"
      ),
    width_bar = case_when(
      ci_level == "level:0.95"  ~ (1),
      ci_level == "level:0.90"  ~ (2),
      ci_level == "level:0.80"  ~ (3)
    ))
}
