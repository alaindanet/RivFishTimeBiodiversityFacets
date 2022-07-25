
source("https://raw.githubusercontent.com/EmilHvitfeldt/emilfun/master/R/palette_scrapers.R")

make_custom_boxplot <- function(x = NULL, aes_col = cl) {
  bp_theme <- theme_minimal() +
    theme(
      legend.title = element_blank(),
      panel.grid.major.x = element_blank(),
      title = element_blank()#, legend.position = "none"
    )

  x <- x +
    geom_hline(yintercept = 0) +
    geom_boxplot(outlier.colour = NA, outlier.fill = NA,
      outlier.size = NA,
      aes(colour = {{aes_col}}, fill = {{aes_col}}))

  dat_tmp <- ggplot_build(x)$data
  if (length(dat_tmp) == 1) {
    dat <- dat_tmp[[1]]
  } else {
    dat <- dat_tmp[[2]]
  }


  x +
    labs(x = "Response variable", y = "Scaled values") +
    scale_x_discrete(position = "top") +
    geom_segment(data = dat,
      inherit.aes = FALSE,
      aes(x = xmin, xend = xmax, y = middle, yend = middle),
      colour = "white", size = 1) +
    bp_theme
}

target_bp_cl_dist <- function(cl_obj = site_cl_rm, fct_var_lvl = NULL) {

  bp_cl_df <- cl_obj %>%
    mutate(cl = as.factor(cl)) %>%
    select(-riv_str_rc1, -hft_c9309_scaled_no_center) %>%
    pivot_longer(
      -c(siteid, cl),
      names_to = "variable",
      values_to = "value"
    )

    bp_cl_df <- bp_cl_df %>%
      mutate(variable = get_var_replacement_vulgarisation()[variable])

    if (!is.null(fct_var_lvl)) {
      bp_cl_df <- bp_cl_df %>%
        mutate(variable = factor(variable, levels = fct_var_lvl))
    }

    bp_cl_dist <- bp_cl_df %>%
      ggplot(aes(x = variable, y = value, fill = as.factor(cl)))


    make_custom_boxplot(bp_cl_dist)

}

make_interaction_heatmap_tps_env <- function(data = NULL) {

  x <- data %>%
    filter(facet == "dbl_interaction", !is.na(Coefficient)) %>%
    mutate(Parameter = str_remove(Parameter,
        "Log \\(Year nb \\+ 1\\) \\* ")
      ) %>%
    separate(Parameter, c("Parameter", "interaction"), sep = " \\* ")

  xx <- data %>%
    filter(facet == "interaction") %>%
    mutate(Parameter = str_remove(Parameter, "Log \\(Year nb \\+ 1\\) \\* "))

  te <- expand.grid(list(
      Parameter = unique(x$Parameter),
      interaction = unique(x$interaction),
      response = unique(x$response)
      ))

  te2 <- te %>%
    left_join(x %>%
      select(Parameter, interaction, response, Coefficient),
    by = c("response", "interaction", "Parameter")) %>%
  left_join(xx %>%
    select(Parameter, response, Coefficient) %>%
    rename(simple = Coefficient), by = c("response", "Parameter")) %>%
  filter(!is.na(Coefficient)) %>%
  mutate(outcome = Coefficient * simple)

te2 %>%
  mutate(term = str_c(Parameter, " / ", interaction)) %>%
  ggplot(aes(x = response, y = term, fill = outcome)) +
  geom_tile() +
  geom_text(aes(label = formatC(outcome, digits = 1, format = "e"))) +
  scale_fill_distiller(
    type = "div",
    direction = -1,
    labels = scales::label_scientific(digits = 1),
    palette = "RdBu") +
  labs(
    fill = "Standardized coefficients"
    ) +
  theme_minimal()


}

plot_model_comp_interaction <- function(
  mod_comp_df = mod_exo_comp_std_df
) {
  p <- mod_comp_df %>%
    filter(facet == "main") %>%
    mutate(Parameter = fct_reorder(Parameter, CI_high, .fun="max")) %>%
    ggplot(
      aes(y = Parameter, x = Coefficient, color = response, xmin = CI_low,
        xmax = CI_high)) +
    geom_vline(xintercept = 0) +
    geom_pointrange(position = position_dodge(width = .7)) +
    labs(x = "Standardized coefficients", colour = "Response variables") +
    theme(legend.position = "bottom") +
    guides(color = guide_legend(nrow = 2, byrow = TRUE))


  #withr::with_options(
  #  list(ggplot2.discrete.colour = pal),
  #  print(p + scale_color_discrete())
  #)

  p1 <- mod_comp_df %>%
    filter(facet == "interaction") %>%
    ggplot(
      aes(y = Parameter, x = Coefficient, color = response,
        xmin = CI_low, xmax = CI_high)
      ) +
    geom_vline(xintercept = 0) +
    geom_pointrange(position = position_dodge(width = .7)) +
    theme(axis.title = element_blank(), legend.position = "none")

  p3 <- mod_comp_df %>%
    filter(facet == "dbl_interaction") %>%
    ggplot(
      aes(
        y = Parameter, x = Coefficient,
        color = response,
        xmin = CI_low, xmax = CI_high)
      ) +
    geom_vline(xintercept = 0) +
    geom_pointrange(position = position_dodge(width = .7)) +
    theme(legend.position = "none", axis.title.y = element_blank())

  leg <- get_legend({p +
    theme(legend.direction = "horizontal") +
    guides(colour = guide_legend(nrow = 3))})
  p2 <- p +
    theme(
      plot.margin = margin(0, 5.5, 6, 64),
      axis.title = element_blank(),
      legend.position = "none"
    )
  p_mod_coef <- plot_grid(p2, p1, p3, leg,
    nrow = 4,
    rel_heights = c(1, 1, 1, .2))

  return(p_mod_coef)

}

plot_pca_clust <- function(
  .data = NULL,
  site_cl = NULL,
  xaxis = "RC1", yaxis = "RC2",
  ctb_thld = .4, label_size = 2,
  size_abscisse_segment = .25,
  size_arrows_segment = .25,
  force = 10, force_pull = 1,
  var_scaling_factor = NULL,
  seed = NA,
  replace_var = get_rev_vec_name_val(get_river_atlas_significant_var()),
  add_variable = TRUE,
  add_point = TRUE,
  add_ellipse = TRUE,
  alpha_point = 1,
  lim_x_y = c(-1, 1),
  key_glyph_rect = TRUE
  ) {

  if ("psych" %in% class(.data)) {
    var_loadings <- .data$loadings[seq_len(nrow(.data$loadings)), ]
  } else {
    var_loadings <- .data$c1[seq_len(nrow(.data$c1)), ]
  }
  pca_data <- var_loadings %>%
    as.data.frame() %>%
    rownames_to_column("variable") %>%
    as_tibble()

  if (add_point | add_ellipse) {
    if ("psych" %in% class(.data)) {
      ind_score <- .data$scores
    } else {
      ind_score <- .data$l1
      colnames(ind_score) <- str_replace(colnames(ind_score), "R", "C")
    }
      tt <- ind_score %>%
        as.data.frame() %>%
        rownames_to_column("siteid") %>%
        as_tibble() %>%
        left_join(site_cl[, c("siteid", "cl")], by = "siteid") %>%
        na.omit()
  }

  if (!is.null(replace_var)) {
    pca_data %<>%
      mutate(variable = replace_var[variable])
  }

  if ("psych" %in% class(.data)) {
    var_exp <- round(.data$Vaccounted["Proportion Var", ] * 100)
  } else {
    var_exp <- round(.data$eig / sum(.data$eig) * 100)
    names(var_exp) <- colnames(.data$c1)
  }

  p <- ggplot() +
    geom_segment(
      data = tibble(x = c(lim_x_y[1], 0), xend = c(lim_x_y[2], 0),
        y = c(0, lim_x_y[1]), yend = c(0, lim_x_y[2])),
      aes(x = x, y = y, xend = xend, yend = yend),
      size = size_abscisse_segment) +
    lims(x = lim_x_y, y = lim_x_y) +
    coord_cartesian(
      expand = FALSE
    ) +
    labs(x = paste0(xaxis, " (", var_exp[xaxis], "%)"),
      y = paste0(yaxis, " (", var_exp[yaxis], "%)")) +
    theme_bw()

  if (add_point) {
    p <- p +
      geom_point(data = tt,
        aes_string(x = xaxis, y = yaxis, colour = "as.factor(cl)"),
        alpha = alpha_point
      )
  }
  if (add_ellipse) {
    p <- p +
      stat_ellipse(
        data = tt,
        aes_string(
          x = xaxis,
          y = yaxis,
          #key_glyph = ifelse(key_glyph_rect, "rect", "line"),
          colour = "as.factor(cl)"
          ),
        segments = 100,
        level = .9
      )
  }

  if (add_variable) {

    pca_label <- pca_data %>%
      filter(
        abs(.data[[xaxis]]) > ctb_thld |
          abs(.data[[yaxis]]) > ctb_thld
      )

    if (!is.null(var_scaling_factor)) {
      pca_data[[xaxis]] <- pca_data[[xaxis]] * var_scaling_factor
      pca_data[[yaxis]] <- pca_data[[yaxis]] * var_scaling_factor

      pca_label[[xaxis]] <- pca_label[[xaxis]] * var_scaling_factor
      pca_label[[yaxis]] <- pca_label[[yaxis]] * var_scaling_factor
    }

    p <- p +
      geom_segment(data = pca_data,
        aes_string(x = 0, y = 0, xend = xaxis, yend = yaxis),
        arrow = ggplot2::arrow(angle = 20, length = unit(0.05, "inches"),
          ends = "last", type = "open"),
        size = size_arrows_segment
        ) +
      ggrepel::geom_label_repel(data = pca_label,
        aes_string(x = xaxis, y = yaxis, label = "variable"),
        size = label_size,
        force = force, force_pull = force_pull, seed = seed,
        box.padding = .1, label.padding = .15
      )
  }
  return(p)
}

get_pca_clust_list <- function(
  pca = NULL,
  axis_list = list(
  c("RC1", "RC2"),
  c("RC1", "RC3"),
  c("RC1", "RC4"),
  c("RC1", "RC5"),
  c("RC2", "RC3"),
  c("RC2", "RC4"),
  c("RC2", "RC5"),
  c("RC3", "RC4"),
  c("RC3", "RC5"),
  c("RC4", "RC5")),
  site_cl = NULL,
  add_point = TRUE,
  add_ellipse = TRUE,
  color_scale = NULL,
  label_size = 2.5) {

  if (is.null(color_scale)) {
    pal <- palette_coolors("https://coolors.co/palette/01befe-ffdd00-ff7d00-ff006d-adff02-8f00ff")
    color_scale <- setNames(pal, unique(site_cl$cl))
  }

  p_pca_cl_ell <- map(axis_list,
  ~plot_pca_clust(
     .data = pca,
     site_cl = site_cl,
     add_point = add_point,
     add_ellipse = add_ellipse,
     xaxis = .x[1], yaxis = .x[2],
     ctb_thld = .2,
     replace_var = get_var_replacement_vulgarisation(),
     size_arrows_segment = 1,
     label_size = label_size,
     alpha_point = .2,
     lim_x_y = c(-3.5, 3.5),
     force_pull = 1,
     force = 80,
     var_scaling_factor = 3.5) +
   theme(legend.position = "bottom") +
   scale_color_manual(values = color_scale)
  )
  leg_cl <- get_legend(p_pca_cl_ell[[1]])
  p_pca_cl_ell <- map(p_pca_cl_ell, ~.x + theme(legend.position = "none"))

  bp <- target_bp_cl_dist(cl_obj = site_cl) +
    scale_fill_manual(values = color_scale) +
    scale_color_manual(values = color_scale) +
    theme(axis.text.x = element_text(angle = 20, vjust = 0))

  output <- list(
    pca_list = p_pca_cl_ell,
    leg = leg_cl, 
    bp = bp
  )

  return(output)

}

#################
#  Predictions  #
#################

comp_pred_main <- function(
  pred = NULL,
  var_comp = list(
    hft_ix_c93 = c(
      "min" = .2, "intact" = 2.5, "quant0.25" = 8.8,
      "median" = 16.8, "quant0.75" = 26.4, "max" = 45.6)
    ),
  control = c("log1_year_nb" = 0,
    "riv_str_rc1" = -0.2115785,
    "hft_ix_c9309_log2_ratio" = 0)
  ) {

  mask_control <-
    round(pred[[names(control)[1]]], 2) == round(control[[ names(control)[1] ]], 2) &
    round(pred[[names(control)[2]]], 2) == round(control[[ names(control)[2] ]], 2) &
    round(pred[[names(control)[3]]], 2) == round(control[[ names(control)[3] ]], 2)

  out <- pred[mask_control, ]

  prediction <- map_dbl(var_comp[[1]],
    ~out[round(out[[names(var_comp)[1]]], 1) %in% round(.x, 1), ]$mean
  )
  prediction <- map_dbl(var_comp[[1]],
    ~out[round(out[[names(var_comp)[1]]], 1) %in% round(.x, 1), ]$mean
  )
  out <- list(
  mean = setNames(prediction, names(var_comp[[1]])),
  quant0.025 = setNames(map_dbl(var_comp[[1]],
    ~out[round(out[[names(var_comp)[1]]], 1) %in% round(.x, 1), ][["quant0.025"]]
  ), names(var_comp[[1]])),
  quant0.975 = setNames(map_dbl(var_comp[[1]],
    ~out[round(out[[names(var_comp)[1]]], 1) %in% round(.x, 1), ][["quant0.975"]]
  ), names(var_comp[[1]]))
  )
  return(out)
}
