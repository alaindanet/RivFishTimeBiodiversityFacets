make_custom_boxplot <- function(x = NULL, aes_col = cl) {
  bp_theme <- theme_minimal() +
    theme(
      legend.title = element_blank(),
      panel.grid.major.x = element_blank(),
      title = element_blank(),
      legend.position = "none"
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

target_bp_cl_dist <- function(cl_obj = site_cl_rm) {

  bp_cl_df <- cl_obj %>%
    mutate(cl = as.factor(cl)) %>%
    select(-riv_str_rc1, -hft_c9309_scaled_no_center) %>%
    pivot_longer(
      -c(siteid, cl),
      names_to = "variable",
      values_to = "value"
    )

    bp_cl_dist <- bp_cl_df %>%
      mutate(variable = get_var_replacement()[variable]) %>%
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
      aes(y = Parameter, x = Coefficient, color = response,
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
