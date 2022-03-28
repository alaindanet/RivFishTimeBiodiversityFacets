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
      response = get_var_replacement()[unique(x$response)]
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
