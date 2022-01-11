plot_community_data <- function(
  dataset = NULL,
  y = NULL,
  x = NULL,
  title = NULL,
  smoothing_method = "loess"
  ) {

  p <- dataset %>%
    ggplot(aes_string(y = y, x = x)) +
    geom_point()

  if (!is.null(smoothing_method)) {
    p <- p +
      geom_smooth(method = smoothing_method, formula = "y ~ x")
  }

  if (!is.null(title)) {
    p <- p +
      labs(title = title)
  }

  return(p)
}


#' replace NA by 0 in abundance data 
#'
#' @param  dataset data.frame
#' @param  y_var chr
#' @param  species_var chr
#' @param  time_var  chr
#' @param  long_format lgl
#'
#' @examples
#'tar_load(toy_dataset)
#'ti <- toy_dataset %>%
#'  filter(siteid == unique(toy_dataset$siteid)[2])
#'get_com_matrix_from_site(dataset = ti, y_var = "abundance")

#' Add 0 to non-observed population in a site
#'
#' @examples
#'get_com_matrix_from_site(dataset = ti, y_var = "abundance")
get_com_matrix_from_site <- function(
  dataset = NULL,
  y_var = NULL,
  species_var = "species",
  time_var = "year",
  long_format = TRUE

) {

  com <- dataset[, c(time_var, species_var, y_var)]

  species <- unique(com[[species_var]])

  com <- com %>%
    pivot_wider(names_from = species_var, values_from = y_var) %>%
    mutate(across(species, ~replace(.x, is.na(.x), 0))) %>%
    arrange(!!sym(time_var)) %>%
    complete(!!sym(time_var) := full_seq(!!sym(time_var), 1))

  if (long_format) {
    com <- com %>%
      pivot_longer(cols = species, names_to = species_var, values_to = y_var)
  }
  return(com)
}

plot_temporal_population <- function(
  com = NULL,
  y_var = "abundance",
  time_var = "year",
  species_var = "species",
  stacked = TRUE,
  ribbon = FALSE,
  .log = FALSE,
  color_species = NULL,
  label = NULL,
  label_parsed = FALSE,
  label_size = 4.5,
  y_label = NULL,
  my_ylim = NULL) {

  # get bm dynamic
  com <- get_com_matrix_from_site(
    dataset = com,
    y_var = y_var,
    species_var = species_var,
    time_var = time_var,
    long_format = TRUE
  )

  # get total y
  total <- com %>%
    group_by(!!sym(time_var)) %>%
    summarise(!!sym(y_var) != sum(!!sym(y_var)), .groups = "drop") %>%
    mutate(!!sym(species_var) := "Total")

  p <- com %>%
    ggplot(aes_string(x = time_var, y = y_var, color = species_var))



  if (stacked) {
    if (ribbon) {
      com <- arrange(com, !!sym(time_var), !!sym(species_var)) 
      com$ymax <- com[[y_var]]
      com$ymin <- 0
      zl <- unique(com[[species_var]])
      for (i in 2:length(zl)) {
        zi <- com[[species_var]] == zl[i]
        zi_1 <- com[[species_var]] == zl[i - 1]
        com$ymin[zi] <- com$ymax[zi_1]
        com$ymax[zi] <- com$ymin[zi] + com$ymax[zi]
      }

      p <- com %>%
        ggplot(
          aes_string(
          x = time_var,
          y = y_var,
          ymax = "ymax",
          ymin = "ymin",
          fill = species_var)
          ) + geom_ribbon()
    } else {
      p <- p +
        geom_area(aes_string(fill = species_var))

    }
  } else {
    p <- p +
      geom_line() +
      geom_line(data = total, color = "black")
  }

  if (!is.null(my_ylim)) {
    p <- p +
      ylim(my_ylim)
  }

  # Make it professional:
  p <- p +
    labs(y = expression(Abundance), x = "Year")

#  if (!is.null(sem_df)) {
#    label <- get_network_summary(com = sem_df, station = station)
#  }

  if (!is.null(label)) {
    if (is.null(y_label)) {
      y_label <- max(total[[y_var]]) + max(total[[y_var]]) * 5 / 100
    }

    p <- p +
    annotate("text", x = median(total[[year]]),
      y = y_label,
      label = label, parse = label_parsed, size = label_size)
  }

  if (.log) {
    p <- p + scale_y_log10()
  }

  if (!is.null(color_species)) {
    p <- p +
      scale_fill_manual(values = color_species) +
      scale_color_manual(values = color_species)
  }

  return(p)


}
