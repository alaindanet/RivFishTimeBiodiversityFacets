compute_riv_str_pca <- function(riv = NULL, ncomp = 2) {

  riv_str_var <- colnames(riv)[
    colnames(riv) %in%
      c("dist_up_km", "ord_stra", "dis_m3_pyr", "ele_mt_cav", "slp_dg_cav")
    ]

  logged_riv_str <- riv %>%
    st_drop_geometry() %>%
    select(all_of(c("siteid", riv_str_var))) %>%
    # Log adapted to -1 in one elevation site, most of variable are % highly
    # skewed
    mutate(across(where(is.numeric), ~log(.x + abs(min(.x)) + 1)))

  mypca <- compute_rotated_pca(
    .data = scale(select(logged_riv_str, -siteid)), naxis = ncomp)
  return(mypca)

}

compute_rotated_pca <- function(.data = NULL, naxis = 2) {
  .data <- na.omit(.data)
  pca <- ade4::dudi.pca(as.data.frame(.data),
    scannf = FALSE, nf = naxis, center = TRUE, scale = TRUE
  )
  pca_rotated <- psych::principal(.data,
    rotate = "varimax", nfactors = naxis,
    scores = TRUE
  )

  return(list(normal = pca, rotated = pca_rotated))
}

plot_rotated_pca <- function(pca_rotated = NULL, axis = c(1, 2)) {
  {
    adegraphics::adegpar()$plabels
    ade4::s.corcircle(
      pca_rotated$rotated$loadings[1:nrow(pca_rotated$rotated$loadings), ],
      xax = axis[1],
      yax = axis[2], box = TRUE
    )
    mtext(paste0("RC ", axis[1]), side = 1, adj = .5, line = -1)
    mtext(paste0("RC ", axis[2]), side = 2, adj = .5, line = -5)
    rotated_plot <- grDevices::recordPlot()
  }
  pca_plot <- factoextra::fviz_pca_var(pca_rotated$normal,
    axes = axis,
    col.var = "contrib", # Color by contributions to the PC
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE # Avoid text overlapping
  )
  return(list(rotated = rotated_plot, normal = pca_plot))
}


my_pca_plot <- function(
  .data = NULL,
  xaxis = "RC1", yaxis = "RC2",
  ctb_thld = .4, label_size = 2,
  force = 10,  force_pull = 1,
  seed = NA,
  replace_var = get_rev_vec_name_val(get_river_atlas_significant_var())
  ) {

  pca_data <- .data$loadings[1:nrow(.data$loadings), ] %>%
    as.data.frame() %>%
    rownames_to_column("variable") %>%
    as_tibble()

  pca_data %<>%
    mutate(variable = replace_var[variable])

    pca_label <- pca_data %>%
      filter(
        abs(.data[[xaxis]]) > ctb_thld |
          abs(.data[[yaxis]]) > ctb_thld
      )

    var_exp <- round(.data$Vaccounted["Proportion Var", ] * 100)

    ggplot() +
      ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1),
        inherit.aes = FALSE
        ) +
      geom_segment(
        data = tibble(
          x = c(-1, 0), xend = c(1, 0),
          y = c(0, -1), yend = c(0, 1)
          ),
        aes(x = x, y = y, xend = xend, yend = yend),
        size = .25
        ) +
      xlim(-1, 1) +
      ylim(-1, 1) +
      labs(
        x = paste0(xaxis, " (", var_exp[xaxis], "%)"),
        y = paste0(yaxis, " (", var_exp[yaxis], "%)")
        ) +
      geom_segment(data = pca_data,
        aes_string(x = 0, y = 0, xend = xaxis, yend = yaxis),
        arrow = ggplot2::arrow(angle = 20, length = unit(0.05, "inches"),
          ends = "last", type = "open"),
        size = .25
        ) +
      ggrepel::geom_label_repel(data = pca_label,
        aes_string(x = xaxis, y = yaxis, label = "variable"),
        size = label_size,
        force = force, force_pull = force_pull, seed = seed,
        box.padding = .1, label.padding = .1
      )
}
