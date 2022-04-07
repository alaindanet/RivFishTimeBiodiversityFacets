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
  } else if (type == "rand") {
    m <- inla_mod$marginals.hyperpar
  }
  map_dfr(m,
    ~inla.hpdmarginal(marginal = .x, p = c(.80, .90, 0.95)) %>%
    as.data.frame() %>%
    rownames_to_column("ci_level"),
  .id = "term"
  )
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
