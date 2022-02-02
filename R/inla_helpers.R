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
