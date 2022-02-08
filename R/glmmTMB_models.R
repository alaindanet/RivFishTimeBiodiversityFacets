#' Jaccard model
temporal_jaccard <- function(
  formula = "jaccard_scaled ~
  0 + year_nb/scaled_dist_up_km +
  (0 + year_nb | main_bas/siteid) +
  (0 + year_nb:scaled_dist_up_km | main_bas)
  ",
  data = na.omit(analysis_dataset),
  family = beta_family(link = "logit"),
  dispformula = "~ year_nb + scaled_dist_up_km",
  offset = rep(1.0, nrow(na.omit(analysis_dataset)))
) {

  glmmTMB::glmmTMB(
    formula = as.formula(formula),
    data = data,
    family = family,
    offset = offset,
    dispformula = as.formula(dispformula)
  )

}
