#' Run spamm models
#'
get_spamm_model <- function(
  resp_var = NULL,
  rhs_formula = NULL,
  family = NULL,
  dataset = NULL) {

  form <- as.formula(
    paste0(
      resp_var,
      " ~ ",
      rhs_formula
    )
  )

  model <- spaMM::fitme(
    form,
    data = dataset,
    family = family
    )
  return(model)
}

#' Run and save spamm models
#'
#'@examples
#'run_save_spamm_model(
#'  resp_var = "abundance",
#'  rhs_formula = "year",
#'  dataset = il_dataset,
#'  family = "poisson"
#')
run_save_spamm_model <- function(
  resp_var = NULL,
  rhs_formula = NULL,
  family = NULL,
  dataset = NULL) {

  model <- get_spamm_model(
    resp_var = resp_var,
    rhs_formula = rhs_formula,
    family = family,
    dataset = dataset)

  object_name <- paste0(resp_var, "_spamm")
  assign(object_name, model)
  save(object_name, file = here::here("data", paste0(object_name, ".rda")))

  return(model)
}
