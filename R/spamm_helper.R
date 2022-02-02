model_rigal_spamm <- function(
  formula = NULL,
  dataset = NULL
){
  spaMM::fitme(formula = as.formula(formula), data = dataset)
}