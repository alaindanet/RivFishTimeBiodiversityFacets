model_rigal_spamm <- function(
  formula = NULL,
  dataset = NULL
){
  spaMM::fitme(formula = as.formula(formula), data = dataset)
}

transform01 <- function(x, na.omit = TRUE) {
  if (na.omit) {
    len <- length(na.omit(x))
  } else {
    len <- length(x) 
  }

  (x * (len - 1) + 0.5) / (len)
}
