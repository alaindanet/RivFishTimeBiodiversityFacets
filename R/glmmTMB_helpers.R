#' Extract random effect from glmmTMB
#'
#' @param fit a glmmTMB model 
#' @param effect a character vector designating a random effect
#' @return a tibble
get_random_effect_glmmTMB <- function(fit = NULL, effect = NULL) {
  nested <- str_detect(effect, ":")

  stopifnot(effect %in% names(coef(fit)$cond))

  coefficient <- coef(fit)$cond[[effect]]

  tmp <- coefficient %>%
    rownames_to_column(var = effect)

  if (nested) {
    new_cols <- str_split(effect, ":", simplify = TRUE)[1, ]

    tmp <- tmp %>%
      separate(
        col = effect,
        into = new_cols,
        sep = ":"
      )
  }

  return(as_tibble(tmp))
}
