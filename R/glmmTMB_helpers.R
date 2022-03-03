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

plot_pred_obs_glmmtmb <- function(x = NULL) {
  resp_var <- names(x$modelInfo$respCol)

  ggplot(tibble(obs = get_data(x)[[resp_var]], pred = predict(x)),
    aes(x = obs, y = pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 3, color = "blue") +
  labs(x = "Observations", y = "Predictions")
}

get_formula_tps_indices <- function(resp = NULL) {

  rhs <- "0 + log1_year_nb / riv_str_rc1 +
    log1_year_nb / hft_ix_c9309_diff_scaled +
    (0 + log1_year_nb | main_bas/siteid)"

  form <- as.formula(paste0(resp, " ~ ", rhs))
  return(form)
}

get_formula_non_tps <- function(resp = NULL) {

  rhs <- "log1_year_nb * riv_str_rc1 +
    log1_year_nb * hft_ix_c9309_diff_scaled +
    (1 + log1_year_nb | main_bas/siteid)"

  form <- as.formula(paste0(resp, " ~ ", rhs))
  return(form)
}
get_formula_abun <- function(resp = NULL) {

  rhs <- "log1_year_nb * riv_str_rc1 +
    log1_year_nb * hft_ix_c9309_diff_scaled +
    (1 + log1_year_nb | main_bas/siteid)"

  form <- as.formula(paste0(resp, " ~ ", rhs))
  return(form)
}

model_comp_interp_int <- function(resp =NULL, df = modelling_data) {

  ne <- glmmTMB(formula = as.formula(paste0(resp, "~
        log1_year_nb * riv_str_rc1 +
        log1_year_nb * hft_ix_c9309_diff_scaled +
        (0 + log1_year_nb | main_bas/siteid)")),
  data = df 
  )

  ne0 <- update(ne,
    formula = as.formula(paste0(resp, "~
        0 + log1_year_nb * riv_str_rc1 +
        log1_year_nb * hft_ix_c9309_diff_scaled +
        (0 + log1_year_nb | main_bas/siteid)"))
  )

  ne0_no_main <- update(ne,  
    formula = as.formula(paste0(resp, "~
        0 + log1_year_nb / riv_str_rc1 +
        log1_year_nb / hft_ix_c9309_diff_scaled +
        (0 + log1_year_nb | main_bas/siteid)"))
  )

  list(main = ne, main0 = ne0, no_main0 = ne0_no_main)
}
