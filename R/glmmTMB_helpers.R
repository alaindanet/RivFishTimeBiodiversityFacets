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

get_formula_tps <- function(resp = NULL, int_env = FALSE,
  hft = "hft_ix_c9309_log2_ratio") {

  rhs_fixed <- paste0("0 + log1_year_nb / riv_str_rc1 +
    log1_year_nb / hft_ix_c93 +
    log1_year_nb /", hft)
  rhs_random <- "+ (0 + log1_year_nb | main_bas/siteid)"

  if (int_env) {
    int <- paste0(
      "+ log1_year_nb:riv_str_rc1:", hft,
      "+ log1_year_nb:riv_str_rc1:hft_ix_c93",
      "+ log1_year_nb:hft_ix_c93:", hft
    )
  } else {
   int <- ""

  }

  form <- as.formula(paste0(resp, " ~ ", rhs_fixed, int, rhs_random))
  return(form)
}

get_formula_tps_no_drivers <- function(resp = NULL) {

  rhs <- "0 + log1_year_nb +
    (0 + log1_year_nb | main_bas/siteid)"

  form <- as.formula(paste0(resp, " ~ ", rhs))
  return(form)
}

get_formula_no_tps <- function(resp = NULL,
  int_env = FALSE, hft = "hft_ix_c9309_log2_ratio") {

  rhs_fixed <- paste0("
    log1_year_nb * riv_str_rc1 +
    log1_year_nb * hft_ix_c93 +
    log1_year_nb * ", hft)
  rhs_random <- "+ (1 + log1_year_nb | main_bas/siteid)"

  if (int_env) {
    int <- paste0(
      "+ log1_year_nb:riv_str_rc1:", hft,
      "+ log1_year_nb:riv_str_rc1:hft_ix_c93",
      "+ log1_year_nb:hft_ix_c93:", hft
    )
  } else {
   int <- ""
  }

  form <- as.formula(paste0(resp, " ~ ", rhs_fixed, int, rhs_random))
  return(form)
}

get_formula_no_tps_no_drivers <- function(resp = NULL) {

  rhs <- "log1_year_nb +
    (1 + log1_year_nb | main_bas/siteid)"

  form <- as.formula(paste0(resp, " ~ ", rhs))
  return(form)
}

get_formula_abun <- function(resp = NULL, int_env = FALSE,
  hft = "hft_ix_c9309_log2_ratio") {

  rhs_fixed <- paste0("
    log1_year_nb * unitabundance +
    log1_year_nb * riv_str_rc1 +
    log1_year_nb * hft_ix_c93 +
    log1_year_nb *", hft) 
  rhs_random <- "+ (1 + log1_year_nb | main_bas/siteid)"

  if (int_env) {
    int <- paste0(
      "+ log1_year_nb:riv_str_rc1:", hft,
      "+ log1_year_nb:riv_str_rc1:hft_ix_c93",
      "+ log1_year_nb:hft_ix_c93:", hft
    )
  } else {
   int <- ""
  }

  form <- as.formula(paste0(resp, " ~ ", rhs_fixed, int, rhs_random))
  return(form)
}

get_formula_abun_no_drivers <- function(resp = NULL) {

  rhs <- "
  log1_year_nb * unitabundance +
    (1 + log1_year_nb | main_bas/siteid)"

  form <- as.formula(paste0(resp, " ~ ", rhs))
  return(form)
}

fun_no_driver_formula <- function(x = NULL) {
  tar_load(c(tps_var, log_rich_var))
  
  if (x %in% tps_var) {
    return(get_formula_tps_no_drivers(resp = x))
  } else if (x %in% log_rich_var) {
    return(get_formula_no_tps_no_drivers(resp = x))
  } else if (x == "log_total_abundance") {
    return(get_formula_abun_no_drivers(resp = x))
  } else {
    stop("no defined variables")
  }
}

fun_int_env_formula <- function(x = NULL) {
  tar_load(c(tps_var, log_rich_var))

  if (x %in% tps_var) {
    return(get_formula_tps(resp = x, int_env = TRUE))
  } else if (x %in% log_rich_var) {
    return(get_formula_no_tps(resp = x, int_env = TRUE))
  } else if (x == "log_total_abundance") {
    return(get_formula_abun(resp = x, int_env = TRUE))
  } else {
    stop("no defined variables")
  }
}

fun_int_env_formula_exo <- function(x = NULL) {
  tps_var <- NULL
  rich_var <- c(
    "species_nb", "species_nb_nat", "species_nb_exo",
    "perc_exo_sp", "perc_nat_sp", "perc_exo_abun",
    "perc_nat_abun"
  )
  abun_var <- c(
    "total_abundance", "nat_abun", "exo_abun" 
  )

  if (x %in% tps_var) {
    return(get_formula_tps(resp = x, int_env = TRUE))
  } else if (x %in% rich_var) {
    return(get_formula_no_tps(resp = x, int_env = TRUE))
  } else if (x %in% abun_var) {
    return(get_formula_abun(resp = x, int_env = TRUE))
  } else {
    stop("no defined variables")
  }
}

fun_hft_formula <- function(x = NULL,
  int4env = FALSE,
  hft_var = "hft_ix_c9309_log2_ratio") {
  tar_load(c(tps_var, log_rich_var))
  
  if (x %in% tps_var) {
    return(get_formula_tps(resp = x, int_env = int4env, hft = hft_var))
  } else if (x %in% log_rich_var) {
    return(get_formula_no_tps(resp = x, int_env = int4env, hft = hft_var))
  } else if (x == "log_total_abundance") {
    return(get_formula_abun(resp = x, int_env = int4env, hft = hft_var))
  } else {
    stop("no defined variables")
  }
}

model_comp_interp_int <- function(resp =NULL, df = modelling_data) {

  ne <- glmmTMB(formula = as.formula(paste0(resp, "~
        log1_year_nb * riv_str_rc1 +
        log1_year_nb *hft_c9309_scaled_no_center +
        (0 + log1_year_nb | main_bas/siteid)")),
  data = df 
  )

  ne0 <- update(ne,
    formula = as.formula(paste0(resp, "~
        0 + log1_year_nb * riv_str_rc1 +
        log1_year_nb *hft_c9309_scaled_no_center +
        (0 + log1_year_nb | main_bas/siteid)"))
  )

  ne0_no_main <- update(ne,  
    formula = as.formula(paste0(resp, "~
        0 + log1_year_nb / riv_str_rc1 +
        log1_year_nb /hft_c9309_scaled_no_center +
        (0 + log1_year_nb | main_bas/siteid)"))
  )

  list(main = ne, main0 = ne0, no_main0 = ne0_no_main)
}

get_random_effect_df <- function(x = NULL,
  type = NULL,
  resp = c("log_total_abundance", "log_chao_richness", var_jaccard),
  modelling_data = modelling_data
) {

  if (type == "basin") {
    var_loc <- "main_bas"
    s_var_loc <- sym(var_loc)
     var_random <- "random_basin"
     s_var_random <- sym(var_random) 
  } else if (type == "site") {
    var_loc <- "siteid"
    s_var_loc <- sym(var_loc)
     var_random <- "random_site"
     s_var_random <- sym(var_random) 

  } 

  basin_tps_clust <- x %>%
    mutate(err = map_chr({{s_var_random}}, ~class(.x)[1])) %>%
    filter(response %in% resp) %>%
    filter(err != "try-error") %>% 
    unnest({{s_var_random}}) %>%
    select(response, {{s_var_loc}}, log1_year_nb)

  if ("log_total_abundance" %in% basin_tps_clust$response) {
    abun_re_basin_unit_abun <-
      modelling_data %>%
      group_by({{s_var_loc}}) %>%
      summarise(unitabundance = unique(unitabundance)) %>%
      group_by({{s_var_loc}}) %>%
      filter(length(unique(unitabundance)) == 1) %>%
      ungroup() 

    abun_re_basin_unit_abun <- 
      abun_re_basin_unit_abun %>%
      left_join(x[x$response == "log_total_abundance",
        ][[var_random]][[1]],
          by = var_loc 
      )
      trends_unit_correction <- c(
        Count = 0,
        CPUE = unique(abun_re_basin_unit_abun[["log1_year_nb:unitabundanceCPUE"]]),
        Ind.100m2 = unique(abun_re_basin_unit_abun[["log1_year_nb:unitabundanceInd.100m2"]]),
        Leslie_Index = unique(abun_re_basin_unit_abun[["log1_year_nb:unitabundanceLeslie_index"]])
      )
      abun_re_basin_unit_abun <- 
        abun_re_basin_unit_abun %>%
        mutate(
          log1_year_nb = map2_dbl(
            log1_year_nb, unitabundance, 
            ~.x + trends_unit_correction[.y]
          )
          ) %>%
        select({{s_var_loc}}, log1_year_nb)

      basin_tps_clust <- rbind(
        filter(basin_tps_clust, response != "log_total_abundance"),
        mutate(abun_re_basin_unit_abun, response = "log_total_abundance")
      ) 
  }

  basin_tps_clust <- 
    basin_tps_clust %>%
    pivot_wider(names_from = "response", values_from = "log1_year_nb")

  basin_tps_clust_df <- as.data.frame(basin_tps_clust[, -1])
  row.names(basin_tps_clust_df) <- basin_tps_clust[[1]]
  return(basin_tps_clust_df)
}

