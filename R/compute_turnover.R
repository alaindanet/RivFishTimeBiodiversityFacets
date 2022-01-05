get_turnover <- function(x = NULL, type = "total") {

  x <- x %>%
    nest_by(siteid)

  x$turnover <- furrr::future_map(x$data,
    ~try(codyn::turnover(
          df = .x,
          time.var = "year",
          species.var = "species",
          abundance.var = "abundance",
          replicate.var = NA,
          metric = type
          )))

  x <- x %>%
    mutate(first_year = min(data$year)) %>%
    ungroup() %>%
    select(-data) %>%
    unnest(cols = c(turnover))

  output <- variable_by_year(x = x, variable = type)

  return(output)
}

#' Wrapper for targets
#'
#'
#'
get_hillebrand_turnover <- function(x = NULL)  {

  x <- x %>%
    group_by(op_id) %>%
    mutate(rel_abundance = abundance / sum(abundance)) %>%
    ungroup()

  x  <- x %>%
    nest_by(siteid) %>%
    mutate(first_year = min(data$year))

  # By site, sort increasing year and build t1 and t2
  output <- furrr::future_map(x$data, function(site) {

    site <- site %>%
      arrange(year) %>%
      nest_by(year)

    t1 <- site[-nrow(site), ]
    t2 <- site[-1, ]

    output <- purrr::map2_dbl(t1$data, t2$data,
      compute_abundance_turnover_two_timesteps)
    names(output) <- t2$year

    output <- enframe(output, name = "year", value = "hillebrand")

    return(output)
    })

    x$year <- names(output)
    x$hillebrand <- output

    out <- x %>%
      ungroup() %>%
      select(-data) %>%
      unnest(cols = c(hillebrand))

    out <- variable_by_year(x = out, variable = "hillebrand")

    return(out)

}

#' compute_abundance_turnover_two_timesteps
#'
#'
#' Based on hillebrand et al. (2018)
#'
#' @examples
#' nsp <- 3
#' p1 <- tibble(species = letters[1:nsp], abundance = c(0, rep(1, nsp - 1)),
#'   rel_abundance = abundance / sum(abundance))
#' p2 <- p1
#' compute_abundance_turnover_two_timesteps(p1, p2)
#' # 0
#' p2$abundance[1] <- 1
#' p2$rel_abundance <- p2$abundance / sum(p2$abundance)
#' compute_abundance_turnover_two_timesteps(p1, p2)
#' 
#' p2$rel_abundance <- c(.5, .5, 0) 
#' compute_abundance_turnover_two_timesteps(p1, p2)
compute_abundance_turnover_two_timesteps <- function(t1 = NULL, t2 = NULL) {

  var_sp_abun <- c("species", "rel_abundance")

  t1 <- dplyr::rename(t1[, var_sp_abun], p1 = rel_abundance)
  t2 <- dplyr::rename(t2[, var_sp_abun], p2 = rel_abundance)

  p <- dplyr::full_join(t1, t2, by = "species")
  p[is.na(p)] <- 0

  numerator <- sum((p$p1 - p$p2)^2)
  denominator <- sum(p$p1^2) + sum(p$p2^2) - sum(p$p1 * p$p2)

  ser <- numerator / denominator

  return(ser)
}

#' Compute by year variable (used for turnover)
#'
variable_by_year <- function(x = NULL, variable = NULL) {

  var_sym <- variable
  new_var <- paste0(variable, "_by_year")

  if (!is.numeric(x$year)) {
    x$year <- as.numeric(x$year)
  }

  x <- x  %>%
    group_by(siteid) %>%
    arrange(siteid, year) %>%
    mutate(
      !!sym(new_var) := !!sym(var_sym) / (year - dplyr::lag(year, order_by
          = year)),
      check_1st_year_na = if_else(is.na(!!sym(new_var)), year == min(year), NA),
      !!sym(new_var) := if_else(
        is.na(!!sym(new_var)),
        !!sym(var_sym) / (year - first_year),
        !!sym(new_var)
      )
      ) %>%
    ungroup()
  # Check my assumption of the second if_else
  stopifnot(all(na.omit(x$check_1st_year_na) == TRUE))

  x <- x %>%
    select(-check_1st_year_na, -first_year)

  return(x)
}

#' From vegdist object to temporal diversity
#'
#' From vegdist object to similarity index from the reference year to the other
#'
get_temporal_vegdist <- function(vegdist_obj = NULL, drop_first_year = TRUE) {

  x <- as.matrix(vegdist_obj)

  min_year <- as.character(min(as.numeric(colnames(x))))
  out <- x[, min_year]

  if (drop_first_year) {
   out <- out[names(out) != min_year]
  }

  return(out)
}

#' Get temporal turnover from first year with vegdist
#'
#' Wrapper around vegdist
#'
#' @examples
#' tar_load(com_mat_site)
#' get_vegdist_temporal_turnover(com_mat_site$mat[[1]], method = "jaccard")
get_vegdist_temporal_turnover <- function(
  mat = NULL,
  method = NULL,
  return_tibble = TRUE,
  drop_first_year = TRUE
  ) {

  dist_obj <- vegan::vegdist(mat, method = method)

  dist_to_reference_year <- get_temporal_vegdist(
    vegdist_obj = dist_obj,
    drop_first_year = drop_first_year
  )


  if (return_tibble) {
    dist_to_reference_year <-
      enframe(dist_to_reference_year, name = "year", value = method)
    dist_to_reference_year$year <- as.numeric(dist_to_reference_year$year)
  }

  return(dist_to_reference_year)

}


#' Target function for vegdist turnover
#'
#'
target_vegdist_turnover <- function(
  dataset = NULL,
  method = NULL,
  return_tibble = TRUE,
  drop_first_year = TRUE
  ) {

  dataset[[method]] <- furrr::future_map(
    dataset[["mat"]],
    get_vegdist_temporal_turnover,
    method = method,
    return_tibble = return_tibble,
    drop_first_year = drop_first_year
  )

  dataset[, c("siteid", method)] %>%
    unnest(cols = !!sym(method))

}


compute_dist <- function(mat = NULL, fun = NULL, ...) {

  dist_mat <- matrix(NA,
    nrow = nrow(mat),
    ncol = nrow(mat),
    dimnames = list(row.names(mat), row.names(mat))
  )

  for (i in seq_len(nrow(dist_mat))) {
    for (j in seq_len(nrow(dist_mat))) {
      dist_mat[i, j] <- fun(x = mat[i, ], y = mat[j, ], ...)
    }
  }
  return(dist_mat)
}
compute_hillebrand <- function(x = NULL, y = NULL) {
  numerator <- sum((x - y)^2)
  denominator <- sum(x^2) + sum(y^2) - sum(x * y)

  numerator / denominator
}
compute_codyn_turnover <- function(x = NULL, y = NULL, type = NULL) {

  x_sp <- names(x)[x > 0]
  y_sp <- names(y)[y > 0]

  appearance <- sum(!y_sp %in% x_sp)
  disappearance <- sum(!x_sp %in% y_sp)
  nb_sp <- length(x)
  stopifnot(length(x) == length(y))

  if (type == "total") {
    (appearance + disappearance) / nb_sp
  } else if (type == "appearance") {
    appearance / nb_sp
  } else if (type == "disappearance") {
    disappearance / nb_sp
  } else {
    stop("wrong type supplied")
  }
}

get_custom_temporal_turnover <- function(
  mat = NULL,
  fun = NULL,
  var_name = "value",
  return_tibble = TRUE,
  drop_first_year = TRUE,
  ...
  ) {

  dist_obj <- compute_dist(mat = mat, fun = fun, ...)

  dist_to_reference_year <- get_temporal_vegdist(
    vegdist_obj = dist_obj,
    drop_first_year = drop_first_year
  )


  if (return_tibble) {
    dist_to_reference_year <-
      enframe(dist_to_reference_year, name = "year", value = var_name)
    dist_to_reference_year$year <- as.numeric(dist_to_reference_year$year)
  }
  return(dist_to_reference_year)
}
target_custom_temporal_turnover <- function(
  dataset = NULL,
  fun = NULL,
  var_name = NULL,
  return_tibble = TRUE,
  drop_first_year = TRUE,
  ...
  ) {

  dataset$value <- furrr::future_map(
    dataset$mat_rel, get_custom_temporal_turnover,
      fun = fun,
      var_name = var_name,
      return_tibble = return_tibble,
      drop_first_year = drop_first_year, ...)

  dataset[, c("siteid", "value")] %>%
    unnest(cols = value)

}
