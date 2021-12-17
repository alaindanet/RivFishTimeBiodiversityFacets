get_turnover <- function(x = NULL, type = "total") {

  x <- x %>%
    nest_by(siteid)

  x$turnover <- map(x$data,
    ~try(turnover(
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

  new_var <- paste0(type, "_by_year")

   x <- x %>%
    group_by(siteid) %>%
    mutate(
      !!sym(new_var) := !!sym(type) / (year - dplyr::lag(year, order_by = year)),
      check_1st_year_na = if_else(is.na(!!sym(new_var)), year == min(year), NA),
      !!sym(new_var) := if_else(is.na(!!sym(new_var)), !!sym(type) / (year - first_year), !!sym(new_var)),
    ) %>%
    ungroup()
  # Check my assumption of the second if_else
  stopifnot(all(na.omit(x$check_1st_year_na) == TRUE))

  x <- x %>%
    select(-check_1st_year_na, -first_year)

  return(x)
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
    nest_by(siteid)

  # By site, sort increasing year and build t1 and t2 
  output <- map(x$data, function(site) {

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
