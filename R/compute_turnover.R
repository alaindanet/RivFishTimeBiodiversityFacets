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
