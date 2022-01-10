#' Get community matrix by site
get_site_community_matrix <- function(
  x = NULL,
  average_first_year = FALSE,
  nb_year_to_average = NULL) {

  mat_com <- x %>%
    select(siteid, year, species, abundance) %>%
    nest_by(siteid) %>%
    mutate(
      mat = list(
        pivot_wider(
          data,
          names_from = "species",
          values_from = "abundance"
          ) %>%
        arrange(year)
      )
    )

    # Make matrices
    matrix_row_names <- function(x = NULL, var_row_names = NULL) {
      y <- x[, !colnames(x) %in% var_row_names]
      out <- as.matrix(y)
      rownames(out) <- x[[var_row_names]]
      return(out)
    }
    mat_com <- mat_com %>%
      mutate(mat = list(matrix_row_names(mat, "year"))) %>%
      ungroup()

    # NA to 0
    mat_com  <- mat_com  %>%
      mutate(
        mat = purrr::map(
          mat, function(x) {
            x[is.na(x)] <- 0
            return(x)
          }
        )
        ) %>%
    select(siteid, mat)

    if (average_first_year) {
      mat_com  <- mat_com  %>%
        mutate(
          mat = purrr::map(mat,
            ~average_first_years_com_mat(
              mat = .x,
              nb_year_to_average = nb_year_to_average
            )
          )
        )
    }

    # Relative abundaces
    mat_com$mat_rel <- purrr::map(mat_com$mat, ~ .x / rowSums(.x))
    check_mat <- map_lgl(mat_com$mat_rel,
      ~all(round(rowSums(.x), 5) == 1))
    stopifnot(all(check_mat))

    return(mat_com)

}

#' Average abundance of first year of community matrix
average_first_years_com_mat <- function(
  mat = NULL,
  nb_year_to_average = NULL) {

  avgfirst_year <- colMeans(mat[1:nb_year_to_average, ])
  new_first_year <- mean(as.numeric(row.names(mat[1:nb_year_to_average, ])))

  new_mat <- mat[nb_year_to_average:nrow(mat), ]
  new_mat[1, ] <- avgfirst_year
  row.names(new_mat)[1] <- new_first_year

  return(new_mat)
}
