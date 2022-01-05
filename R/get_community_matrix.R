#' Get community matrix by site
get_site_community_matrix <- function(x = NULL) {

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
      mutate(mat = list(matrix_row_names(mat, "year")))

    # NA to 0
    mat_com  <- mat_com %>%
      ungroup() %>%
      mutate(
        mat = map(
          mat, function (x) {
            x[is.na(x)] <- 0
            return(x)
          }
        )
      ) %>%
      select(siteid, mat)

    # Relative abundaces 
    mat_com$mat_rel <- map(mat_com$mat, ~ .x / rowSums(.x))
    check_mat <- map_lgl(mat_com$mat_rel,
      ~all(round(rowSums(.x), 5) == 1))
    stopifnot(all(check_mat))

    return(mat_com)

}
