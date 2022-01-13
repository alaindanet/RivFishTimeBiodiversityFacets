#' Get community matrix by site
get_site_community_matrix <- function(
  x = NULL,
  average_first_year = FALSE,
  nb_sampling_to_average = NULL) {

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
      mat_com$mat <- purrr::map(mat_com$mat,
        ~average_first_years_com_mat(
          mat = .x,
          nb_sampling_to_average = nb_sampling_to_average
        )
      )
    }

    # Relative abundaces
    mat_com$mat_rel <- purrr::map(mat_com$mat, ~ .x / rowSums(.x))
    mat_com$mat_bin <- purrr::map(mat_com$mat,
      function (x) {
        x[x > 0] <- 1
        return(x)
      }
    )
    check_mat <- map_lgl(mat_com$mat_rel,
      ~all(round(rowSums(.x), 5) == 1))
    stopifnot(all(check_mat))

    return(mat_com)

}

#' Average abundance of first year of community matrix
average_first_years_com_mat <- function(
  mat = NULL,
  nb_sampling_to_average = NULL) {

  avgfirst_year <- colMeans(mat[1:nb_sampling_to_average, , drop = FALSE])
  new_first_year <- mean(as.numeric(row.names(mat[1:nb_sampling_to_average, ])))

  new_mat <- mat[nb_sampling_to_average:nrow(mat), , drop = FALSE]
  new_mat[1, ] <- avgfirst_year
  row.names(new_mat)[1] <- new_first_year

  return(new_mat)
}

#' Get community matrix by basin and year 
get_basin_year_community_matrix <- function(
  x = NULL,
  loc = filtered_dataset$loc,
  min_nb_site_by_basin = NULL,
  min_year_by_basin = NULL
  ) {

  mat_com <- x %>%
    left_join(
      loc %>%
        select(siteid, main_bas),
      by = "siteid"
    ) %>%
    select(siteid, year, species, abundance, main_bas) %>%
    nest_by(main_bas, year) %>%
    mutate(
      mat = list(
        pivot_wider(
          data,
          names_from = "species",
          values_from = "abundance"
          ) %>%
        arrange(siteid)
      )
    )

    # Make matrices
    mat_com <- mat_com %>%
      mutate(mat = list(matrix_row_names(mat, "siteid"))) %>%
      ungroup()

    if (!is.null(min_nb_site_by_basin)) {
      nb_site_by_basin <- purrr::map_int(mat_com$mat, nrow)
      mask <- nb_site_by_basin >= min_nb_site_by_basin

      mat_com <- mat_com[mask, ]
    }

    if (!is.null(min_year_by_basin)) {
      nb_year_by_basin <- table(mat_com$main_bas)
      mask_basin <- nb_year_by_basin >= min_year_by_basin

      mat_com <- mat_com %>%
        filter(main_bas %in% names(nb_year_by_basin)[mask_basin])
    }


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
    select(main_bas, year, mat)

    # Relative abundaces
    mat_com$mat_rel <- purrr::map(mat_com$mat, ~ .x / rowSums(.x))
    mat_com$mat_bin <- purrr::map(mat_com$mat,
      function (x) {
        x[x > 0] <- 1
        return(x)
      }
    )
    check_mat <- map_lgl(mat_com$mat_rel,
      ~all(round(rowSums(.x), 5) == 1))
    stopifnot(all(check_mat))

    return(mat_com)


}

matrix_row_names <- function(x = NULL, var_row_names = NULL) {
  y <- x[, !colnames(x) %in% var_row_names]
  out <- as.matrix(y)
  rownames(out) <- x[[var_row_names]]
  return(out)
}

select_rownames_in_matrix_list <- function(
  mat_list = NULL,
  min_nb_sampled = 10
  ){

  site_nb_sampling <- map(mat_list, row.names) %>%
    unlist() %>%
    table
  site_to_keep <- names(site_nb_sampling)[site_nb_sampling >= min_nb_sampled]

  common_site <- Reduce(intersect,
    map(mat_list, ~row.names(.x)[row.names(.x) %in% site_to_keep]
    )
)
  return(common_site)

}

# Put all the species in the data:
get_total_species_in_matrix_list <- function(mat_list = NULL) {
  unique(purrr::map(mat_list, colnames) %>% unlist)
}

filter_complete_com_matrix <- function(
  mat = NULL,
  site_to_keep = NULL,
  common_species = NULL
) {

  if (length(site_to_keep) == 0) {
    return(NA)
  }


  mat <- mat[row.names(mat) %in% site_to_keep, , drop = FALSE]


  # Add species if missing
  missing_species <- common_species[!common_species %in% colnames(mat)]

  if (length(missing_species) != 0) {
    ## Make 0 columns
    zero_col_vector <- rep(0, length(missing_species) * nrow(mat))
    zero_col_matrix <- matrix(
      zero_col_vector,
      ncol = length(missing_species),
      byrow = FALSE,
      dimnames = list(
        NULL,
        missing_species
      )
    )
    ##Add to mat
    mat <- cbind(mat, zero_col_matrix)
  }

  return(mat)

}

target_filter_com_mat_basin <- function(
  com_mat_basin = NULL,
  min_nb_sampling_by_site = 10,
  min_nb_site_by_basin = 10,
  clean = TRUE
  ) {

  output <- com_mat_basin %>%
    select(main_bas, year, mat) %>%
    nest_by(main_bas) %>%
    mutate(
      site_to_keep = list(
        select_rownames_in_matrix_list(
          data[["mat"]],
          min_nb_sampled = min_nb_sampling_by_site
        )
        ),
      nb_site = length(site_to_keep),
      total_species = list(get_total_species_in_matrix_list(data$mat)),
      nb_species = length(total_species)
      ) %>%
    unnest(cols = data) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(mat = list(
        filter_complete_com_matrix(
          mat = mat,
          site_to_keep = site_to_keep,
          common_species = total_species)
      )
      ) %>%
    filter(nb_site >= min_nb_site_by_basin) %>%
    ungroup() 

  if (clean) {
    output <- output %>%
      select(main_bas, year, mat)
  }

  return(output)

}
