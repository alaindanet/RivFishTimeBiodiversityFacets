# Borrow the source of iNEXT bc the default functions are too long run
source("https://raw.githubusercontent.com/JohnsonHsieh/iNEXT/master/R/invChat.R")
source("https://raw.githubusercontent.com/JohnsonHsieh/iNEXT/master/R/iNEXT.r")
invChat.Ind_c <- compiler::cmpfun(invChat.Ind)

#' Compute chao hillnumber coverage style
#'
#' @examples
#'
get_chao_hillnb <- function(
  x = NULL,
  coverage = NULL,
  confidence_int = NULL,
  adjust_abun_density = TRUE
  ) {

  # Adjust abundance to get a minimum of one individual where minimal density is
  # below one:
  if (adjust_abun_density) {
    x <- x %>%
      group_by(op_id) %>%
      mutate(abundance = round(adjust_abun_chao(abundance))) %>%
      ungroup()
  }

  x <- x %>%
    nest_by(op_id)

  # Take the abundance of species in each operation
  chao_data <- purrr::map(x$data, "abundance")
  names(chao_data) <- x$op_id

  chao <- furrr::future_map_dfr(
    chao_data,
    ~invChat.Ind_c(.x, C = coverage, conf = confidence_int),
    .id = "op_id"
  )

  colnames(chao)[colnames(chao) %in% c("q = 0", "q = 1", "q = 2")] <-
    c("chao_richness", "chao_shannon", "chao_simpson")

  # Sanatizer

  ## na simpson bc of species richness = 0:
  stopifnot(all(chao[is.na(chao$chao_simpson), ]$chao_richness == 1))
  ### Simpson = 0
  if (any(is.na(chao$chao_simpson))) {
    chao[is.na(chao$chao_simpson), ]$chao_simpson <- 0
  }

  ## na shannon bc all abundances are equals
  ### abundance
  abun_equal <- furrr::future_map_dfr(
    chao_data,
    ~c(n_unique = length(unique(.x))),
    .id = "op_id"
  )

  stopifnot(
    all(abun_equal[is.na(chao$chao_shannon), ]$n_unique == 1)
  )
#When all abundances are equal and since hill numbers are computed with the
  #natural logarythm, shannon should be equal to ln($D^0$)
  chao[is.na(chao$chao_shannon), ]$chao_shannon <- log(
    chao[is.na(chao$chao_shannon), ]$chao_richness
  )
  # End sanatizer

  chao$chao_evenness <- chao$chao_shannon / log(chao$chao_richness)

  return(tibble::as_tibble(chao))
}

#' Ajust some abundance for chao computation 
adjust_abun_chao <- function(x = NULL) {
  m <- min(x)
  if (m < 1) {
    x <- x * (1 / m)
  } else {
    return(x)
  }
}

#' Get hillnumber from vegan package
get_hillnb <- function(x = NULL, dataset = NULL) {

  mat_com <- x %>%
    select(siteid, year, species, abundance) %>%
    nest_by(siteid) %>%
    mutate(mat = list(
        pivot_wider(data,
          names_from = "species",
          values_from = "abundance") %>%
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
      mutate(mat0 = map(mat, function (x) {
          x[is.na(x)] <- 0
          return(x)
    }
    )
      )

      div_vector <- mat_com %>%
        mutate(
          species_nb = map(mat0, ~specnumber(.x)),
          shannon = map(mat0, ~diversity(.x)),
          simpson = map(mat0, ~diversity(.x, "simpson")),
          inv_simpson = map(mat0, ~diversity(.x, "invsimpson")),
          evenness = map2(shannon, species_nb, ~ .x / log(.y))
          ) %>%
      select(siteid, species_nb, shannon, simpson, inv_simpson, evenness)

    div_df <- div_vector %>%
      pivot_longer(cols = !siteid,
        names_to = "variable",
        values_to = "diversity"
        ) %>%
    mutate(diversity = map2(variable, diversity,
        ~enframe(.y, name = "year", value = .x))
      ) %>%
    pivot_wider(names_from = "variable", values_from = "diversity")

  div_df  <- div_df %>%
    mutate(merged = pmap(list(species_nb, shannon, simpson, inv_simpson, evenness), function(a, b, c, d, e) {
        Reduce(function(...) merge(..., by='year', all.x=TRUE), list(a,b,c, d, e))
        }))

  div <- div_df %>%
    select(siteid, merged) %>%
    unnest(cols = merged) %>%
    mutate(year = as.numeric(year)) %>%
    left_join(select(dataset, op_id, siteid, year),
      by = c("siteid", "year"))

    return(div)
}

