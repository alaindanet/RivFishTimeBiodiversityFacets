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

  return(as_tibble(chao))
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
