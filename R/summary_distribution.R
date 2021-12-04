#' Get unique values
#'
#' @param x  numeric vector.
#' @param na.omit logical.
#' @return vector of length 1 containing the unique value of the vector or the
#' character "no_unique".
#' @examples
#' x <- rep(list(c(runif(1e3), rep(NA, 1000))), 100)
#' get_unique_values(x)
#' get_unique_values(x, na.omit = FALSE)
get_unique_values <- function(x = NULL, na.omit = TRUE) {

  if (na.omit) {
    x <- na.omit(x)
  }

  res <- unique(x)

  if (length(res) <= 1) {
    res
  } else {
    "no_unique"
  }
}
get_unique_values_c <- compiler::cmpfun(get_unique_values)

#' Get the summary of a numerical vector
#'
#' compute the "min", "1st_quart", "median", "2nd_quart", "max", "n", number of
#' na and na fraction.
#'
#' @param x numerical vector.
#' @param na.rm logical.
#' @return a named vector.
#' @examples
#' x <- c(runif(100), NA)
#' summary_distribution(x)
#' summary_distribution(x, na.rm = TRUE)
summary_distribution <-
  function(x = NULL, na.rm = FALSE) {

    quant <- quantile(x, probs = c(0, .25, .5, .75, 1), na.rm = na.rm)
    names(quant) <- c("min", "1st_quart", "median", "2nd_quart", "max")

    other_desc <- c(
      mean = mean(x, na.rm = na.rm),
      sd = sd(x, na.rm = na.rm),
      n = length(x),
      n_na = length(x[is.na(x)]),
      frac_na = length(x[is.na(x)]) / length(x)
    )

    output <- c(quant, other_desc)
    return(output)
}
