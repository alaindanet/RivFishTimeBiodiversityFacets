#' Load and clean Lise database
#'
load_time_series_data <- function(path) {

  output <- read_csv(path) %>%
    select(-...1) #Remove dummy variables

  # Stop if parsing problems
  readr::stop_for_problems(output)

  # Clean column names
  colnames(output) <- tolower(colnames(output))

  # Correct the encoding (necessary for mapView)
  col_chr <- purrr::map_lgl(output, is.character)
  output[, col_chr] <- map(output[, col_chr],
    ~stringi::stri_conv(.x, from = "ISO-8859-1", to = "UTF-8"))

  return(output)
}
