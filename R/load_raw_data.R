#' Load and clean Lise database
#'
load_time_series_data <- function(path) {

  output <- read_csv(path) %>%
    select(-...1) #Remove dummy variables

  # Stop if parsing problems
  readr::stop_for_problems(output)

  # Clean column names
  colnames(output) <- tolower(colnames(output))

  return(output)
}

