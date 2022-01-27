# Format water_temperature
format_water_temperature <- function(
  wt = NULL,
  siteid = filtered_dataset$location$siteid,
  raster_path = NULL) {
  
  x <- terra::rast(x = R.utils::filePath(water_temperature_file, expandLinks = "any"))
  
  # Add site id, put in long format
  wt <- wt %>%
    mutate(
      siteid = siteid,
      tmp = kelvin_to_celcius(tmp)
    ) %>%
    pivot_longer(cols = -siteid, names_to = "col", values_to = "tmp")
  
  time <- tibble(col = seq_len(length(time(x))), date = time(x))
  
  
  # Add date
  wt <- wt %>%
    mutate(col = as.integer(str_extract(col, "\\d"))) %>%
    left_join(time, by = "col")
  
  out <- wt %>%
    select(siteid, date, tmp)
  
  return(out)
  
}

#' kelvin to celcius
#'
#' @examples 
#' kelvin_to_celcius(273.15)
kelvin_to_celcius <- function(x = NULL) {
  x - 273.15
}
