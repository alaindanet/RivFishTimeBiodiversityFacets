# Format water_temperature
format_water_temperature <- function(
  wt = NULL,
  siteid = filtered_dataset$location$siteid,
  raster_path = NULL) {
  
  x <- terra::rast(x = R.utils::filePath(raster_path, expandLinks = "any"))
  
  # Add site id, put in long format
  wt <- wt %>%
    mutate(
      siteid = siteid
    ) %>%
    pivot_longer(cols = -siteid, names_to = "col", values_to = "tmp") %>%
    mutate(tmp = kelvin_to_celcius(tmp)) %>%
    mutate(col = as.integer(str_extract(col, "\\d+")))

  time <- tibble(col = seq_len(length(time(x))), date = time(x))

  # Add date
  wt <- wt %>%
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

get_moving_average_tmp <- function(
  wt = NULL,
  avg_by_site = FALSE) {

  wt_y_mw <- wt %>%
    nest_by(siteid) %>%
    mutate(
      mw_tmp = list(
        slide_period_dfr(
          .x = data,
          .i = data$date,
          .period = "year",
          .f = ~data.frame(
            date = mean(.x$date),
            mw_tmp = mean(.x$tmp)
            ),
          #        .f = function(x) mean(x),
          .before = 6,
          .after = 6,
          .complete = TRUE
        )
      )
    )

    out <- wt_y_mw %>%
      select(siteid, mw_tmp) %>%
      unnest(mw_tmp)

    if (avg_by_site) {
      out <- out %>%
        summarise(tmp_w_ama = mean(mw_tmp))
    } else {
      out <- out %>%
        ungroup() %>%
        mutate(year = year(date)) %>%
        group_by(siteid, year) %>%
        summarise(tmp_w_ama = mean(mw_tmp))
    }

    return(out)
}
