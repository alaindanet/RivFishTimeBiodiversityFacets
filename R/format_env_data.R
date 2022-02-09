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
filter_water_temperature <- function(
  wt = NULL,
  raw_tmp_threshold = 40, 
  nb_sd_threshold = 5
  ) {

  limit_abberant <- wt %>%
    filter(tmp <= raw_tmp_threshold) %>%
    mutate(month = month(date)) %>%
    nest_by(siteid, month) %>%
    summarise(
      mean_tmp = mean(data$tmp, na.rm = T),
      sd_tmp = sd(data$tmp, na.rm = T), 
      .groups = "drop"
    )

    wt_check <- wt %>%
      mutate(month = month(date)) %>%
      left_join(limit_abberant, by = c("siteid", "month")) %>%
      filter(!is.na(sd_tmp)) %>%
      filter(tmp <= raw_tmp_threshold) %>%
      mutate(
        check_tmp = abs(tmp - mean_tmp) >=
          nb_sd_threshold * sd_tmp
      )

      out <- wt_check %>%
        filter(!check_tmp) %>%
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
          .complete = FALSE
        )
      )
    )

    out <- wt_y_mw %>%
      select(siteid, mw_tmp) %>%
      unnest(mw_tmp)

    if (avg_by_site) {
      out <- out %>%
        group_by(siteid) %>%
        summarise(tmp_w_ama = mean(mw_tmp))
    } else {
      out <- out %>%
        ungroup() %>%
        mutate(year = year(date)) %>%
        group_by(siteid, year) %>%
        summarise(tmp_w_ama = mean(mw_tmp), .groups = "drop")
    }

    return(out)
}

format_air_temperature <- function(x = air_temperature) {
  
  colnames(x)[!colnames(x) %in% "siteid"] <- colnames(x)[!colnames(x) %in% "siteid"] %>% 
    str_extract(., "\\d{2}_\\d{4}") %>%
    str_replace(., "_", "-") %>%
    paste0("15-",.)
  
  x %>%
    pivot_longer(cols = -siteid, names_to = "date", values_to = "tmp") %>%
    mutate(date = dmy(date),
           tmp_c = kelvin_to_celcius(tmp / 10)
    ) %>%
    select(-tmp)
  
}