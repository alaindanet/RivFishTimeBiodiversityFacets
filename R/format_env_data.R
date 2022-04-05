
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
    select(siteid, date, tmp) %>%
    mutate(date = lubridate::as_date(date))

  return(out)

}
filter_water_temperature <- function(
  wt = NULL,
  raw_tmp_threshold = 40,
  nb_sd_threshold = 5,
  na_omit = TRUE
  ) {

  limit_abberant <- wt %>%
    mutate(month = month(date)) %>%
    nest_by(siteid, month) %>%
    summarise(
      mean_tmp = mean(data$tmp, na.rm = T),
      sd_tmp = sd(data$tmp, na.rm = T),
      .groups = "drop")

  wt_check <- wt %>%
    mutate(month = month(date)) %>%
    left_join(limit_abberant, by = c("siteid", "month")) %>%
    mutate(
      check_tmp = abs(tmp - mean_tmp) >=
        nb_sd_threshold * sd_tmp
    )

    if (na_omit) {
      out <- wt_check %>%
        filter(!is.na(sd_tmp)) %>%
        filter(tmp <= raw_tmp_threshold) %>%
        filter(!check_tmp) %>%
        select(siteid, date, tmp)
    } else {
      out <- wt_check %>%
        mutate(tmp_corrected = ifelse(
            is.na(tmp) | is.na(sd_tmp) | tmp > raw_tmp_threshold | check_tmp,
            NA, tmp
            ))
    }

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
  avg_by_site = FALSE,
  var_y = "tmp",
  output_tmp_var = "tmp_w_ama",
  max_prop_na = 1 / 3,
  complete = TRUE
  ) {

  sym_output_var <- sym(output_tmp_var)

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
            mw_tmp = ifelse(
              sum(is.na(.x[[var_y]])) / length(.x[[var_y]]) < max_prop_na,
              mean(.x[[var_y]], na.rm = TRUE),
              NA
              )
            ),
          #        .f = function(x) mean(x),
          .before = 6,
          .after = 6,
          .complete = complete
        )
      )
    )
    out <- wt_y_mw %>%
      select(siteid, mw_tmp) %>%
      unnest(mw_tmp)

    if (avg_by_site) {
      out <- out %>%
        group_by(siteid) %>%
        summarise(!!sym_output_var := mean(mw_tmp))
    } else {
      out <- out %>%
        ungroup() %>%
        mutate(year = year(date)) %>%
        group_by(siteid, year) %>%
        summarise(!!sym_output_var := mean(mw_tmp), .groups = "drop")
    }

    return(out)
}

get_mv_avg_rollapplyr <- function(
  wt = NULL,
  var_y = "tmp") {

  complete_site_date <- expand.grid(list(
      siteid = unique(wt$siteid),
      date = unique(wt$date)
    )) %>%
  as_tibble() %>%
    left_join(wt, by = c("siteid", "date"))

output <- complete_site_date %>%
  group_by(siteid) %>%
  nest() %>%
  mutate(
    data = map(data,
      function(x) {
        x$mv_avg_12m <- zoo::rollapplyr(
          data = zoo::zoo(x[[var_y]], x$date),
          width = 12, FUN = mean, fill = NA,
          partial = 9, align = "center"
        )

        x$mv_avg_3m <- zoo::rollapplyr(
          data = zoo::zoo(x[[var_y]], x$date),
          width = 3, FUN = mean, fill = NA,
          partial = 2, align = "center"
        )
        return(x)
      })
    ) %>%
  unnest(data)

return(output)

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
    select(-tmp) %>%
    arrange(date)
  
}
