
get_month_difference <- function(x = NULL, med_x = NULL) {

    if (is.na(x) | is.na(med_x) | !is.Date(x) | !is.Date(med_x)) {
        return(NA)
    }
    lubridate::interval(x, med_x)
}

date_interval_to_duration <- function (x = NULL) {
    if (is.na(x)) {return(NA)}
    x %>%
        as.duration %>%
        as.numeric(., "month") %>%
        abs(.)
}

#' Filter operation based on their month
#'
#' 
#' @examples 
#' month_filter(x = "2020-01-01", med_x = "2002-01-01", range_x = 1)
#' month_filter(x = "2020-02-01", med_x = "2002-01-01", range_x = 1)
#' month_filter(x = "2020-12-03", med_x = "2002-01-01", range_x = 1)
#' month_filter(x = "2020-11-30", med_x = "2002-01-01", range_x = 1)
#' # FALSE
#' month_filter(x = "2020-11-28", med_x = "2002-01-01", range_x = 1)
#' month_filter(x = "2020-02-15", med_x = "2002-01-01", range_x = 1)
#' #NA
#' month_filter(x = NA, med_x = "2002-01-01", range_x = 1)
#' month_filter(x = "2020-02-30", med_x = "2002-01-01", range_x = 1)
#' month_filter(x = 1, med_x = "2002-01-01", range_x = 1)
#' month_filter(x = "2020-02-30", med_x = 1, range_x = 1)
month_filter <- function(x = NULL, range_x = NULL,
    month_int_decimal = 1) {

    # delete the year: take the remainder of the division by 12 month
    month_interval <- x %% 12 %>% round(., month_int_decimal)

    # Month number between two dates should be between 12 - range <= x < 12 or 0
    # <= x < range 
    check <-
        # date before median:
        (month_interval >= 12 - range_x & month_interval <= 12) |
        # date after median: 
        (month_interval <= range_x & month_interval >= 0)

    return(check)
}
month_filter_c <- compiler::cmpfun(month_filter)

#' Take the sampling being the closest to the other
#'
#'
#' @examples
#'choose_from_multiple(x = NULL, quarter = c(1,1,2), mode_quarter = 2)
#'# 3
#'choose_from_multiple(x = c(1,3,NA), quarter = c(1,1,2), mode_quarter = 2)
#'choose_from_multiple(x = c(3,1,NA), quarter = c(1,1,2), mode_quarter = 2)
#'choose_from_multiple(x = c(NA,NA,NA), quarter = c(1,1,2), mode_quarter = 2)
#'choose_from_multiple(x = c(NA,NA,NA), quarter = c(1,1,2), mode_quarter = NA)
#'#NA
#'choose_from_multiple(x = c(NA,NA,NA), quarter = c(1,NA,2), mode_quarter = 2)
#'choose_from_multiple( x = c(1, 12.1))
#'choose_from_multiple(x = c(NA, 12.1))
#'choose_from_multiple(x = c(.09, 12.1))
choose_from_multiple <- function (x = NULL, quarter = NULL, mode_quarter = NULL) {

  if (length(x) <= 1 & length(quarter) <= 1) {
    return(as.integer(1))
  }

  if (any(!is.na(x)) & all(!is.null(x))) {
    nb_month <- x %% 12
    output <- which.min(
      c(abs(nb_month), abs(nb_month - 12))
      )

    if (output > length(nb_month)) {
      return(output - length(nb_month))
    } else {

    return(output)
    }

  }

  if (any(!is.null(quarter)) & any(!is.na(quarter)) & !is.null(mode_quarter) &
    !is.na(mode_quarter)) {
    to_compare <- unique(mode_quarter)

    stopifnot(length(to_compare) == 1)

    which(quarter == to_compare)

  } else {
    return(NA)
  }

}

quarter_filter <- function(quar = NULL, mode_quar = NULL) {
    quar == mode_quar
}

#' Selection the sampling event to keep
#'
#' we selected:
#'   - the sampling that took place close to the median month of each site
#' (extent_month parameter)
#'   - only one sampling by site and by year, the one whose date was the closest
#' from median of the site
#'   - sites that have been monitored at least nb_sampling times
#'   - some protocol and abundance unit (selected_protocol, selected_abun_unit)
#'
#' When precise date is absent but month and year are present, we can replace
#' missing date by "%year-%month-15" (convert_month_to_date).
#'
#' NA sampling protocol and unit of abundance are removed
filter_op <- function(
    op_protocol = NULL,
    selected_protocol = NULL,
    selected_abun_unit = NULL,
    nb_sampling = NULL,
    span_min = 10,
    extent_month = 1,
    convert_month_to_date = FALSE,
    return_no_filtered = FALSE
    ) {

  if (convert_month_to_date) {
    # Convert NA date which have a month and year
    mask_na_date_year_month <- is.na(op_protocol$date) &
      !is.na(op_protocol$year) & !is.na(op_protocol$month)
    op_protocol[mask_na_date_year_month, ]$date <- paste0(
      op_protocol[mask_na_date_year_month, ]$year,
      "-",
      op_protocol[mask_na_date_year_month, ]$month,
      "-",
      "15"
      ) %>% ymd(.)
  }


  # Sanitizer
  op_protocol$quarter <- as.integer(op_protocol$quarter)

  # Remove sampling without month or quarter
  mask_month_quarter <- !is.na(op_protocol$month) &
    !is.na(op_protocol$quarter)
  op_protocol <- op_protocol[mask_month_quarter, ]
  # Remove NA unit of abundance or protocol
  mask_na_protocol_unit <- !is.na(op_protocol$protocol) |
    !is.na(op_protocol$unitabundance)
  op_protocol <- op_protocol[mask_na_protocol_unit, ]


  # Keep constant protocol by site
  op_protocol <- op_protocol %>%
    nest_by(siteid) %>%
    mutate(data = list(filter(data, protocol == getmode(protocol)))) %>%
    ungroup() %>%
    unnest(cols = c(data))

  ## Check unique protocol by site 
  unique_protocol_site <- op_protocol %>%
    nest_by(siteid) %$%
    map_chr(data, ~get_unique_values_c(.x$protocol))
  stopifnot(all(unique_protocol_site != "no_unique"))

  # Filter samplings based on the month 

  ## get time of fishing
  timing_fishing <- op_protocol %>%
    group_by(siteid) %>%
    summarise(
      med_month = median(month, na.rm = TRUE),
      mode_quarter = getmode(quarter)) %>%
    mutate( med_date = ymd(paste0("2020-", round(med_month), "-15")),
      mode_quarter = as.integer(mode_quarter))

    ## Compute difference between sampling and median + mask
    set_mask_filtering <- op_protocol %>%
      nest_by(siteid) %>%
      left_join(timing_fishing, by = "siteid") %>%
      mutate(
        month_duration = list(
          interval(ymd(data$date), med_date) %>%
            as.duration %>%
            as.numeric(., "month")
        )
        ) %>%
      unnest(cols = c(data, month_duration)) %>%
      ungroup() %>%
      mutate(
        month_check = month_filter(
          x = month_duration,
          range_x = extent_month,
          month_int_decimal = 1
          ),
        quarter_check = map2_lgl(
          quarter, mode_quarter,
          ~quarter_filter(quar = .x, mode_quar = .y)
        )
      )

      if (return_no_filtered) {
        return(set_mask_filtering)
      }

      output <- set_mask_filtering %>%
        filter(month_check | is.na(month) & quarter_check)

      # check quarter works
      mask_check <-
        is.na(output$month) &
        !is.na(output$quarter) &
        !is.na(output$mode_quarter) &
        (output$quarter != output$mode_quarter)
      stopifnot(all(mask_check == FALSE))

      # Protocol selection
      if (!is.null(selected_protocol)) {
        output <- output %>%
          filter(protocol %in% selected_protocol)
      }

      # Abundance Unit selection
      if (!is.null(selected_abun_unit)) {
        output <- output %>%
          filter(unitabundance %in% selected_abun_unit)
      }


      # Select one sampling per site and year
      output <- output %>%
        nest_by(siteid, year) %>%
        mutate(
          data = list(
            data[choose_from_multiple(
              x = data[["month_duration"]],
              quarter = data[["quarter"]],
              mode_quarter = data[["mode_quarter"]]
              ), ]
          )
          ) %>%
        unnest(cols = c(data)) %>%
        ungroup()

    # Span 
    if (!is.null(span_min)) {
	summary_sampling <- output %>%
	    group_by(siteid) %>%
	    summarise(
		span = max(year) - min(year) + 1
	    )

	    mask_span <- output$siteid %in%
		summary_sampling[summary_sampling$span >= span_min, ]$siteid

	    output <- output[mask_span, ]
    }

    # Number of sampling
    if (!is.null(nb_sampling)) {
	summary_sampling <- output %>%
	    group_by(siteid) %>%
	    summarise(
		n = n(),
		span = max(year) - min(year) + 1
	    )

	    mask_nb_sampling <- output$siteid %in%
		summary_sampling[summary_sampling$n >= nb_sampling, ]$siteid

	    output <- output[mask_nb_sampling, ]
    }

      var_to_drop <- c("med_month", "mode_quarter", "med_date",
	  "month_duration", "month_check", "quarter_check")
      output <- output %>%
	  select(- all_of(var_to_drop))

      stopifnot(all(!is.na(output$protocol)))

      return(output)
}
