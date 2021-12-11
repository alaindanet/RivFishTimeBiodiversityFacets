
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

    # delete the year: take the remainder of the division by 12
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

quarter_filter <- function(quar = NULL, mode_quar = NULL) {
    quar == mode_quar 
}
filter_op <- function(
    op_protocol = NULL,
    selected_protocol = NULL,
    selected_abun_unit = NULL,
    nb_sampling = NULL,
    extent_month = 1 
    ) {

    # get time of fishing 
    timing_fishing <- op_protocol %>%
        group_by(siteid) %>%
        summarise(
            med_month = median(month, na.rm = TRUE),
            mode_quarter = getmode(quarter)
        ) %>%
        mutate(med_date = ymd(paste0("2020-", round(med_month), "-01")))


    set_mask_filtering <- op_protocol %>%
        nest_by(siteid) %>%
        left_join(timing_fishing, by = "siteid") %>%
        mutate(
            month_duration = list(
                interval(mdy(data$date), med_date) %>%
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
            quarter_check = map2_lgl(quarter, mode_quarter,
                ~quarter_filter(quar = .x, mode_quar = .y)
            )
        )

    output <- set_mask_filtering %>%
        filter(month_check | is.na(month) & quarter_check)

    # check
    stopifnot(all(is.na(output$month) & output$quarter == output$mode_quarter))

    # Protocol selection
    if (!is.null(selected_protocol)) {
        output <- output %>%
            filer(protocol %in% selected_protocol)
    }

    # Abundance Unit selection 
    if (!is.null(selected_abun_unit)) {
        output <- output %>%
            filer(unitabundance %in% selected_abun_unit)
    }
    
    # Number of sampling 
    if (!is.null(nb_sampling)) {
        summary_sampling <- output %>%
            group_by(siteid) %>%
            summarise(n = n())

        mask_nb_sampling <- output$siteid %in% summary_sampling[summary_sampling$n >= nb_sampling,]$siteid
        
        output <- output[mask_nb_sampling,]
    }


    return(output)
}
