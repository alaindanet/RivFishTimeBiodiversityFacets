#' Select protocol and site
#'
#'
#' @param type character string of length one "all", "site_quali", sit 
#'
get_filtered_dataset <- function(
  op_protocol = NULL,
  type = NULL,
  measurement = NULL,
  site_desc_loc = NULL,
  add_var_from_protocol = NULL
  ) {

  output <- list()

  if (type %in% c("all", "quali")) {
    output$site_quali <- get_site_quali_quanti_protocol(
      op_data = op_protocol,
      type = "quali",
      save_data = FALSE
    )
  }

  if (type %in% c("all", "quanti")) {
    output$site_quanti <- get_site_quali_quanti_protocol(op_data = op_protocol,
      type = "quanti",
      save_data = FALSE)
  }

  if (type %in% c("all", "location")) {
    mask <- site_desc_loc$siteid %in% op_protocol$siteid
    output$location <- site_desc_loc[mask, ]
    # Remove NZ: opportunistic fishing operation 
    mask <- output$location$country != "New Zealand"
    output$location <- output$location[mask, ]
  }

  if (type %in% c("all", "measurement", "abun_rich_op")) {
    mask <- measurement$op_id %in% op_protocol$op_id
    output$measurement <- measurement[mask, ]

  }

  if (type %in% c("all", "abun_rich_op")) {
    output$abun_rich_op <- get_abun_rich_op(
      ts_data = output$measurement,
      save_data = FALSE
    )

    output$abun_rich_op <- output$abun_rich_op %>%
      mutate(
        log_total_abundance = log(total_abundance),
        log_species_nb = log(species_nb)
      )
  }

  if (!is.null(add_var_from_protocol)) {

    output <- map(output, function(x, ...) {

      if (!any(colnames(x) == "op_id")) {
        return(x)
      } else {
        x <- x %>%
          left_join(op_protocol[, c("op_id", add_var_from_protocol)],
            by = "op_id"
            )

        return(x)
      }
    }
    )
  }

  if (type != "all") {
    output[[type]]
  } else {
    return(output)
  }

}
