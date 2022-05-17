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
  add_var_from_protocol = NULL,
  lime_data = lime_site_swe 
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

  if (type %in% c("all", "location", "measurement")) {
    mask <- site_desc_loc$siteid %in% op_protocol$siteid
    output$location <- site_desc_loc[mask, ]
    # Remove NZ: opportunistic fishing operation
    mask <- output$location$country != "New Zealand"
    output$location <- output$location[mask, ]
    # Remove limmed sites of SWEDEN
    mask <- !output$location$siteid %in% unique(lime_data$siteid)
    output$location <- output$location[mask, ]
  }

  if (type %in% c("all", "measurement", "abun_rich_op")) {
    mask <- measurement$op_id %in% op_protocol$op_id
    output$measurement <- measurement[mask, ]
    # Filter sites 
    opid_to_keep <- op_protocol[
      op_protocol$siteid %in% unique(output$location$siteid), ]$op_id
    mask <- measurement$op_id %in% unique(opid_to_keep) 
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

tar_avg_first_year_measurement <- function(
  dataset = NULL,
  nb_sampling_to_average = 3
  ) {

  output <- dataset %>%
    nest_by(siteid) %>%
    mutate(
      data = list(
        avg_first_year_measurement(
          x = data,
          nb_sampling_to_average = nb_sampling_to_average
        )
      )
    )

    output %>%
      ungroup() %>%
      unnest(cols = data)


}

avg_first_year_measurement <- function(
  x = NULL,
  nb_sampling_to_average = 3
  ) {

  stopifnot(all(na.omit(x$abundance) != 0) & all(na.omit(x$biomass) != 0))
  new_df <- x %>%
    pivot_wider(
      names_from = species,
      names_sep = ".",
      values_from = c(abundance, biomass),
      values_fill = 0
      ) %>%
  arrange(year)

avg_first_year <- new_df %>%
  slice(1:nb_sampling_to_average) %>%
  summarise(across(where(is.numeric), mean))

output <- rbind(
  new_df[-c(1:nb_sampling_to_average), ],
  mutate(
    avg_first_year,
    op_id = new_df[median(1:nb_sampling_to_average), ]$op_id
  )
)

# Reput abundance and biomass in right shape
output <- output %>%
  pivot_longer(
    cols = c(-op_id, -year),
    names_to = c("variable", "species"),
    names_sep = "([.])",
    values_to = "value") %>%
pivot_wider(names_from = "variable",
  values_from = "value")

output <- output %>%
  filter(abundance != 0)

# Check that year not touch are the same 
year_to_be_same <- sort(unique(x$year))[-c(1:nb_sampling_to_average)]
x_comparison <- x %>%
  filter(year %in% year_to_be_same) %>%
  arrange(year, species)
output_comparison <- output %>%
  filter(year %in% year_to_be_same) %>%
  arrange(year, species)

stopifnot(all(x_comparison$abundance == output_comparison$abundance))
stopifnot(all(is.na(x_comparison$biomass) == is.na(output_comparison$biomass)))
stopifnot(all(na.omit(x_comparison$biomass) == na.omit(output_comparison$biomass)))

return(output)

}

get_filtered_abun_rich_exo <- function(
  abun_rich = NULL,
  perc_na_abun_thld = 0.05,
  min_nb_sampling_by_site = 5
) {

  filtered_abun_rich_exo <- abun_rich %>%
    filter(!perc_na_exo_abun > perc_na_abun_thld) %>%
    select(
      siteid, op_id,
      species_nb, species_nb_nat, species_nb_exo,
      perc_exo_sp, perc_nat_sp,
      total_abundance, nat_abun, exo_abun,
      perc_exo_abun, perc_nat_abun
      ) %>%
    group_by(siteid) %>%
    filter(length(siteid) >= min_nb_sampling_by_site) %>%
    ungroup()

  stopifnot(
    filtered_abun_rich_exo %>%
      group_by(siteid) %>%
      summarise(n = n()) %>%
      filter(n < min_nb_sampling_by_site) %>%
      nrow == 0
  )

  return(filtered_abun_rich_exo)

}
