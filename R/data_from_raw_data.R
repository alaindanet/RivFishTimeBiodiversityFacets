#' Get small tibbles from raw data
#'
#' get_site_desc_loc(): Keep unique values for site location
#'
get_site_desc_loc <- function(ts_data = NULL) {

  # Get unique values for each site
  site_desc_loc <- ts_data %>%
    group_by(siteid) %>%
    summarise_at(.vars = get_var_localisation(),
      .funs = ~get_unique_values_c(x = .x, na.omit = FALSE)) %>%
    ungroup()

  usethis::use_data(site_desc_loc, overwrite = TRUE)
  return(site_desc_loc)
}

get_abun_rich_op <- function(ts_data = NULL) {

   abun_rich_op <- ts_data %>%
    group_by(op_id) %>%
    summarise(
      species_nb = length(unique(species)),
      total_abundance = sum(abundance),
      .groups = "drop"
    )

  usethis::use_data(abun_rich_op, overwrite = TRUE)
  return(abun_rich_op)
}

get_species_number_site <- function(
  ts_data = NULL, species_number_by_op = NULL, op_protocol = NULL) {

  species_total_site <- ts_data %>%
    group_by(siteid) %>%
    summarise(species_tot_nb = length(unique(species)), .groups = "drop")

  species_number_site <- species_number_by_op %>%
    left_join(op_protocol, by = "op_id") %>%
    group_by(siteid) %>%
    summarise(enframe(summary_distribution(x = species_nb)), .groups = "drop") %>%
    pivot_wider(names_from = "name", values_from = "value") %>%
    left_join(species_total_site, by = "siteid")

  usethis::use_data(species_number_site, overwrite = TRUE)
  return(species_number_site)
}

get_species_status <- function(ts_data = NULL) {

  species_status <- ts_data %>%
    group_by(species) %>%
    summarise(status = get_unique_values_c(speciesstatus))

  usethis::use_data(species_status, overwrite = TRUE)
  return(species_status)

}

get_op_protocol <- function(ts_data = NULL) {

  quali_protocol <- ts_data %>%
    group_by(siteid, op_id) %>%
    summarise_at(vars(all_of(get_var_protocol())),
      get_unique_values_c, na.omit = FALSE) %>%
    ungroup()

  #unitbiomass is full of na bc often
  #biomass is not reported

  # Check if there are biomass measurements without biomass unit:
  stopifnot(all((!is.na(ts_data$biomass) & is.na(ts_data$unitbiomass)) == FALSE))

  biomass_unit <- ts_data %>%
    group_by(op_id) %>%
    summarise(unitbiomass = get_unique_values_c(x = unitbiomass, na.omit = TRUE))

  # Check that there is not multiple biomass units by operation
  stopifnot(all(!biomass_unit$unitbiomass %in% c("no_unique", NA)))

  # Merge data 
  op_protocol <- quali_protocol %>%
    select(-unitbiomass) %>%
    left_join(biomass_unit, by = c("op_id"))

  usethis::use_data(op_protocol, overwrite = TRUE)

  return(op_protocol)



}

get_site_quali_quanti_protocol <- function(op_data = NULL, type = NULL) {

  stopifnot(type %in% c("quali", "quanti"))

  if (type == "quali") {
    # get the unique qualitative description of site protocol
    site_protocol_quali <- op_data %>%
      group_by(siteid) %>%
      summarise_at(vars(all_of(c("protocol", "protocol_detail", "unitbiomass", "unitabundance"))),
        get_unique_values_c, na.omit = FALSE) %>%
      ungroup()

    usethis::use_data(site_protocol_quali, overwrite = TRUE)
    return(site_protocol_quali)

  } else {
    # get the quantitative description of site protocol, i.e. the summary
    # distribution of month, year, sampling effort
    site_protocol_quanti <- op_data %>%
      group_by(siteid) %>%
      select_if(is.numeric) %>%
      pivot_longer(!siteid, names_to = "variable", values_to = "value") %>%
      group_by(siteid, variable) %>%
      summarise(enframe(summary_distribution(value, na.rm = TRUE)), .groups = "drop") %>%
      pivot_wider(names_from = "name", values_from = "value")

    usethis::use_data(site_protocol_quanti, overwrite = TRUE)
    return(site_protocol_quanti)
  }
}

get_abundance_biomass_data <- function(ts_data = NULL) {

  measurement <- ts_data %>%
    select(op_id, species, abundance, biomass)
  usethis::use_data(measurement, overwrite = TRUE)

  return(measurement)

}

get_toy_dataset <- function(
  measurement = NULL,
  op_protocol = NULL,
  site_protocol_quanti = NULL, seed = 123) {

  site_id_no_na_month <- site_protocol_quanti %>%
    filter(n_na == 0, variable == "month") %>%
    .[["siteid"]]

  set.seed(seed)
  siteid_mask <- sample(
    op_protocol[
      op_protocol$unitabundance == "Count" &
        op_protocol$siteid %in% site_id_no_na_month,
      ]$siteid, 5)
  # Make a toy dataset
  toy_dataset <- measurement %>%
    left_join(op_protocol, by = "op_id") %>%
    filter(siteid %in% siteid_mask)

  usethis::use_data(toy_dataset, overwrite =TRUE)
  return(toy_dataset)

}
