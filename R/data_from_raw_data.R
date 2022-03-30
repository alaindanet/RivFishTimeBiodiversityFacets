#' Get small tibbles from raw data
#'
#' get_site_desc_loc(): Keep unique values for site location
#'
get_site_desc_loc <- function(ts_data = NULL, save_data = TRUE) {

  # Get unique values for each site
  site_desc_loc <- ts_data %>%
    group_by(siteid) %>%
    summarise_at(.vars = get_var_localisation(),
      .funs = ~get_unique_values_c(x = .x, na.omit = FALSE)) %>%
    ungroup()

  # Fix Country
  mask_usa <- stringr::str_detect(site_desc_loc$country, "United States")
  site_desc_loc$country[mask_usa] <- "USA"
  mask_canada <- stringr::str_detect(site_desc_loc$country, "Canada")
  site_desc_loc$country[mask_canada] <- "CAN"

  if (save_data) {
    usethis::use_data(site_desc_loc, overwrite = TRUE)
  }
  return(site_desc_loc)
}

get_abun_rich_op <- function(ts_data = NULL, save_data = TRUE) {

   abun_rich_op <- ts_data %>%
    group_by(op_id) %>%
    summarise(
      species_nb = length(unique(species)),
      total_abundance = sum(abundance),
      .groups = "drop"
    )

    if (save_data) {
      usethis::use_data(abun_rich_op, overwrite = TRUE)
    }
  return(abun_rich_op)
}

get_species_number_site <- function(
  ts_data = NULL, species_number_by_op = NULL, op_protocol = NULL, save_data = TRUE) {

  species_total_site <- ts_data %>%
    group_by(siteid) %>%
    summarise(species_tot_nb = length(unique(species)), .groups = "drop")

  species_number_site <- species_number_by_op %>%
    left_join(op_protocol, by = "op_id") %>%
    group_by(siteid) %>%
    summarise(enframe(summary_distribution(x = species_nb)), .groups = "drop") %>%
    pivot_wider(names_from = "name", values_from = "value") %>%
    left_join(species_total_site, by = "siteid")

    if (save_data) {
      usethis::use_data(species_number_site, overwrite = TRUE)
    }
  return(species_number_site)
}

get_species_status <- function(ts_data = NULL, save_data = TRUE) {

  species_status <- ts_data %>%
    group_by(species) %>%
    summarise(status = get_unique_values_c(speciesstatus))

    if (save_data) {
      usethis::use_data(species_status, overwrite = TRUE)
    }
  return(species_status)

}

get_op_protocol <- function(ts_data = NULL, save_data = TRUE) {

  quali_protocol <- ts_data %>%
    group_by(siteid, op_id) %>%
    summarise_at(vars(all_of(get_var_protocol())),
      get_unique_values_c, na.omit = FALSE) %>%
    ungroup()

  #unitbiomass is full of na bc biomass is often not reported

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
 
  # Transform quarter in numeric variable:
  # I remove non-numeric variables because there are interesting since they
  # cover 6 months.
  # operation in 2/3: april, may, june / july, august, september
  # operation in 4/1: october, november, december / jan, fev, march
  op_protocol[op_protocol$quarter %in% c("4/1", "2/3"), ]$quarter <- NA
  op_protocol$quarter <- as.numeric(op_protocol$quarter)

  # Get proper date format
  op_protocol$date <-  mdy(op_protocol$date)

  if (save_data) {
    usethis::use_data(op_protocol, overwrite = TRUE)
  }

  return(op_protocol)
}

get_site_quali_quanti_protocol <- function(op_data = NULL, type = NULL, save_data = TRUE) {

  stopifnot(type %in% c("quali", "quanti"))

  if (type == "quali") {
    # get the unique qualitative description of site protocol
    site_protocol_quali <- op_data %>%
      group_by(siteid) %>%
      summarise_at(vars(all_of(c("protocol", "protocol_detail", "unitbiomass", "unitabundance"))),
        get_unique_values_c, na.omit = FALSE) %>%
      ungroup()

  if (save_data) {
    usethis::use_data(site_protocol_quali, overwrite = TRUE)
  }
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

  if (save_data) {
    usethis::use_data(site_protocol_quanti, overwrite = TRUE)
  }
    return(site_protocol_quanti)
  }
}

get_abundance_biomass_data <- function(ts_data = NULL, save_data = TRUE) {

  measurement <- ts_data %>%
    select(op_id, species, abundance, biomass)


  # Check for several records of the same species
  bad_abundance <- measurement %>%
    group_by(op_id, species) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(n > 1)
  mask <- paste0("op_id == '", bad_abundance$op_id, "' & ",
    "species == '", bad_abundance$species, "'", collapse = " | ")

  check_df <- measurement %>%
    filter(eval(parse(text = mask)))
  if (nrow(check_df) > 0) {
    warning("In the following samplings: ",
      paste0(bad_abundance$op_id, collapse = ", "),
      "\n one species had two records, I sum them since their abundance is low.\n",
      print(check_df)
    )

    fixed_multiple_record <- check_df %>%
      group_by(op_id, species) %>%
      summarise(across(where(is.double), sum), .groups = "drop") %>%
      ungroup()

    # glue measurement without bad measurement and the fixed parts 
    measurement <- rbind(
      filter(measurement, !eval(parse(text = mask))),
      fixed_multiple_record) %>%
    arrange(op_id, species)
  }

  #bad_abundance <- filtered_dataset$measurement %>%
  #group_by(siteid, year, species) %>%
  #summarise(n = n()) %>%
  #filter(n > 1)


  if (save_data) {
    usethis::use_data(measurement, overwrite = TRUE)
  }

  return(measurement)

}

get_toy_dataset <- function(
  measurement = NULL,
  op_protocol = NULL,
  site_protocol_quanti = NULL, seed = 123, save_data = TRUE) {

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

  if (save_data) {
    usethis::use_data(toy_dataset, overwrite =TRUE)
  }
  return(toy_dataset)

}

get_measurement_exo <- function(
  occ_exotic = occ_exotic,
  measurement = filtered_dataset$measurement
  ) {

  # Get fishbase species synonyms
  df_syn_rivfishtime <- unique(measurement$species) %>%
    rfishbase::synonyms() %>%
    select(provided_name = synonym, valid_name = Species, Comment = Status) %>%
    as_tibble()
  df_syn_tedesco <- unique(occ_exotic$species) %>%
    rfishbase::synonyms() %>%
    select(provided_name = synonym, valid_name = Species, Comment = Status) %>%
    as_tibble()

  # Join synonyms to occ_exotic dataset 
  occ_exotic_fb <- occ_exotic %>%
    select(basin_name, species, native_exotic_status) %>%
    left_join(
      df_syn_tedesco %>%
        filter(Comment == "accepted name") %>%
        select(provided_name, valid_name) %>%
        rename(species = provided_name, fishbase_name = valid_name),
      by = "species"
    )
    # match species of sp_rivfishtime with tedesco based on fishbase species
    # name 
    basin_sp_status <- df_syn_rivfishtime %>%
      filter(Comment == "accepted name") %>%
      select(provided_name, valid_name) %>%
      rename(species = provided_name, fishbase_name = valid_name) %>%
      left_join(
        occ_exotic_fb %>%
          select(-species),
        by = "fishbase_name"
        ) %>%
      select(-fishbase_name)

    # match site and species based on basin_name
    site_sp_status <- exo_basin_site %>%
      # add species and status
      left_join(
        basin_sp_status,
        by = c("basin_name")
        ) %>%
    select(-basin_name)

  measurement_exo <- measurement %>%
    left_join(site_sp_status, by = c("siteid", "species"))

  return(measurement_exo)

}

get_abun_rich_exo <- function(
  measurement_exo = NULL
  ) {

  measurement_exo %>%
    group_by(op_id) %>%
    summarise(
      siteid = unique(siteid),
      species_nb = length(unique(species)),
      total_abundance = sum(abundance),
      nat_abun = sum(abundance[native_exotic_status == "native"]),
      exo_abun = sum(abundance[native_exotic_status == "exotic"]),
      na_exo_abun = sum(abundance[is.na(native_exotic_status)]),
      na_exo_sp = length(unique(species[is.na(native_exotic_status)])),
      species_nb_nat = length(unique(species[native_exotic_status == "native"])),
      species_nb_exo = length(unique(species[native_exotic_status == "exotic"]))
      ) %>%
    mutate(
      perc_na_exo_abun = na_exo_abun / total_abundance,
      perc_na_exo_sp = na_exo_sp / species_nb,
      perc_exo_sp = species_nb_exo / species_nb,
      perc_nat_sp = species_nb_nat / species_nb,
      perc_exo_abun = exo_abun / total_abundance,
      perc_nat_abun = nat_abun / total_abundance
    )
}
