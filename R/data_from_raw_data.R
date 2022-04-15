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
  exo_basin_site = exo_basin_site,
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
        )

    # match site and species based on basin_name
    site_sp_status <- exo_basin_site %>%
      # add species and status
      left_join(
        basin_sp_status,
        by = c("basin_name")
        )

  measurement_exo <- measurement %>%
    left_join(site_sp_status, by = c("siteid", "species"))

  return(measurement_exo)

}

complete_native_exotic_data <- function(
  meas = measurement_exo,
  loc = site_desc_loc) {

  # Register the origin of native/exotic species status
  meas <- meas %>%
    mutate(native_exotic_origin =
      ifelse(is.na(native_exotic_status),
        NA, "tedesco"
      )
    )

  # Get synonyms bw rivfishtime and fishbase
  df_syn_rivfishtime <- unique(meas$species) %>%
    rfishbase::synonyms() %>%
    select(provided_name = synonym, valid_name = Species, Comment = Status) %>%
    as_tibble() %>%
    filter(!Comment %in% c("misapplied name", "ambiguous synonym")) %>%
    select(provided_name, valid_name) %>%
    rename(species = provided_name, fishbase_name = valid_name)

  # Get the missing association species / country
  missing_species_measurement <-
    meas %>%
    filter(is.na(native_exotic_status)) %>%
    select(-fishbase_name) %>%
    # Re add fishbase name
    left_join(df_syn_rivfishtime, by = "species") %>%
    left_join(loc %>%
      select(siteid, country),
    by = c("siteid")
    ) %>%
    distinct(species, fishbase_name, country)

  # Correspondence bw country in rivfishtime and fishbase
  country_code_name <- c(
    "USA" = "USA",
    "FRA" = "France",
    "SWE" = "Sweden",
    "ESP" = "Spain",
    "GBR" = "UK",
    "AUS" = "Australia",
    "BEL" = "Belgium",
    "BWA" = "Botswana",
    "CIV" = "Cote d'Ivoire",
    "FIN" = "Finland",
    "HUN" = "Hungary",
    "BRA" = "Brazil",
    "NOR" = "Norway",
    "CAN" = "Canada",
    "JPN" = "Japan"
  )
  # Check if missing country correspondence
  stopifnot(all(
      unique(missing_species_measurement$country) %in%
        names(country_code_name)
      ))

  # Get the species status by country from fishbase
  country_fishbase <- rfishbase::country(
    species_list = unique(c(missing_species_measurement$fishbase_name,
        missing_species_measurement$species)))

  # Filter the combination species / country that we need 
  country_fishbase_filtered <-
    country_fishbase %>%
    filter((Species %in% unique(
          unlist(missing_species_measurement[,c("fishbase_name", "species")])
          ) & str_detect(
          country,
          paste0(country_code_name[missing_species_measurement$country],
            collapse = "|")
          ))) %>%
    select(country_fishbase = country, fishbase_name = Species, status =
      Status) %>%
    mutate(country = names(country_code_name)[
      map_int(country_fishbase, ~which(str_detect(.x, country_code_name)))
      ]
    )
    stopifnot(all(!is.na(country_fishbase_filtered$country)))
    stopifnot(all(!is.na(country_fishbase_filtered$country_fishbase)))
    stopifnot(all(!is.na(missing_species_measurement$country)))

    country_fishbase_resolution <- missing_species_measurement %>%
      left_join(country_fishbase_filtered,
        by = c("fishbase_name", "country")) %>%
      distinct(fishbase_name, country, status, .keep_all = TRUE)

    # Check for dbl:
    dupl_country_sp <- country_fishbase_resolution %>%
      group_by(country, fishbase_name) %>%
      summarise(n = n(), .groups = "drop") %>%
      filter(n > 1)

    # Let's put it in native if GBR is a country
    country_fishbase_resolution %>%
      filter(country == "GBR", fishbase_name == "Gymnocephalus cernua")
    country_fishbase_resolution <-
      country_fishbase_resolution %>%
      mutate(status = ifelse(
          country_fishbase == "UK Scotland" &
            fishbase_name == "Gymnocephalus cernua",
          "native", status)) %>%
      distinct(fishbase_name, country, status, .keep_all = TRUE)

    # Recheck if double species / country
    dupl_country_sp <- country_fishbase_resolution %>%
      group_by(country, fishbase_name) %>%
      summarise(n = n(), .groups = "drop") %>%
      filter(n > 1)
    stopifnot(nrow(dupl_country_sp) == 0)

    # Search manually, best way to learn about fish
    country_fishbase_resolution <-
      country_fishbase_resolution %>%
      mutate(native_exotic_origin2 = ifelse(is.na(status), "handmade",
          "autofishbase")) %>%
      mutate(
        status = case_when(
          #https://www.fishbase.de/summary/Rutilus-rutilus.html
          country == "USA" & species == "Rutilus rutilus" ~ "introduced",
          #https://www.gbif.org/species/5206976/metrics
          country == "CIV" & species == "Barbus chlorotaenia" ~ "introduced",
          # No mention found in the country, but in the neighbors country
          country == "CIV" & species == "Barbus foutensis" ~ "introduced",
          # No mention found in the country, but in the neighbors country
          #https://books.google.com/books?id=9lEFfsaCDNkC&pg=PA153&lpg=PA153&dq=%22Synodontis+ocellifer%22+cote+d%27ivoire&source=bl&ots=jWOQ1OPcKE&sig=ACfU3U1aPNT3bM4XrlK1YeYeMF7JaqiGdg&hl=en&sa=X&ved=2ahUKEwiUwLWlgID3AhXTMX0KHbxXCXEQ6AF6BAgtEAM#v=onepage&q=%22Synodontis%20ocellifer%22%20cote%20d'ivoire&f=false
          country == "CIV" & species == "Synodontis ocellifer" ~ "introduced",
          # Native in a lot of countries around and cross Africa, so I decied to put
          # it as native
          #https://www.fishbase.se/Country/CountryList.php?ID=4479&GenusName=Hydrocynus&SpeciesName=vittatus
          country == "CIV" & species == "Hydrocynus vittatus" ~ "native",
          #https://fishesofaustralia.net.au/home/species/2693
          country == "AUS" & species == "Carassius carassius" ~ "introduced",
          # Originated from South America but introduced in Asia:
          #https://www.fishbase.se/Country/CountryList.php?ID=4751&GenusName=Geophagus&SpeciesName=brasiliensis
          country == "AUS" & species == "Geophagus brasiliensis" ~ "introduced",
          country == "FRA" & species == "Alosa agone" ~ "native",
          # Native from eastern Europe but introduced in several countries of
          # northen Europe and western europe: 
          #https://www.fishbase.se/Country/CountryList.php?ID=12019&GenusName=Neogobius&SpeciesName=melanostomus
          country == "FRA" & species == "Neogobius melanostomus" ~ "introduced",
          # Native from eastern Europe but introduced in several countries of
          # central Europe: 
          #https://www.fishbase.se/Country/CountryList.php?ID=25977&GenusName=Ponticola&SpeciesName=kessleri
          country == "FRA" & species == "Ponticola kessleri" ~ "introduced",
          # Native from eastern Europe but introduced in several countries of
          # central Europe: 
          #https://www.fishbase.de/Country/CountryList.php?ID=65128&GenusName=Proterorhinus&SpeciesName=semilunaris
          country == "FRA" & species == "Proterorhinus semilunaris" ~ "introduced",
          # Introduced: https://inpn.mnhn.fr/espece/cd_nom/70166
          # first reported in 2014
          country == "FRA" & species == "Neogobius fluviatilis" ~ "introduced",
          # European sport fish:
          country == "USA" & species == "Rutilus rutilus" ~ "introduced",
          TRUE                           ~ status
          )) %>%
      # Fix fishbase name:
      distinct(country, fishbase_name, status, .keep_all = TRUE)

    completed_meas <- meas %>%
      select(-fishbase_name) %>%
      # Re add fishbase name
      left_join(df_syn_rivfishtime, by = "species") %>%
      left_join(loc %>%
        select(siteid, country),
      by = c("siteid")) %>%
      left_join(
        country_fishbase_resolution %>%
          select(country, fishbase_name, native_exotic_status2 = status, native_exotic_origin2),
        by = c("country", "fishbase_name")
      ) %>%
      mutate(
        native_exotic_status = ifelse(is.na(native_exotic_status),
          native_exotic_status2, native_exotic_status),
        native_exotic_origin = ifelse(is.na(native_exotic_origin),
          native_exotic_origin2, native_exotic_origin),

        ) %>%
      select(-native_exotic_status2, -native_exotic_origin2)

    
    if(any(is.na(completed_meas$native_exotic_status))) {
      print(completed_meas %>%
              filter(is.na(native_exotic_status)) %>%
              distinct(country, fishbase_name, native_exotic_origin)
            )
      stop("Missing species in the completed exotic species database")
    }

    # Stick to two native exotic status 

    completed_meas <- completed_meas %>% 
      mutate(native_exotic_status = case_when(
          native_exotic_status == "introduced" ~ "exotic",
          native_exotic_status == "not established" ~ "exotic",
          native_exotic_status == "endemic" ~ "native",
          native_exotic_status == "misidentification" ~ "native",
          TRUE ~ native_exotic_status
      )
      )

    stopifnot(all(
        completed_meas$native_exotic_status
        %in% c("native", "exotic")
      ))

    return(completed_meas)

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
