#' Load and clean Lise database
#'
load_time_series_data <- function(path) {

  output <- read_csv(path)

  if ("...1" %in% colnames(output)) {
    output <- output %>%
    select(-...1) #Remove dummy variables
  }

  # Stop if parsing problems
  readr::stop_for_problems(output)

  # Clean column names
  colnames(output) <- tolower(colnames(output))

  # Correct the encoding (necessary for mapView)
  col_chr <- purrr::map_lgl(output, is.character)
  output[, col_chr] <- map(output[, col_chr],
    ~stringi::stri_conv(.x, from = "ISO-8859-1", to = "UTF-8"))

  return(output)
}

get_raw_file_path <- function() {
  if (Sys.info()["user"] == "ahdanet") {
    dir_file <- "L:/RivFishTIME/house_version"
  } else {
    dir_file <- here::here("inst", "extdata")
  }
  paste0(dir_file, "/", "GlobalTimeSeries_database_1232021.csv")
}

download_chelsa <- function(
  year_selected = seq(1980, 2019),
  dl_dir = here::here("L://ENV_LAYERS/CHELSA"),
  overwrite = FALSE
  ) {

  if (!dir.exists(dl_dir)) {
    dir.create(dl_dir, recursive = FALSE, showWarnings = FALSE)
  }

  url_prefix <- "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/tas/"
  file_radical <- "CHELSA_tas_"
  file_suffix <- "_V.2.1.tif"

  # Generate file names:
  monthly <- c(paste0("0", seq(1, 9)), paste0(1, seq(0, 2)))
  year_selected <- year_selected[
    year_selected >= 1980 | year_selected <= 2019
    ]

  file_names <- purrr::map(
    year_selected,
    ~paste0(file_radical, monthly, "_", .x, file_suffix)) %>%
    purrr::reduce(., c)


  if (!overwrite) {
    mask_already_dl <- file_names %in% list.files(dl_dir)
    file_names <- file_names[!mask_already_dl]
  }

  for (i in seq_along(file_names)) {
    curl::curl_download(
      url = paste0(url_prefix, file_names[i]),
      destfile = paste0(dl_dir, "/", file_names[i])
    )
  }

}

convert_chelsa_to_celcius <- function(x = NULL) {
  #https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification_V2.pdf
  x / 10 - 273.15
}

get_additional_dataset_ref_rivfistime <- function(
  published = published_rivfishtime_characteristic,
  updated = updated_rivfishtime_characteristic,
  site_paper = filtered_dataset_modelling$location
) {

  # Keep only sites used in analysis
  published %<>%
    filter(SiteID %in% unique(site_paper$siteid)) %>%
    arrange(SiteID)

  # Check that siteid match between published rivfishtime and updated version
  test_match <- updated %>%
    select(Glob_ID, SiteID, Origin) %>%
    filter(
      SiteID %in% unique(c(
          published$SiteID
          ))) %>%
    arrange(SiteID)
  stopifnot(all(published$SiteID == test_match$SiteID))

  # Filter site present in paper but not present in RivFishTime publication
  ti <- updated %>%
    filter(
      SiteID %in% unique(site_paper$siteid) & !SiteID %in% unique(published$SiteID)
      ) %>%
    group_by(Origin, Country) %>%
    summarise(n = n(), .groups = "drop")

  # Get references about extra dataset
  origin_link <- c(
    "Maryland" = "https://www.montgomerycountymd.gov/water/streams/data.html",
    "Ohio"     = "https://www.orsanco.org/programs/fish-population/",
    "RAMP"     = "http://www.ramp-alberta.org/RAMP.aspx",
    "MARIS"    = paste0(c("https://www.sciencebase.gov/catalog/item/54998234e4b08b255be64e6e",
        "https://www.sciencebase.gov/catalog/item/529e0108e4b0516126f68e3c"), collapse = ", ")
  )
  origin_program <- c(
    "Maryland"  = "Montgomery county monitoring program (2018)",
    "Ohio"      = "Ohio statewide monitoring program (2018)",
    "RAMP"      = "Regional Aquatics Monitoring Program (2018)",
    "MARIS"     = "Multistate Aquatic Resources Information System"
  )
  origin_citation <- c(
    "Maryland"  = "Montgomery county monitoring program (2018). Available at https://www.montgomerycountymd.gov/water/streams/data.html",
    "Ohio"      = "Ohio statewide monitoring program (2018). Available at https://www.orsanco.org/programs/fish-population/",
    "RAMP"      = "Regional Aquatics Monitoring Program (2018). Available at http://www.ramp-alberta.org/RAMP.aspx",
    "MARIS"     = "U.S. Geological Survey, Core Science Analytics and Synthesis Program, 20131201, Multistate Aquatic Resources Information System (MARIS): United States Geological Survey, https://doi.org/10.5066/F7988525."
  )

  out <- ti %>%
  mutate(
    program = origin_program[Origin],
    link = origin_link[Origin],
    reference = origin_citation[Origin],
    Country = str_replace(Country, "United States", "USA")
    ) %>%
  select(
    `source id` = Origin,
    Country,
    n,
    program,
    `Database source` = link,
    reference
  ) %>%
  rename_with(str_to_sentence)

return(out)

}
