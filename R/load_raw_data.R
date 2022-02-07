#' Load and clean Lise database
#'
load_time_series_data <- function(path) {

  output <- read_csv(path) %>%
    select(-...1) #Remove dummy variables

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

