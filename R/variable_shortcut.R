#' Shortcut to obtain vector of variable names 
#'
#' @param NULL
#' @return character vector
get_var_localisation <- function () {
  c("latitude", "longitude", "province", "waterbody", "country", "region",
    "ecoregion", "main_bas")
}
get_var_protocol <- function () {
  c("date", "year", "month", "quarter", "protocol",
    "protocol_detail", "sampledlength_m", "unitabundance",
    "unitbiomass", "watertemp_c")
}

get_var_replacement <- function() {
  c(
  log_species_nb = "Log species richness",
  species_nb = "Species richness",
  log_total_abundance = "Log total abundance",
  total_abundance = "Total abundance",
  chao_richness = "Chao species richness",
  chao_shannon = "Chao shannon",
  chao_simpson = "Chao simpson"
  )
}
