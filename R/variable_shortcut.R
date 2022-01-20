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
  chao_simpson = "Chao simpson",
  chao_evenness = "Chao evenness",
  evenness = "Evenness",
  jaccard_dis = "Jaccard (binary, dissimilarity)",
  jaccard = "Jaccard (binary, similarity)",
  horn = "Horn (binary, similarity)",
  "chao" = "Chao (binary, similarity)",
  hillebrand = "SER_a (rel abundance)",
  total  = "Total turnover (codyn)",
  appearance = "Appearance",
  disappearance = "Disappearance",
  shannon = "Shannon",
  simpson = "Simpson",
  nestedness = "Nestedness (jaccard)",
  turnover = "Turnover (jaccard)"
  )
}
