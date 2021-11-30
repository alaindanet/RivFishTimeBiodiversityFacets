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
