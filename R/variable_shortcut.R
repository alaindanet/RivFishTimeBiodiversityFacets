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

get_river_atlas_significant_var <- function() {
  c(
    #"Length of the reach" = "length_km",
    "Distance from source" = "dist_up_km",
    "Strahler order" = "ord_stra",
    #"Order flow" = "ord_flow",
    "Average elevation (m)" = "ele_mt_cav",
    "Annual average of temperature (°C)" = "tmp_dc_cyr",
    #"Annual maximum of temperature (°C)" = "tmp_dc_cmx",
    #"Annual minimum of temperature (°C)" = "tmp_dc_cmn",
    #"Average slope (degree)" = "slp_dg_cav",
    "Annual average of discharge (m3/s)" = "dis_m3_pyr",
    #"Annual maximum of discharge (m3/s)" = "dis_m3_pmx",
    #"Annual minimum of discharge (m3/s)" = "dis_m3_pmn",
    #"River area (reach segment, in ha)" = "ria_ha_csu",
    #"River volume (reach segment, m3)" = "riv_tc_csu",
    "Protected area extent (%)" = "pac_pc_cse",
    "Urban extent (%)" = "urb_pc_cse",
    #"Human footprint 1993 (index)" = "hft_ix_c93",
    "Human footprint 2009 (index)" = "hft_ix_c09"
  )
}
