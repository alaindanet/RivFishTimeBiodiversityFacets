#' Shortcut to obtain vector of variable names
#'
#' @param NULL
#' @return character vector
get_var_localisation <- function () {
  c("latitude", "longitude", "province", "waterbody", "country", "region",
    "ecoregion", "main_bas", "origin")
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
    log_chao_richness = "Log Chao species richness",
    chao_shannon = "Chao shannon",
    chao_simpson = "Chao simpson",
    chao_evenness = "Chao evenness",
    evenness = "Evenness",
    evenness_scaled = "Evenness",
    jaccard_dis = "Jaccard (binary, dissimilarity)",
    jaccard_dis_scaled = "Jaccard (binary, dissimilarity)",
    jaccard = "Jaccard (binary, similarity)",
    horn = "Horn (binary, similarity)",
    "chao" = "Chao (binary, similarity)",
    hillebrand = "SER_a (rel abundance)",
    hillebrand_dis_scaled = "Dissimilarity (Simpson index)",
    total = "Total turnover (codyn)",
    appearance = "Appearance",
    appearance_scaled = "Appearance",
    disappearance_scaled = "Disappearance",
    shannon = "Shannon",
    simpson = "Simpson",
    nestedness = "Nestedness (jaccard)",
    nestedness_scaled = "Nestedness (jaccard)",
    turnover = "Turnover (jaccard)",
    turnover_scaled = "Turnover (jaccard)",
    species_nb_nat = "Species richness (native)",
    species_nb_exo = "Species richness (non-native)",
    perc_exo_sp = "Proportion non-native species",
    perc_nat_sp = "Proportion native species",
    perc_nat_abun = "Proportion native abundance",
    perc_exo_abun = "Proportion non-native abundance",
    nat_abun = "Native abundance",
    exo_abun = "Non-native abundance"
      )
}

get_var_replacement_vulgarisation <- function() {
  c(
    log_species_nb = "Species richness (raw)",
    species_nb = "Species richness",
    log_total_abundance = "Abundance (total)",
    total_abundance = "Total abundance",
    chao_richness = "Chao species richness",
    log_chao_richness = "Richness",
    chao_shannon = "Chao shannon",
    chao_simpson = "Chao simpson",
    chao_evenness = "Chao evenness",
    evenness = "Evenness",
    evenness_scaled = "Evenness",
    jaccard_dis = "Jaccard (binary, dissimilarity)",
    jaccard_dis_scaled = "Jaccard dissimilarity",
    jaccard = "Jaccard (binary, similarity)",
    horn = "Horn (binary, similarity)",
    "chao" = "Chao (binary, similarity)",
    hillebrand = "SER_a (rel abundance)",
    hillebrand_dis_scaled = "Dissimilarity",
    total = "Total turnover (codyn)",
    appearance = "Appearance",
    appearance_scaled = "Appearance",
    disappearance_scaled = "Disappearance",
    shannon = "Shannon",
    simpson = "Simpson",
    nestedness = "Nestedness (jaccard)",
    nestedness_scaled = "Nestedness",
    turnover = "Turnover",
    turnover_scaled = "Turnover",
    species_nb_nat = "Species richness (native)",
    species_nb_exo = "Species richness (exotic)",
    perc_exo_sp = "Non-native richness",
    perc_nat_sp = "Native richness",
    perc_nat_abun = "Native abundance",
    perc_exo_abun = "Non-native abundance",
    nat_abun = "Native abundance",
    exo_abun = "Non-native abundance"
      )
}

get_model_term_replacement <- function() {
  c(
    log1_year_nb = "Log (Year nb + 1)",
    year_nb = "Year nb",
    `year` = "Year",
    riv_str_rc1 = "PCA1\nstream gradient",
    hft_c9309_scaled_no_center = "Human footprint\nchange (1993-2009)",
    hft_ix_c9309_diff_scaled = "Human footprint\nchange (1993-2009)",
    hft_ix_c9309_log2_ratio = "Log2 Human footprint\nratio (2009/1993)",
    hft_ix_c93 = "Human footprint\n(1993)",
    main_bas = "Basin",
    siteid = "Site",
    `siteid:main_bas` = "Site",
    `:` = ":\n"
  )
}

get_model_term_replacement_vulgarisation <- function() {
  c(
    log1_year_nb = "Time",
    year_nb = "Time",
    riv_str_rc1 = "PCA1\nstream gradient",
    hft_c9309_scaled_no_center = "Recent change",
    hft_ix_c9309_diff_scaled = "Recent change",
    hft_ix_c9309_log2_ratio = "Recent change",
    hft_ix_c93 = "Legacy",
    main_bas = "Basin",
    siteid = "Site",
    `siteid:main_bas` = "Site",
    `:` = " x\n"

  )
}

get_model_term_replacement_paper_figure <- function() {
  c(
    log1_year_nb = "Time",
    riv_str_rc1 = "Stream gradient",
    hft_c9309_scaled_no_center = "Recent pressures",
    hft_ix_c9309_diff_scaled = "Recent pressures",
    hft_ix_c9309_log2_ratio = "Recent pressures",
    hft_ix_c93 = "Past pressures",
    main_bas = "Basin",
    unitabundance = "Abundance unit",
    siteid = "Site",
    `siteid:main_bas` = "Site",
    `:` = " x\n"

  )
}

get_river_atlas_significant_var <- function() {
  c(
    #"Length of the reach" = "length_km",
    "Distance from source (km)" = "dist_up_km",
    "Strahler order" = "ord_stra",
    #"Order flow" = "ord_flow",
    "Average elevation (m)" = "ele_mt_cav",
    "Annual average of temperature (°C)" = "tmp_dc_cyr",
    #"Annual maximum of temperature (°C)" = "tmp_dc_cmx",
    #"Annual minimum of temperature (°C)" = "tmp_dc_cmn",
    "Average slope (degree)" = "slp_dg_cav",
    "Annual average of discharge (m3/s)" = "dis_m3_pyr",
    #"Annual maximum of discharge (m3/s)" = "dis_m3_pmx",
    #"Annual minimum of discharge (m3/s)" = "dis_m3_pmn",
    #"River area (reach segment, in ha)" = "ria_ha_csu",
    #"River volume (reach segment, m3)" = "riv_tc_csu",
    "Protected area extent (%)" = "pac_pc_cse",
    "Urban extent (%)" = "urb_pc_cse",
    "Human footprint ratio 2009/1993" = "hft_ix_c9309_ratio",
    "Log2 Human footprint ratio (2009/1993)" = "hft_ix_c9309_log2_ratio",
    "Human footprint index 1993" = "hft_ix_c93",
    "Human footprint index 2009" = "hft_ix_c09"
  )
}

get_rev_vec_name_val <- function(x = NULL) {
  y <- names(x)
  names(y) <- x
  return(y)
}

get_hft_riv_var <- function() {
  c(
    log_dist_up_km = "Log source distance (km)",
    hft_c9309_scaled_no_center = "Human footprint\nchange (93-09)\n scaled no center",
    hft_ix_c9309_diff_scaled = "Human footprint\nchange (93-09)\nscaled",
    hft_ix_c93 = "Human footprint 93",
    hft_ix_c09 = "Human footprint 09",
    hft_ix_c9309_log2_ratio = "Human footprint\nlog2 ratio (93-09)"
  )

}

get_land_class_var <- function() {
  c(
    #  "Potential Natural Vegetation Extent (catchment)" = "pnv_pc_c01-c15",
    #  "Potential Natural Vegetation Extent (watershed)" = "pnv_pc_u01-u15",
    "Forest Cover Extent (%, catchment)" = "for_pc_cse",
    "Forest Cover Extent (%, watershed)" = "for_pc_use",
    "Cropland Extent (%, catchment)" = "crp_pc_cse",
    "Cropland Extent (%, watershed)" = "crp_pc_use",
    "Pasture Extent (%, catchment)" = "pst_pc_cse",
    "Pasture Extent (%, watershed)" = "pst_pc_use",
    "Urban Extent (%, catchment)" = "urb_pc_cse",
    "Urban Extent (%, watershed)" = "urb_pc_use",
    "Population Density (people per km2, catchment)" = "ppd_pk_cav",
    "Population Density (watershed)" = "ppd_pk_uav"
  )
}

get_unitabun_replacement <- function() {
  c(
    "Ind.100m2" = "Individual number per 100m2",
    "CPUE" = "Catch Per Unit Effort (CPUE)"
  )
}

get_nat_origin_replacement <- function() {
  c(
    "tedesco" = "Tedesco database (basin scale)",
    "autofishbase" = "Fishbase (country scale)",
    "handmade" = "Atlas, fishbase (country scale)",
    "usgs" = "NAS database (state scale, USA)"
  )
}

replacement_random_term <- function() {
  c(
    "Gaussian observations" = "Error",
    "intercept_main_bas" = "Intercept (basin)",
    "main_bas1" = "Time (basin)",
    "intercept_main_bassiteid" = "Intercept (site nested in basin)",
    "siteid1" = "Time (site nested in basin)"
  )
}

replacement_random_term_table <- function() {
  c(
    #"Gaussian observations" = "Error",
    #"intercept_main_bas" = "Basin",
    "main_bas1" = "Basin",
    #"intercept_main_bassiteid" = "Intercept (site nested in basin)",
    "siteid1" = "Site"
  )
}

replacement_random_term_bis <- function() {
  c(
    "Gaussian observations" = "epsilon"
  )
}

get_pred_signification <- function() {

  c(
    "0" = "Year nb of sample for a site = 0, i.e. baseline year",
    "10" = "Year nb of sample for a site = 10, i.e. a decade",
    "intact" = "Median intact site (human footprint index between 1 and 4, Williams et al. 2020)",
    "max" = "Most degraded site in the dataset, i.e. highest human footprint index",
    "median" = "Median stream gradient",
    "div/2" = "Anthropogenic pressures divided by two between 1993 and 2009",
    "inc/2" = "Anthropogenic pressures multipled by two between 1993 and 2009"
  )
}

get_term_level <- function() {
  c(
    "Time",
    "Past pressures",
    "Recent pressures",
    "Stream gradient",
    "Time x\nPast pressures",
    "Time x\nRecent pressures",
    "Time x\nStream gradient",
    "Time x\nPast pressures x\nRecent pressures",
    "Time x\nStream gradient x\nPast pressures",
    "Time x\nStream gradient x\nRecent pressures"
  )

}
