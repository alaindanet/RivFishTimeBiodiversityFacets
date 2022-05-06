get_analysis_dataset <- function(
  filtered_dataset = NULL,
  chao_hillnb = NULL,
  hillebrand = NULL,
  turnover_c = NULL,
  vegdist_turnover_c = NULL,
  hillnb = NULL,
  baselga_c = NULL,
  river = NULL,
  water_temperature = NULL,
  pca_riv_str = NULL
  ) {

  analysis_dataset <- filtered_dataset$abun_rich_op %>%
    left_join(filtered_dataset$site_quali, by = "siteid") %>%
    left_join(filtered_dataset$location, by = "siteid") %>%
    left_join(chao_hillnb %>%
        select(op_id, chao_richness, chao_shannon, chao_simpson, chao_evenness),
      by = "op_id"
      ) %>%
    left_join(hillebrand, by = c("siteid", "year")) %>%
    left_join(turnover_c, by = c("siteid", "year")) %>%
    left_join(vegdist_turnover_c, by = c("siteid", "year")) %>%
    mutate(total_abundance_int = as.integer(total_abundance)) %>%
    left_join(select(hillnb, -species_nb, -op_id),
      by = c("siteid", "year")
    )

    if (!is.null(baselga_c)) {
      analysis_dataset <- analysis_dataset %>%
        left_join(rename(baselga_c, jaccard_dis = jaccard),
          by = c("siteid", "year"))
    }

    if (!is.null(river)) {
      if(!is.null(pca_riv_str)) {
        river <- river %>%
            ## Add PCA score
            mutate(
              riv_str_rc1 =  pca_riv_str$rotated$scores[, "RC1"],
              riv_str_rc2 =  pca_riv_str$rotated$scores[, "RC2"]
            )
      }
      analysis_dataset <- analysis_dataset %>%
        left_join(river, by = c("siteid"))
    }

    if (!is.null(water_temperature)) {
      analysis_dataset <- analysis_dataset %>%
        left_join(water_temperature, by = "year", "tmp_w_ama")
    }


    year_stat_site <- filtered_dataset$site_quanti %>%
      filter(variable == "year") %>%
      select(siteid, min, max) %>%
      mutate(span = max + 1 - min) %>%
      rename(first_year = min, last_year = max)

    analysis_dataset <- analysis_dataset %>%
      left_join(year_stat_site, by = "siteid")

    # compute temporal richness
    temporal_richness <- get_richness_turnover(
      x = analysis_dataset,
      var_rich = c("chao_richness", "species_nb", "total_abundance"),
      suffix_var = "_tps"
    )

    analysis_dataset <- analysis_dataset %>%
      left_join(temporal_richness, by = c("siteid", "year"))

    # Variable transformation
    analysis_dataset <- analysis_dataset %>%
      mutate(
        # Response variables transformation
        jaccard_scaled = transform01(jaccard),
        jaccard_dis = 1 - jaccard,
        jaccard_dis_scaled = transform01(jaccard_dis),
        nestedness_scaled = transform01(nestedness),
        turnover_scaled = transform01(turnover),
        hillebrand_scaled = transform01(hillebrand),
        hillebrand_dis = 1 - hillebrand,
        hillebrand_dis_scaled = transform01(hillebrand_dis),
        appearance_scaled = transform01(appearance),
        disappearance_scaled = transform01(disappearance),
        evenness_scaled = transform01(evenness),
        log_chao_richness = log(chao_richness),
        chao_richness_tps_scaled = transform01(chao_richness_tps),
        species_nb_tps_scaled = transform01(species_nb_tps),

        # River atlas 
        scaled_dist_up_km = scale(dist_up_km),
        log_dist_up_km = log(dist_up_km),
        scaled_tmp_dc_cyr = scale(tmp_dc_cyr),
        tmp_c_cyr = tmp_dc_cyr / 10,
        scaled_tmp_c_cyr = scale(tmp_c_cyr),
        # In riveratlas, HFT is multiplied by 10
        hft_ix_c09 = hft_ix_c09 / 10,
        hft_ix_c93 = hft_ix_c93 / 10,
        hft_ix_c9309_percent =
          (hft_ix_c09 - hft_ix_c93) / hft_ix_c93 * 100,
        log_hft_ix_c9309_percent = hft_ix_c9309_percent,
        hft_ix_c9309_ratio = hft_ix_c09 / hft_ix_c93,
        hft_ix_c9309_log2_ratio = log2(hft_ix_c9309_ratio),
        hft_ix_c9309_diff = hft_ix_c09 - hft_ix_c93,
        hft_ix_c9309_diff_scaled = scale(hft_ix_c9309_diff),
        hft_c9309_scaled_no_center = scale(hft_ix_c9309_diff,
          center = FALSE),
        #riv_str_rc1 =  pca_riv_str$rotated$scores[, "RC1"],
        #riv_str_rc2 =  pca_riv_str$rotated$scores[, "RC2"],
        main_bas = main_bas,
        one = 1.0 #dummy offset
        ) %>%
    group_by(siteid) %>%
    mutate(
      year_nb = year - min(year),
      log1_year_nb = log(year_nb + 1)
      ) %>%
    ungroup() %>%
    group_by(unitabundance) %>%
      mutate(
        total_abundance_scaled = as.numeric(
          scale(total_abundance)
        )
        ) %>%
      ungroup()

  return(analysis_dataset)

}

get_rigal_slope_df <- function(
  rigal_trends = NULL,
  var_names = NULL) {
  
  names(rigal_trends) <- var_names
  slope <- map_dfr(rigal_trends,
                   ~.x %>%
                     select(siteid, linear_slope),
                   .id = "response"
  )
  slope_df <- slope %>%
    pivot_wider(names_from = "response", values_from = "linear_slope")
  
  return(slope_df)
  
}

get_rigal_analysis_dataset <- function(
  rigal_df = NULL,
  river = NULL,
  spatial = NULL
) {
   rigal_df %>%
    left_join(river, by = c("siteid")) %>%
    left_join(spatial, by = "siteid") %>%
    mutate(
      tmp_c_cyr = tmp_dc_cyr / 10,
      log_dist_up_km = log(dist_up_km),
      main_bas = as.factor(main_bas)
    )
}

get_modelling_data_exo <- function(
  abun_rich = filtered_abun_rich_exo,
  model_data = modelling_data,
  ana_data = analysis_dataset
) {

  var_modelling_data_4_exo_analysis <- c(
    "siteid", "main_bas",
    "year", "year_nb", "log1_year_nb",
    "unitabundance", "riv_str_rc1",
    "hft_ix_c93", "hft_ix_c9309_diff_scaled",
    "hft_c9309_scaled_no_center", "hft_ix_c9309_log2_ratio"
  )

  output <- abun_rich %>%
    left_join(ana_data %>%
      select(op_id, siteid, year),
      by = c("siteid", "op_id")
      ) %>%
  left_join(
    model_data[, var_modelling_data_4_exo_analysis],
    by = c("siteid", "year")
    ) %>%
  na.omit()

output  <- output %>%
  mutate(
    log_species_nb = log(species_nb),
    log_total_abundance = log(total_abundance),
  )

return(output)

}
