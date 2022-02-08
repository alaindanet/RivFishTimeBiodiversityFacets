source("./packages.R")

## Load your R files
lapply(list.files(here("R"), full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(
  # tar_target(target2, function_to_make2(arg)) ## targets style
  tar_target(
    raw_data_file,
    get_raw_file_path(),
    format = "file",
    error = "continue"),
  tar_target(riveratlas_shp_files,
    get_shp_files(dir = here("inst", "extdata", "RiverATLAS_v10_shp")),
    error = "continue"
    ),
  tar_target(water_temperature_file,
    here("inst", "extdata", "waterTemperature_Global_monthly_1979-2014.nc"),
    format = "file",
    error = "continue"
    ),
  tar_target(timeseries, load_time_series_data(raw_data_file)),
  tar_target(site_desc_loc, get_site_desc_loc(ts_data = timeseries)),
  tar_target(abun_rich_op, get_abun_rich_op(ts_data = measurement)),
  tar_target(species_number_site,
    get_species_number_site(
      ts_data = timeseries,
      species_number_by_op = abun_rich_op,
      op_protocol = op_protocol)),
  tar_target(species_status, get_species_status(ts_data = timeseries)),
  tar_target(op_protocol, get_op_protocol(ts_data = timeseries)),
  tar_target(site_protocol_quali, get_site_quali_quanti_protocol(
      op_data = op_protocol, type = "quali")),
  tar_target(site_protocol_quanti, get_site_quali_quanti_protocol(
      op_data = op_protocol, type = "quanti")),
  tar_target(measurement, get_abundance_biomass_data(ts_data = timeseries)),
  tar_target(toy_dataset, get_toy_dataset(
      measurement = measurement,
      op_protocol = op_protocol,
      site_protocol_quanti = site_protocol_quanti,
      seed = 123)),
  tar_target(filtered_op_protocol, filter_op(
      op_protocol = op_protocol,
      selected_protocol = NULL,
      selected_abun_unit = NULL,
      nb_sampling = 5,
      span_min = 10,
      extent_month = 1.5,
      convert_month_to_date = TRUE,
      return_no_filtered = FALSE
    )
    ),
  tar_target(filtered_dataset, get_filtered_dataset(
      op_protocol = filtered_op_protocol,
      type = "all",
      measurement = measurement,
      site_desc_loc = site_desc_loc,
      add_var_from_protocol = c("siteid", "year")
      )),
  tar_target(measurement_avg3y, tar_avg_first_year_measurement(
      dataset = filtered_dataset$measurement,
      nb_sampling_to_average = 3
      ) %>%
    select(-siteid, -year)
    ),
  tar_target(filtered_dataset_avg3y, get_filtered_dataset(
      op_protocol = filtered_op_protocol,
      type = "all",
      measurement = measurement_avg3y,
      site_desc_loc = site_desc_loc,
      add_var_from_protocol = c("siteid", "year")
      )),
  # Community structure
  tar_target(neutral_com, target_untb(filtered_dataset = filtered_dataset)),
  tar_target(neutral_turnover,
    neutral_com %>%
      mutate(
       jaccard = purrr::map(
         sim,
         ~get_vegdist_temporal_turnover_c(
           mat = as.matrix(.x[250:500, ]),
           method = "jaccard",
           return_tibble = TRUE,
           drop_first_year = FALSE
         )
       )
      ) %>%
    select(-sim)
  ),
  tar_target(com_mat_site,
    get_site_community_matrix(
      x = filtered_dataset$measurement,
      average_first_year = FALSE,
      nb_sampling_to_average = NULL 
    )
    ),
  tar_target(com_mat_site_avg3y,
    get_site_community_matrix(
      x = filtered_dataset$measurement,
      average_first_year = TRUE,
      nb_sampling_to_average = 3
    )
    ),
  tar_target(com_mat_basin,
    get_basin_year_community_matrix(
      x = filtered_dataset$measurement,
      loc = filtered_dataset$location,
      min_nb_site_by_basin = 5,
      min_year_by_basin = 10
    )
    ),
  tar_target(com_mat_basin_no_filter,
    get_basin_year_community_matrix(
      x = filtered_dataset$measurement,
      loc = filtered_dataset$location,
      min_nb_site_by_basin = NULL,
      min_year_by_basin = NULL
    )
    ),
  tar_target(filtered_com_mat_basin,
    target_filter_com_mat_basin(
      com_mat_basin = com_mat_basin,
      min_nb_sampling_by_site = 10,
      min_nb_site_by_basin = 10,
      clean = TRUE
    )),
  tar_target(vegdist_index, c("jaccard", "horn", "chao")),
  tar_target(vegdist_turnover,
    target_vegdist_turnover(
      dataset = com_mat_site,
      method = vegdist_index,
      return_tibble = TRUE,
      drop_first_year = FALSE
      ),
    pattern = map(vegdist_index),
    iteration = "list"),
  tar_target(vegdist_turnover_c,
    reduce(vegdist_turnover, left_join, by = c("siteid", "year"))
    ),
  tar_target(vegdist_turnover_avg3y,
    target_vegdist_turnover(
      dataset = com_mat_site_avg3y,
      method = vegdist_index,
      return_tibble = TRUE,
      drop_first_year = FALSE
      ),
    pattern = map(vegdist_index),
    iteration = "list"),
  tar_target(vegdist_turnover_avg3y_c,
    reduce(vegdist_turnover_avg3y, left_join, by = c("siteid", "year"))
    ),
  tar_target(turnover_types_chr, c("total", "appearance", "disappearance")),
  tar_target(turnover_time_to_time,
      get_turnover(x = filtered_dataset$measurement, type = turnover_types_chr),
    pattern = map(turnover_types_chr),
    iteration = "list"
  ),
  tar_target(turnover_time_to_time_c,
    turnover %>% reduce(left_join, by = c("siteid", "year"))
  ),
  tar_target(hillebrand_time_to_time,
    get_hillebrand_turnover(x = filtered_dataset$measurement)
  ),
  tar_target(hillebrand,
    target_custom_temporal_turnover(
      dataset = com_mat_site,
      fun = compute_hillebrand,
      var_name = "hillebrand",
      mat_col = "mat_rel",
      return_tibble = TRUE,
      drop_first_year = FALSE,
      similarity = TRUE
    )
  ),
  tar_target(hillebrand_avg3y,
    target_custom_temporal_turnover(
      dataset = com_mat_site_avg3y,
      fun = compute_hillebrand,
      var_name = "hillebrand",
      return_tibble = TRUE,
      drop_first_year = FALSE,
      similarity = TRUE
    )
  ),
  tar_target(turnover,
    target_custom_temporal_turnover(
      dataset = com_mat_site,
      fun = compute_codyn_turnover,
      var_name = turnover_types_chr,
      return_tibble = TRUE,
      drop_first_year = FALSE,
      type = turnover_types_chr
      ),
    pattern = map(turnover_types_chr),
    iteration = "list"),
  tar_target(baselga_types_chr, c("jaccard", "nestedness", "turnover")),
  tar_target(baselga,
    target_custom_temporal_turnover(
      dataset = com_mat_site,
      fun = compute_jaccard_decomp,
      var_name = baselga_types_chr,
      return_tibble = TRUE,
      drop_first_year = FALSE,
      type = baselga_types_chr
      ),
    pattern = map(baselga_types_chr),
    iteration = "list"),
  tar_target(turnover_c,
    turnover %>% reduce(left_join, by = c("siteid", "year"))
  ),
  tar_target(baselga_c,
    baselga %>% reduce(left_join, by = c("siteid", "year"))
  ),
  tar_target(turnover_avg3y,
    target_custom_temporal_turnover(
      dataset = com_mat_site_avg3y,
      fun = compute_codyn_turnover,
      var_name = turnover_types_chr,
      return_tibble = TRUE,
      drop_first_year = FALSE,
      type = turnover_types_chr
      ),
    pattern = map(turnover_types_chr),
    iteration = "list"),
  tar_target(turnover_avg3y_c,
    turnover_avg3y %>% reduce(left_join, by = c("siteid", "year"))
  ),
  tar_target(chao_hillnb,
    get_chao_hillnb(
      x = filtered_dataset$measurement,
      coverage = .985,
      confidence_int = NULL,
      adjust_abun_density = TRUE)
    ),
  tar_target(hillnb,
    get_hillnb(
      x = filtered_dataset$measurement,
      dataset = filtered_op_protocol),
    ),
  tar_target(chao_hillnb_avg3y,
    get_chao_hillnb(
      x = filtered_dataset_avg3y$measurement,
      coverage = .985,
      confidence_int = NULL,
      adjust_abun_density = TRUE)
    ),
  tar_target(hillnb_avg3y,
    get_hillnb(
      x = filtered_dataset_avg3y$measurement,
      dataset = filtered_op_protocol),
    ),

  tar_target(analysis_dataset,
    get_analysis_dataset(
      filtered_dataset = filtered_dataset,
      chao_hillnb = chao_hillnb,
      hillebrand = hillebrand,
      turnover_c = turnover_c,
      vegdist_turnover_c = vegdist_turnover_c,
      hillnb = hillnb,
      baselga_c = baselga_c,
      river = riveratlas_site[,
        colnames(riveratlas_site) %in%
          c("siteid", setNames(get_river_atlas_significant_var(), NULL))
        ]
    )
    ),
  tar_target(fr,
    analysis_dataset %>%
      filter(country == "FRA") %>%
      select(all_of(
          c(
            "siteid","ecoregion", "main_bas", "year",
            var_temporal_trends,
            setNames(get_river_atlas_significant_var(), NULL)
          )
          )) %>%
      na.omit() %>%
      mutate(
        jaccard_scaled = transform01(jaccard),
        main_bas = as.factor(main_bas),
        scaled_dist_up_km = scale(dist_up_km),
        log_dist_up_km = log(dist_up_km),
        scaled_tmp_dc_cyr = scale(tmp_dc_cyr),
        tmp_c_cyr = tmp_dc_cyr / 10,
        scaled_tmp_c_cyr = scale(tmp_c_cyr)
        ) %>%
      group_by(siteid) %>%
      mutate(year_nb = year - min(year)) %>%
      ungroup()
    ),
  tar_target(mod_wt_data,
    wt_mv_avg  %>%
      left_join(
        filtered_dataset$location %>%
          select(siteid, ecoregion, main_bas) %>%
          mutate(main_bas = as.character(main_bas)),
        by = "siteid"
      )),
  tar_target(analysis_dataset_avg3y,
    get_analysis_dataset(
      filtered_dataset = filtered_dataset_avg3y,
      chao_hillnb = chao_hillnb_avg3y,
      hillebrand = hillebrand_avg3y,
      turnover_c = turnover_avg3y_c,
      vegdist_turnover_c = vegdist_turnover_avg3y_c,
      hillnb = hillnb_avg3y
    )
    ),
  # statistic
  tar_target(biodiv_facets, c("total_abundance_int", "species_nb",
      "chao_richness", "chao_shannon", "chao_simpson")),
#  tar_target(spamm_com_models,
#    run_save_spamm_model(
#      resp_var = biodiv_facets,
#      rhs_formula = "year + unitabundance + (1 | ecoregion / siteid) + (year |
#      first_year)",
#      dataset = analysis_dataset,
#      family = "negbin"
#    ),
#    pattern = map(biodiv_facets),
#    iteration = "list"
#  ),
  # Temporal trends
  tar_target(var_temporal_trends,
    c("total_abundance", "log_total_abundance", "species_nb", "log_species_nb",
      "chao_richness", "chao_shannon", "chao_simpson", "chao_evenness",
      "jaccard", "horn", "chao", "hillebrand", "total", "appearance",
      "disappearance", "evenness", "shannon", "simpson",
      "jaccard_dis", "nestedness", "turnover")
    ),
  tar_target(rigal_trends,
    get_rigal_trajectory_classification(
      analysis_dataset,
      y_var = var_temporal_trends,
      x_var = "year", site_id = "siteid",
      backtransform_linear_coefs = TRUE),
    pattern = map(var_temporal_trends),
    iteration = "list"
    ),
tar_target(rigal_slp_df,
           get_rigal_slope_df(
             rigal_trends = rigal_trends,
             var_names = var_temporal_trends)),
tar_target(slp_env,
           get_rigal_analysis_dataset(
             rigal_df = rigal_slp_df,
              river = riveratlas_site[,
                colnames(riveratlas_site) %in% c("siteid", get_river_atlas_significant_var())],
           spatial = select(filtered_dataset$location, siteid, ecoregion, main_bas)
           )
           ),
  tar_target(rigal_trends_avg3y,
    get_rigal_trajectory_classification(
      analysis_dataset_avg3y,
      y_var = var_temporal_trends,
      x_var = "year", site_id = "siteid"),
    pattern = map(var_temporal_trends),
    iteration = "list"
    ),
  #tar_target(simple_lm,
      #analysis_dataset %>%
        #select(all_of(c("siteid", "year", var_temporal_trends))) %>%
        #group_by(siteid) %>%
        #nest() %>%
        #mutate(model = purrr::map(
            #data,
            #~lm(as.formula(paste0(var_temporal_trends, " ~ year ")), data = .x)
      #)
          #) %>%
        #select(-data),
    #pattern = map(var_temporal_trends),
    #iteration = "list"
    #),
  #tar_target(simple_lm_coef,
      #simple_lm %>%
        #mutate(coef_mod = purrr::map(model, broom::tidy)) %>%
        #select(-model),
    #pattern = map(simple_lm),
    #iteration = "list"
  #),
  tar_target(snapped_site_river,
    target_snap_site_to_river(
      river_shp_filepath = get_full_file_name(riveratlas_shp_files),
      site_sf = filtered_dataset$location %>%
        st_as_sf(coords = c("longitude", "latitude"), crs = 4326),
      proj_crs =  4087,
      length_chunk = 200,
      max_dist = 1000
      ),
    pattern = map(riveratlas_shp_files),
    iteration = "list"
    ),
  tar_target(riveratlas_site,
    target_extract_riveratlas_info(
      river_shp_files = map_chr(riveratlas_shp_files,
        ~get_full_file_name(filename = .x)),
      snap_list = snapped_site_river) %>%
      janitor::clean_names()
    ),

  tar_target(water_temperature,
    extract_water_temperature_values(
      raster_path = water_temperature_file,
      site = filtered_dataset$location %>%
        st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
      )),
  tar_target(wt,
    format_water_temperature(
      wt = water_temperature,
      siteid = filtered_dataset$location$siteid,
      raster_path = water_temperature_file) %>%
    filter_water_temperature(
      wt = .,
      raw_tmp_threshold = 40,
      nb_sd_threshold = 5
    )
    ),
  tar_target(wt_mv_avg, get_moving_average_tmp(wt = wt)),
  tar_target(mod_wt,
    glmmTMB(
      tmp_w_ama ~ year * ecoregion +
        (1 + year | main_bas / siteid),
      data = mod_wt_data)),
  tar_target(spde, make_spde(loc = filtered_dataset$location)),
  tar_target(inla_rich, try(inla(
        species_nb ~
          year +
          f(siteid, model = "iid") +
          f(main_bas, model = "iid") +
          f(span, year, model = "iid"),
        family = "zeroinflatednbinomial1",
        control.family = list(link = "log"),
        control.predictor = list(link = 1, compute = TRUE),
        control.compute = list(
          cpo = TRUE,
          dic = TRUE,
          config = TRUE,
          return.marginals.predictor = TRUE),
        data = analysis_dataset,
        verbose = TRUE
        )), error = "continue"),
  tar_target(inla_abun, inla(
      total_abundance ~
        year +
        unitabundance +
        f(siteid, model = "iid") +
        f(span, year, model = "iid"),
      family = "gaussian",
      #control.family = list(link = "log"),
      control.predictor = list(link = 1, compute = TRUE),
      control.compute = list(
        cpo = TRUE,
        dic = TRUE,
        config = TRUE,
        return.marginals.predictor = TRUE),
      data = analysis_dataset,
      verbose = TRUE
      ), error = "continue"),
  tar_target(inla_test,
    inla(y ~ x,
      family = "gaussian",
      data = list(
        x = rnorm(100, mean = 6, sd = 2),
        y = rnorm(100, mean = 6, sd = 1)
        ),
      control.predictor = list(link = 1)
    )
    ),
  tar_target(trend_env,
    model_rigal_spamm(
      formula = paste0(var_temporal_trends,
        " ~ ",
        "dist_up_km + tmp_dc_cyr +
        (1 + dist_up_km + tmp_dc_cyr | ecoregion/main_bas)"
      ),
    data = slp_env),
  pattern = map(var_temporal_trends),
  iteration = "list"),
tar_target(beta_jaccard_tmb,
  temporal_jaccard(
    formula = "jaccard_scaled ~
    0 + year_nb/scaled_dist_up_km +
    (0 + year_nb | main_bas/siteid) +
    (0 + year_nb | span) +
    (0 + year_nb:scaled_dist_up_km | main_bas)",
  data = na.omit(analysis_dataset),
  family = beta_family(link = "logit"),
  dispformula = "~ year_nb + scaled_dist_up_km",
  offset = rep(1.0, nrow(na.omit(analysis_dataset)))
  )
  ),
tar_target(gaussian_jaccard_tmb,
  temporal_jaccard(
    formula = "jaccard_scaled ~
    0 + year_nb/scaled_dist_up_km +
    (0 + year_nb | main_bas/siteid) +
    (0 + year_nb | span) +
    (0 + year_nb:scaled_dist_up_km | main_bas)
  ",
  data = na.omit(analysis_dataset),
  family = gaussian(link = "identity"),
  dispformula = "~ 1",
  offset = rep(1.0, nrow(na.omit(analysis_dataset)))
  )
  ),
tar_target(nb_sp_rich_tmb,
  glmmTMB::glmmTMB(species_nb ~
    year_nb * scaled_dist_up_km +
    (1 + year_nb | main_bas / siteid) +
    (1 + scaled_dist_up_km | main_bas),
  offset = NULL,
  dispformula = ~ main_bas + siteid,
  family = nbinom2(link = "log"),
  data = na.omit(analysis_dataset))
  ),

  # Report
  tar_render(intro, here("vignettes/intro.Rmd")),
  tar_render(report, here("doc/aa-research-questions.Rmd")),
  tar_render(raw_data_watch, here("doc/ab-raw-data.Rmd")),
  tar_render(filtered_data_watch, here("doc/ac-data-filtering.Rmd")),
  tar_render(community_structure, "doc/aca-community-structure.Rmd"),
  tar_render(trends_report, here("doc/ad-temporal-trends.Rmd")),
  tar_render(meeting_report, "doc/xx-meeting-report.Rmd"),
  tar_render(meeting_slides, here("talk/meeting.Rmd")),
  tar_render(explain_high_turnover,
    here("doc/af-explain-high-turnover.Rmd"))

)
