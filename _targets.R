library(targets)

## Load your R files
lapply(list.files(here::here("R"), full.names = TRUE), source)
tar_option_set(packages = c(
    "targets", "tarchetypes","tidyverse", "magrittr",
    "lubridate", "here", "kableExtra", "scales", "rmarkdown", "sf",
    "rnaturalearth", "rnaturalearthdata", "terra", "cowplot", "viridis",
    "janitor", "codyn", "vegan", "slider", "future", "INLA", "inlatools",
    "glmmTMB", "easystats", "ggeffects", "tclust", "clValid"))

library(future.callr)
plan(callr)
library(targets)
source("./packages.R")

#future::plan(future::multisession)

## tar_plan supports drake-style targets and also tar_target()
list(
  # tar_target(target2, function_to_make2(arg)) ## targets style
  tar_target(raw_data_file,
    get_raw_file_path(),
    format = "file",
    error = "continue"),
  tar_target(riveratlas_shp_files,
    get_shp_files(dir = here("inst", "extdata", "RiverATLAS_v10_shp")),
    error = "continue"
    ),
  tar_target(water_temperature_file,
    here("inst", "extdata", "waterTemperature_Global_monthly_1979-2014.nc") %>%
      R.utils::filePath(., expandLinks = "any"),
    format = "file",
    error = "continue"
    ),
  tar_target(basin_tedesco_shp,
     here("inst", "extdata", "Tedesco_2017", "Basin042017_3119.shp") %>%
      R.utils::filePath(., expandLinks = "any"),
    format = "file",
    error = "continue"
    ),
  tar_target(occ_exotic_file,
    R.utils::filePath(here("inst", "extdata", "Tedesco_2017", "Occurrence_Table_12092019.csv"),
    expandLinks = "any")
    ),
  tar_target(timeseries,
    load_time_series_data(raw_data_file)
    ),
  tar_target(occ_exotic,
    read_csv(occ_exotic_file,
      col_types = list(`5.Fishbase.Species.Code` = col_character())) %>%
    janitor::clean_names() %>%
    rename_with(~str_remove(.x, "^x\\d_")) %>%
    mutate(species = str_replace_all(fishbase_valid_species_name, "\\.", " "))),
  tar_target(basin_tedesco,
    read_sf(basin_tedesco_shp) %>%
      clean_names()
    ),
  tar_target(measurement_exo,
    get_measurement_exo(
      occ_exotic = occ_exotic,
      exo_basin_site = exo_basin_site,
      measurement = filtered_dataset$measurement
      ) %>%
    complete_native_exotic_data(
      meas = .,
      loc = site_desc_loc
    )),
  tar_target(abun_rich_exo,
    get_abun_rich_exo(measurement_exo = measurement_exo)
    ),
  tar_target(filtered_abun_rich_exo,
    get_filtered_abun_rich_exo(
      abun_rich = abun_rich_exo,
      perc_na_abun_thld = 0.05,
      min_nb_sampling_by_site = 5)
    ),
  tar_target(site_desc_loc,
    get_site_desc_loc(ts_data = timeseries)),
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
      )
    ),
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
tar_target(world_site_sf, 
  get_world_site_sf(loc = filtered_dataset$location)
  ),
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
  tar_target(baselga_avg3y,
    target_custom_temporal_turnover(
      dataset = com_mat_site_avg3y,
      fun = compute_jaccard_decomp,
      var_name = baselga_types_chr,
      return_tibble = TRUE,
      drop_first_year = FALSE,
      type = baselga_types_chr
      ),
    pattern = map(baselga_types_chr),
    iteration = "list"),
  tar_target(baselga_avg3y_c,
    baselga_avg3y %>% reduce(left_join, by = c("siteid", "year"))
    ),
  tar_target(chao_hillnb,
    get_chao_hillnb(
      x = filtered_dataset$measurement,
      coverage = .985,
      confidence_int = NULL,
      adjust_abun_density = TRUE)
    ),
  tar_target(chao_hillnb_cov80,
    get_chao_hillnb(
      x = filtered_dataset$measurement,
      coverage = .80,
      confidence_int = NULL,
      adjust_abun_density = TRUE)
    ),
  tar_target(chao_hillnb_cov1,
    get_chao_hillnb(
      x = filtered_dataset$measurement,
      coverage = 1,
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
        ] %>%
          st_drop_geometry()
        ,
        pca_riv_str = pca_riv_str
    )
    ),
  tar_target(fr,
    analysis_dataset %>%
      filter(country == "FRA") %>%
      select(all_of(
          c(
            "siteid", "ecoregion", "main_bas", "year",
            var_temporal_trends,
            setNames(get_river_atlas_significant_var(), NULL)
          )
          )) %>%
      na.omit()
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
        baselga_c = baselga_avg3y_c,
        turnover_c = turnover_avg3y_c,
        vegdist_turnover_c = vegdist_turnover_avg3y_c,
        hillnb = hillnb_avg3y,
        river = riveratlas_site[,
          colnames(riveratlas_site) %in%
            c("siteid", setNames(get_river_atlas_significant_var(), NULL))
          ] %>%
            st_drop_geometry(),
          pca_riv_str = pca_riv_str
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
  tar_target(exo_basin_site,
    match_tedesco_basin_site(
      site = world_site_sf$site,
      basin = basin_tedesco
      )
  ),
  tar_target(riveratlas_total,
    get_full_riveratlas(
      river_shp_files = map_chr(riveratlas_shp_files,
        ~get_full_file_name(filename = .x)),
      river_id = "HYRIV_ID",
      var_to_collect = get_river_atlas_significant_var() 
    )
    ),
  tar_target(p_atlas_rivfishtime_env,
    target_plot_rivatlas_rivfishtime_env(
      riveratlas_total = riveratlas_total,
      riveratlas_site = riveratlas_site,
      variable = c(setNames(get_river_atlas_significant_var(), NULL),
        "hft_ix_c9309_diff", "hft_ix_c9309_log2_ratio")
    )),
  tar_target(water_temperature,
    extract_water_temperature_values(
      raster_path = water_temperature_file,
      site = filtered_dataset$location %>%
        st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
      )),
  tar_target(formated_wt,
    format_water_temperature(
      wt = water_temperature,
      siteid = filtered_dataset$location$siteid,
      raster_path = water_temperature_file)
    ),
  tar_target(wt,
    filter_water_temperature(
      wt = formated_wt,
      raw_tmp_threshold = 40,
      nb_sd_threshold = 5
    )
    ),
  tar_target(wt_mv_avg, get_moving_average_tmp(wt = wt)),
  tar_target(wt_mv_avg_roll, get_mv_avg_rollapplyr(wt = wt)),
  tar_target(write_temperature_mv_avg,
    write_csv(full_join(at_mv_avg, wt_mv_avg, by = c("siteid", "year")),
      file = here("data", "awt.csv")),
    error = "continue"
    ),

  tar_target(mod_wt,
    glmmTMB(
      tmp_w_ama ~ year * ecoregion +
        (1 + year | main_bas / siteid),
      data = mod_wt_data)),
  tar_target(spde, make_spde(loc = filtered_dataset$location)),

  tar_target(air_temperature,
    target_extract_chelsa_data(
      chelsa_shp_files = list.files("L://ENV_LAYERS/CHELSA", full.names = TRUE),
      site = filtered_dataset$location %>%
        st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
      )),
  tar_target(formated_air_temperature,
    format_air_temperature(air_temperature)
    ),
  tar_target(at_mv_avg,
    get_moving_average_tmp(
      wt = formated_air_temperature,
      var_y = "tmp_c",
      output_tmp_var = "tmp_a_ana")
    ),
  tar_target(at_mv_avg_roll,
    get_mv_avg_rollapplyr(
      wt = formated_air_temperature,
      var_y = "tmp_c")
    ),
  tar_target(pca_riv_str,
    compute_riv_str_pca(riv = riveratlas_site, ncomp = 2)
    ),
  tar_target(p_pca_riv_str,
    plot_rotated_pca(pca_rotated = pca_riv_str)
    ),
  # tar_target(inla_rich, try(inla(
  #       species_nb ~
  #         year +
  #         f(siteid, model = "iid") +
  #         f(main_bas, model = "iid") +
  #         f(span, year, model = "iid"),
  #       family = "zeroinflatednbinomial1",
  #       control.family = list(link = "log"),
  #       control.predictor = list(link = 1, compute = TRUE),
  #       control.compute = list(
  #         cpo = TRUE,
  #         dic = TRUE,
  #         config = TRUE,
  #         return.marginals.predictor = TRUE),
  #       data = analysis_dataset,
  #       verbose = TRUE
  #       )), error = "continue"),
  # tar_target(inla_abun, inla(
  #     total_abundance ~
  #       year +
  #       unitabundance +
  #       f(siteid, model = "iid") +
  #       f(span, year, model = "iid"),
  #     family = "gaussian",
  #     #control.family = list(link = "log"),
  #     control.predictor = list(link = 1, compute = TRUE),
  #     control.compute = list(
  #       cpo = TRUE,
  #       dic = TRUE,
  #       config = TRUE,
  #       return.marginals.predictor = TRUE),
  #     data = analysis_dataset,
  #     verbose = TRUE
  #     ), error = "continue"),
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
        (1 + dist_up_km + tmp_dc_cyr | ecoregion/main_bas)"),
      data = slp_env),
    pattern = map(var_temporal_trends), iteration = "list"),
  tar_target(var_analysis,
    c(
      "siteid", "main_bas", "year", "year_nb", "log1_year_nb",
      "scaled_dist_up_km", "span",
      "jaccard_dis", "jaccard_dis_scaled",
      "turnover", "turnover_scaled", "nestedness", "nestedness_scaled",
      "species_nb", "log_species_nb", "species_nb_tps", "species_nb_tps_scaled",
      "chao_richness", "log_chao_richness", "chao_richness_tps", "chao_richness_tps_scaled",
      "unitabundance",
      "total_abundance", "total_abundance_tps", "log_total_abundance", "total_abundance_scaled",
      "hillebrand", "hillebrand_dis", "hillebrand_dis_scaled",
      "appearance", "appearance_scaled", "disappearance",
      "disappearance_scaled", "evenness", "evenness_scaled", "riv_str_rc1", "hft_ix_c93",
      "hft_ix_c9309_diff_scaled", "hft_c9309_scaled_no_center", "hft_ix_c9309_log2_ratio")
    ),
  tar_target(modelling_data,
    analysis_dataset %>%
      select(all_of(var_analysis)) %>%
      na.omit()
    ),
  tar_target(modelling_data_exo,
    get_modelling_data_exo(
      abun_rich = filtered_abun_rich_exo,
      model_data = modelling_data,
      ana_data = analysis_dataset
    )),
  tar_target(site_env,
    modelling_data %>%
      filter(siteid %in% row.names(site_no_drivers)) %>% 
      group_by(siteid) %>%
      summarise(across(where(is.numeric), mean), .groups = "drop") %>%
      arrange(match(siteid, row.names(site_no_drivers))) %>%
      left_join(filtered_dataset$location, by = "siteid")
    ),

  tar_target(var_jaccard,
    c("jaccard_dis_scaled", "turnover_scaled",
      "nestedness_scaled", "hillebrand_dis_scaled", "appearance_scaled",
      "disappearance_scaled", "evenness_scaled")),
  tar_target(year_var, c("year_nb", "log1_year_nb")),
  tar_target(intercept, c(0, 1)),
  tar_target(beta_jaccard_tmb,
    tibble(
      year_var = year_var,
      intercept = intercept,
      response = var_jaccard,
      mod = list(
        temporal_jaccard(
          formula = paste0(var_jaccard, " ~ ",
            intercept, " + ",
            year_var," * riv_str_rc1 + ",
            year_var," * hft_ix_c9309_diff_scaled +
            (", intercept, " + ", year_var," | main_bas/siteid) +
            (", intercept, " + ", year_var," | span) +
            (", intercept, " + ", year_var, ":hft_ix_c9309_diff_scaled | main_bas) +
            (", intercept, " + ", year_var,":riv_str_rc1 | main_bas)"),
          data = modelling_data,
          family = beta_family(link = "logit"),
          dispformula = "~ year_nb + riv_str_rc1"
          ))),
    pattern = cross(var_jaccard, year_var, intercept)),
  tar_target(gaussian_jaccard_tmb,
    tibble(
      year_var = year_var,
      intercept = intercept,
      response = var_jaccard,
      mod = list(
        temporal_jaccard(
          formula = paste0(var_jaccard, " ~ ",
            intercept, " + ",
            year_var," * riv_str_rc1 +",
            year_var, " * hft_ix_c9309_diff_scaled +
            (", intercept, " + ", year_var," | main_bas/siteid) +
            (", intercept, " + ", year_var," | span) +
            ( 0 + ", year_var, ":hft_ix_c9309_diff_scaled | main_bas) +
            ( 0 + ", year_var,":riv_str_rc1 | main_bas)"),
          data = modelling_data,
          offset = NULL,
          family = gaussian(link = "identity"),
          dispformula = "~ 1"
          )
        )),
    pattern = cross(var_jaccard, year_var, intercept)
    ),
  tar_target(gaussian_jaccard_tmb_simple,
    tibble(
      response = var_jaccard,
      year_var = year_var,
      mod = list(
        temporal_jaccard(
          formula = paste0(var_jaccard, " ~
            ", year_var, " * riv_str_rc1 +",
          year_var, " * hft_ix_c9309_diff_scaled +
          (1 + ", year_var," | main_bas:siteid) +
          (1 + 
            ", year_var, " +
            hft_ix_c9309_diff_scaled +
            riv_str_rc1 +",
          year_var,":riv_str_rc1 +",
          year_var,":hft_ix_c9309_diff_scaled | main_bas)"),
        data = modelling_data,
        offset = NULL,
        family = gaussian(link = "identity"),
        dispformula = "~ 1")
        )),
    pattern = cross(var_jaccard, year_var)
    ),
  tar_target(rich_var, c("chao_richness", "species_nb", "log_species_nb", "chao_richness_tps_scaled", "species_nb_tps_scaled")),
  tar_target(gaussian_rich_tmb,
    tibble(
      response = rich_var,
      year_var = year_var,
      mod = list(glmmTMB(
          formula = as.formula(paste0(rich_var, " ~
              ", year_var, " * riv_str_rc1 +",
            year_var, " * hft_ix_c9309_diff_scaled +
            (1 + ", year_var," | main_bas/siteid) +
            (1 + ", year_var," | span) +
            (0 + ", year_var, ":hft_ix_c9309_diff_scaled | main_bas) +
            (0 + riv_str_rc1 + ", year_var,":riv_str_rc1 | main_bas)")),
          data = modelling_data,
          family = gaussian(link = "identity")
          ))),
    pattern = cross(rich_var, year_var)
    ),
  tar_target(gaussian_rich_tmb_simple,
    tibble(
      response = rich_var,
      year_var = year_var,
      mod = list(
        temporal_jaccard(
          formula = paste0(rich_var, " ~
            ", year_var, " * riv_str_rc1 +",
          year_var, " * hft_ix_c9309_diff_scaled +
          (1 + ", year_var," | main_bas:siteid) +
          (1 + ", year_var, "+
            hft_ix_c9309_diff_scaled +
            riv_str_rc1 +",
          year_var,":riv_str_rc1 +",
          year_var,":hft_ix_c9309_diff_scaled | main_bas)"),
        data = modelling_data,
        offset = NULL,
        family = gaussian(link = "identity"),
        dispformula = "~ 1"
          ))),
    pattern = cross(rich_var, year_var)
    ),
  tar_target(rich_jaccard_var, c(rich_var, var_jaccard)),
  tar_target(gaussian_rich_jaccard_tmb_no_int_re,
    tibble(
      response = rich_jaccard_var,
      year_var = year_var,
      mod = list(try(glmmTMB(formula = as.formula(
              paste0(rich_jaccard_var, " ~
                ", year_var, " * riv_str_rc1 +",
              year_var, " * hft_ix_c9309_diff_scaled +
              (1 + ", year_var," | main_bas:siteid) +
              (1 + ", year_var, "+
                hft_ix_c9309_diff_scaled +
                riv_str_rc1 | main_bas)")),
              data = modelling_data,
              family = gaussian(link = "identity")
              )))
          ),
        pattern = cross(rich_jaccard_var, year_var)
        ),
      tar_target(gaussian_rich_jaccard_tmb_no_evt_re,
        tibble(
          response = rich_jaccard_var,
          year_var = year_var,
          mod = list(try(glmmTMB(
                formula = as.formula(paste0(rich_jaccard_var, " ~
                    ", year_var, " * riv_str_rc1 +",
                  year_var, " * hft_ix_c9309_diff_scaled +
                  (1 + ", year_var," | main_bas:siteid) +
                  (1 + ", year_var, "| main_bas)")),
                data = modelling_data,
                family = gaussian(link = "identity"))
                )
                )),
            pattern = cross(rich_jaccard_var, year_var)
            ),
          tar_target(abun_var, c(
              "total_abundance", "total_abundance_scaled",
              "log_total_abundance",
              "total_abundance_tps")),
          tar_target(gaussian_abun_tmb,
            tibble(
              response = abun_var,
              year_var = year_var,
              mod = list(
                try(glmmTMB(
                    formula = as.formula(paste0(abun_var, " ~
                        ", year_var, " * riv_str_rc1 +",
                      year_var, "* unitabundance +",
                      year_var, " * hft_ix_c9309_diff_scaled +
                      (1 + ", year_var, " | main_bas/siteid) +
                      (1 + ", year_var, " | span) +
                      (0 + hft_ix_c9309_diff_scaled + ", year_var, ":hft_ix_c9309_diff_scaled | main_bas) +
                      (0 + riv_str_rc1 + ", year_var, ":riv_str_rc1 | main_bas)")),
                    data = modelling_data,
                    family = gaussian(link = "identity")
                    ))
                    )),
                pattern = cross(abun_var, year_var)
                ),
              tar_target(gaussian_abun_tmb_simple,
                tibble(
                  response = abun_var,
                  year_var = year_var,
                  mod = list(
                    try(glmmTMB(
                        formula = as.formula(paste0(abun_var, " ~
                            ", year_var, " * riv_str_rc1 +",
                          year_var, "* unitabundance +",
                          year_var, " * hft_ix_c9309_diff_scaled +
                          (1 + hft_ix_c9309_diff_scaled + ", year_var, ":hft_ix_c9309_diff_scaled +  riv_str_rc1 + ", year_var, ":riv_str_rc1 | main_bas) +
                          (1 + ", year_var, " | main_bas:siteid) 
                        ")),
                        data = modelling_data,
                        family = gaussian(link = "identity")
                        )))),
                pattern = cross(abun_var, year_var)
                ),
              tar_target(gaussian_abun_tmb_no_int_re,
                tibble(
                  response = abun_var,
                  year_var = year_var,
                  mod = list(
                    try(glmmTMB(
                        formula = as.formula(paste0(abun_var, " ~
                            ", year_var, " * riv_str_rc1 +",
                          year_var, "* unitabundance +",
                          year_var, " * hft_ix_c9309_diff_scaled +
                          (1 + ", year_var, "+  hft_ix_c9309_diff_scaled +  riv_str_rc1 | main_bas) +
                          (1 + ", year_var, " | main_bas:siteid) 
                        ")),
                        data = modelling_data,
                        family = gaussian(link = "identity")
                        )))),
                pattern = cross(abun_var, year_var)
                ),
              tar_target(gaussian_abun_tmb_no_evt_re,
                tibble(
                  response = abun_var,
                  year_var = year_var,
                  mod = list(
                    try(glmmTMB(
                        formula = as.formula(paste0(abun_var, " ~
                            ", year_var, " * riv_str_rc1 +",
                          year_var, "* unitabundance +",
                          year_var, " * hft_ix_c9309_diff_scaled +
                          (1 + ", year_var, "| main_bas) +
                          (1 + ", year_var, " | main_bas:siteid) 
                        ")),
                        data = modelling_data,
                        family = gaussian(link = "identity")
                        )))),
                pattern = cross(abun_var, year_var)
                ),
              ## More coherent modelling by response variable caracteristics
  tar_target(tps_var, c("jaccard_dis_scaled", "turnover_scaled",
      "nestedness_scaled", "hillebrand_dis_scaled", "appearance_scaled",
      "disappearance_scaled", "chao_richness_tps_scaled",
      "species_nb_tps_scaled", "total_abundance_tps")),
  tar_target(gaussian_tps,
    tibble(
      response = tps_var,
      mod = list(try(glmmTMB(
              formula = get_formula_tps(resp = tps_var),
              data = modelling_data,
              family = gaussian(link = "identity"))
              )
        )),
    pattern = map(tps_var)
      ),
  tar_target(log_rich_var, c("log_species_nb", "log_chao_richness")),
  tar_target(gaussian_rich,
    tibble(
      response = log_rich_var,
      mod = list(try(glmmTMB(
            formula = get_formula_no_tps(resp = log_rich_var),
            data = modelling_data,
            family = gaussian(link = "identity"))))
      ),
    pattern = map(log_rich_var)
    ),
  tar_target(gaussian_abun,
    tibble(
      response = "log_total_abundance",
      mod = list(try(glmmTMB(
            formula = get_formula_abun(resp = "log_total_abundance"),
            data = modelling_data,
            family = gaussian(link = "identity"))))
      )
    ),
  tar_target(facet_var, c(tps_var, log_rich_var, "log_total_abundance")),
  tar_target(gaussian_no_drivers,
    tibble(
      response = facet_var,
      mod = list(try(glmmTMB(
            formula = fun_no_driver_formula(x = facet_var),
            data = modelling_data,
            family = gaussian(link = "identity"))))
      ),
    pattern = map(facet_var)
    ),
  tar_target(gaussian_no_drivers_scale,
    tibble(
      response = facet_var,
      mod = list(try(glmmTMB(
            formula = fun_no_driver_formula(x = facet_var),
            data = modelling_data %>%
              mutate(across(all_of(facet_var), ~scale(.)[,1])),
            family = gaussian(link = "identity"))))
      ),
    pattern = map(facet_var)
    ),
  tar_target(gaussian_int_env,
    tibble(
      response = facet_var,
      mod = list(try(glmmTMB(
            formula = fun_int_env_formula(x = facet_var),
            data = modelling_data,
            family = gaussian(link = "identity"))))
      ),
    pattern = map(facet_var)
    ),
  tar_target(gaussian_log_hft,
    tibble(
      response = facet_var,
      mod = list(try(glmmTMB(
            formula = fun_hft_formula(x = facet_var,
              int4env = FALSE,
              hft_var = "hft_ix_c9309_log2_ratio"),
            data = modelling_data,
            family = gaussian(link = "identity"))))
      ),
    pattern = map(facet_var)
    ),
  tar_target(gaussian_int_env_comp_std,
    # Drop the main effect
    compare_parameters(
      setNames(gaussian_int_env$mod, gaussian_int_env$response),
      standardize = "refit")
    ),
  tar_target(mod_comp_std_df,
    gaussian_int_env_comp_std %>%
      as_tibble() %>%
      select(Parameter, starts_with("CI_"), starts_with("Coefficient")) %>%
      pivot_longer(-Parameter, names_to = "names", values_to = "values") %>%
      separate(col = names, into  = c("type", "response"), sep = "\\.") %>%
      pivot_wider(names_from = "type", values_from = "values") %>%
      filter(
        !Parameter %in% "(Intercept)",
        !str_detect(Parameter, "unitabundance"),
        response %in% clust_var
        ) %>%
      mutate(
        response = get_var_replacement()[response],
        Parameter = str_replace_all(Parameter,
          "([a-z|0-9])\\s([a-z|0-9])", "\\1_\\2"),
        Parameter = str_remove_all(Parameter, "[()]"),
        Parameter = str_replace_all(Parameter, get_model_term_replacement()),
        facet = ifelse(
          str_detect(Parameter, "\\*"),
          ifelse(
            str_count(Parameter, "\\*") == 2,
            "dbl_interaction", "interaction"),
          "main")
      )

    ),
  tar_target(p_mod_comp_std,
    plot_model_comp_interaction(
      mod_comp_df = mod_comp_std_df
      )),
  tar_target(mod_exo_comp_std_df,
    mod_exo_comp_std %>%
      as_tibble() %>%
      select(Parameter, starts_with("CI_"), starts_with("Coefficient")) %>%
      pivot_longer(-Parameter, names_to = "names", values_to = "values") %>%
      separate(col = names, into  = c("type", "response"), sep = "\\.") %>%
      pivot_wider(names_from = "type", values_from = "values") %>%
      filter(
        !Parameter %in% "(Intercept)",
        !str_detect(Parameter, "unitabundance")
        ) %>%
      mutate(
        response = get_var_replacement()[response],
        Parameter = str_replace_all(Parameter,
          "([a-z|0-9])\\s([a-z|0-9])", "\\1_\\2"),
        Parameter = str_remove_all(Parameter, "[()]"),
        Parameter = str_replace_all(Parameter, get_model_term_replacement()),
        facet = ifelse(
          str_detect(Parameter, "\\*"),
          ifelse(
            str_count(Parameter, "\\*") == 2,
            "dbl_interaction", "interaction"),
          "main")
      )
    ),
  tar_target(p_mod_exo_comp_std,
    plot_model_comp_interaction(
      mod_comp_df = mod_exo_comp_std_df
      )),
  tar_target(binded_gaussian,
    rbind(gaussian_tps, gaussian_rich, gaussian_abun)
    ),
  tar_target(gaussian_comp_std,
    # Drop the main effect
    compare_parameters(setNames(binded_gaussian$mod, binded_gaussian$response), standardize = "refit")
    ),
  tar_target(gaussian_coef,
    map_dfr(setNames(binded_gaussian$mod, binded_gaussian$response),
      ~broom.mixed::tidy(.x), .id = "response"
      )),
  tar_target(gaussian_re_sd,
    gaussian_coef %>%
      filter(
        effect == "ran_pars",
        group != "Residual",
        term == "sd__log1_year_nb",
        response %in% clust_var
        ) %>%
    select(response, group, estimate) %>%
    mutate(
      group = get_model_term_replacement()[group],
      response = get_var_replacement()[response]
      )),
  tar_target(p_re_sd,
    gaussian_re_sd %>%
      ggplot(aes(x = estimate, y = response, color = group)) +
      geom_point(size  = 3) +
      theme_minimal() +
      labs(x = "Random effect on temporal trends (standard deviation)") +
      theme(
        axis.title.y = element_blank(),
        panel.background = element_rect(colour = "white"),
        legend.title = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal"
        )),
  tar_target(gaussian_re_self_c,
    binded_gaussian %>%
      mutate(
        random_site = map(mod,
          ~try(get_random_effect_glmmTMB(
              .x, effect = "siteid:main_bas")
            )),
        random_basin = map(mod,
          ~try(get_random_effect_glmmTMB(
              .x, effect = "main_bas")
            ))
        ) %>%
      select(-mod)
    ),
  tar_target(gaussian_no_drivers_re_self_c,
    gaussian_no_drivers %>%
      mutate(
        random_site = map(mod,
          ~try(get_random_effect_glmmTMB(
              .x, effect = "siteid:main_bas")
            )),
        random_basin = map(mod,
          ~try(get_random_effect_glmmTMB(
              .x, effect = "main_bas")
            ))
        ) %>%
      select(-mod)
    ),
  tar_target(tps_model_for_comp,
    tibble(
      response = tps_var,
      mod = list(model_comp_interp_int(resp = tps_var, df = modelling_data))
      ) %>%
      mutate(p = purrr::map(mod, ~plot_model_comp_coeff(model_list = .x))),
    pattern = map(tps_var)
    ),
  tar_target(p_int_mod,
    make_interaction_heatmap_tps_env(data = mod_comp_std_df)
    ),
  tar_target(mod_tmb,
    rbind(
      gaussian_jaccard_tmb %>%
        filter(intercept == 1 & year_var == "year_nb") %>%
        select(-intercept), 
      gaussian_rich_tmb %>%
        filter(year_var == "year_nb"),
      gaussian_abun_tmb %>%
        filter(year_var == "year_nb")) %>%
    filter(!response %in% c("species_nb", "log_species_nb"))),
  tar_target(mod_tmb_comp,
    # Drop the main effect
    compare_parameters(setNames(mod_tmb$mod, mod_tmb$response), drop = "^scaled")
    ),
  tar_target(mod_tmb_comp_std,
    # Drop the main effect
    compare_parameters(
      setNames(mod_tmb$mod, mod_tmb$response), standardize = "basic")
    ),
  tar_target(vif,
    glmmTMB(
      log_chao_richness ~
        log1_year_nb + riv_str_rc1 +
        hft_ix_c9309_log2_ratio +
        hft_ix_c93 +
        (1 + log1_year_nb | main_bas/siteid),
      modelling_data
      ) %>%
    check_collinearity()),
  tar_target(exo_resp_var,
    c("species_nb", "log_species_nb",
      "species_nb_nat", "species_nb_exo",
      "perc_exo_sp", "perc_nat_sp", "perc_exo_abun",
      "perc_nat_abun",
      "total_abundance", "log_total_abundance",
      "nat_abun", "exo_abun"
      )),
  tar_target(gaussian_exo,
    tibble(
      response = exo_resp_var,
      mod = list(try(glmmTMB(
            formula = fun_int_env_formula_exo(x = exo_resp_var),
            data = modelling_data_exo,
            family = gaussian(link = "identity"))))
      ),
    pattern = map(exo_resp_var)
    ),
tar_target(mod_exo_comp,
    # Drop the main effect
    compare_parameters(
      setNames(gaussian_exo$mod, gaussian_exo$response),
      standardize = "basic")
    ),
  tar_target(mod_exo_comp_std,
    # Drop the main effect
    compare_parameters(
      setNames(gaussian_exo$mod, gaussian_exo$response),
      standardize = "refit")
    ),
  # Binding
  tar_target(binded_gaussian_tmb,
    rbind(gaussian_jaccard_tmb,
      mutate(gaussian_rich_tmb, intercept = 1),
      mutate(gaussian_abun_tmb, intercept = 1)
      )),
  tar_target(binded_gaussian_tmb_simple,
    rbind(gaussian_jaccard_tmb_simple,
      gaussian_rich_tmb_simple,
      gaussian_abun_tmb_simple
      )),
  tar_target(binded_gaussian_tmb_no_evt_re,
    rbind(gaussian_rich_jaccard_tmb_no_evt_re,
      gaussian_abun_tmb_no_evt_re)),
  tar_target(binded_gaussian_tmb_no_int_re,
    rbind(gaussian_rich_jaccard_tmb_no_int_re,
      gaussian_abun_tmb_no_int_re)),

  # Random effects
  tar_target(random_effects,
    binded_gaussian_tmb %>%
      mutate(random_effects = map(mod,
          ~try(
            parameters(
              .x,
              group_level = TRUE
              ) %>%
            as_tibble() %>%
            clean_names() %>%
            select(-all_of(c("se", "ci", "component", "effects")))
          )
      )
        ) %>%
    select(-mod)
  ),
tar_target(random_effect_self_c,
  binded_gaussian_tmb %>%
    filter(intercept == 1, year_var == "year_nb")  %>%
    mutate(
      random_site = map(mod,
        ~try(get_random_effect_glmmTMB(
            .x, effect = "siteid:main_bas")
          )),
      random_basin = map(mod,
        ~try(get_random_effect_glmmTMB(
            .x, effect = "main_bas")
          )),
      random_span = map(mod,
        ~try(get_random_effect_glmmTMB(
            .x, effect = "span")
          )),
      ) %>%
    select(-mod)
  ),

#tar_target(pred_gaussian,
  #binded_gaussian %>%
    #mutate(pred_riv = furrr::future_map(
        #mod,
        #~ggemmeans(.x,
          #terms = c("log1_year_nb", "riv_str_rc1 [quart2]"),
          #type = "fe")
        #),
      #pred_plot_riv = furrr::future_map(pred_riv, plot),
      #pred_hft = furrr::future_map(
        #mod, 
        #~ggemmeans(.x,
          #terms = c("log1_year_nb", "hft_ix_c9309_diff_scaled [quart2]"),
          #type = "fe")
        #),
      #pred_plot_hft = furrr::future_map(pred_hft, plot)
      #) %>%
  #select(-mod)
#),
  tar_target(clust_var,
    c("log_total_abundance", "log_chao_richness",
      "jaccard_dis_scaled", "hillebrand_dis_scaled",
      "appearance_scaled", "disappearance_scaled",
      "turnover_scaled", "nestedness_scaled")
    ),
  tar_target(basin_no_drivers,
    na.omit(
      get_random_effect_df(
        x = gaussian_no_drivers_re_self_c,
        type = "basin",
        resp = clust_var,
        modelling_data = modelling_data
     )
   )
   ),
 tar_target(basin_tps,
   na.omit(
     get_random_effect_df(
        x = gaussian_re_self_c,
        type = "basin",
        resp = clust_var,
        modelling_data = modelling_data
     )
   )
   ),
 tar_target(site_tps,
   na.omit(
     get_random_effect_df(
        x = gaussian_re_self_c,
        type = "site",
        resp = clust_var,
        modelling_data = modelling_data
        )
   )
   ),
 tar_target(site_no_drivers,
   na.omit(
     get_random_effect_df(
       x = gaussian_no_drivers_re_self_c,
       type = "site",
       resp = clust_var,
       modelling_data = modelling_data
     )
   )
   ),
 tar_target(clust_curv_site,
   tclust::ctlcurves(
     x = scale(site_no_drivers, center = FALSE),
     k = 1:12,
     alpha = seq(0, .3, by = .05),
     restr.fact = 1
     )
   ),
 tar_target(clust_curv_site_fac_50,
   tclust::ctlcurves(
     x = scale(site_no_drivers, center = FALSE),
     k = 1:12,
     alpha = seq(0, .3, by = .05),
     restr.fact = 50 
     )
   ),
 tar_target(k6_fac_1, 
   tclust(
     x = scale(site_no_drivers, center = FALSE),
     iter.max = 100, 
     k = 6,
     alpha = 0.05,
     restr.fact = 1,
     warnings = 2
   )
   ),
 tar_target(k6_fac_50,
   tclust(
     x = scale(site_no_drivers, center = FALSE),
     iter.max = 100, 
     k = 6,
     alpha = 0.05,
     restr.fact = 50,
     warnings = 2
   )
   ),
 tar_target(k7_fac_1,
   tclust(scale(site_no_drivers, center = FALSE), iter.max = 100, 
     k = 7, alpha = 0.05, restr.fact = 1)),
 tar_target(k12_fac_1,
   tclust(x = scale(site_no_drivers, center = FALSE),
     iter.max = 100, 
     k = 12, alpha = 0.05,
     restr.fact = 1, warnings = 2)
 ),
 tar_target(discr_k6_fac_1, DiscrFact(k6_fac_1, threshold = 0.5)),
 tar_target(discr_k6_fac_50, DiscrFact(k6_fac_50, threshold = 0.5)),
 tar_target(discr_k12_fac_1, DiscrFact(k12_fac_1, threshold = 0.5)),
 tar_target(discr_k7_fac_1, DiscrFact(k7_fac_1, threshold = 0.5)),
 tar_target(clustering_site_check,
   tibble(
     type = c("internal", "stability"),
     clvalid = list(purrr::map(type,
         ~clValid::clValid(
           obj = scale(site_no_drivers, center = FALSE),
           nClust = 2:12,
           method = "ward",
           clMethods = c("hierarchical", "kmeans", "pam"),
           validation = .x,
           maxitems = 10000
         )
        )
     )  
   )
   ),
 tar_target(p_clust_prop,
   plot_cluster_proportion(
     cluster_df = site_cl_rm,
     site_env = site_env,
     loc_var = ecoregion,
     vjust = 2.5,
     size = 5
     )),
 tar_target(site_cl_0,
   get_cluster_df(
     tclust_obj = k6_fac_1,
     site_env = site_env,
     assign_threshold = .1,
     clean_method = "0"
     )),
 tar_target(site_cl_rm,
   get_cluster_df(
     tclust_obj = k6_fac_1,
     site_env = site_env,
     assign_threshold = .1,
     clean_method = "rm"
     )),
 tar_target(site_cl_na,
   get_cluster_df(
     tclust_obj = k6_fac_1,
     site_env = site_env,
     assign_threshold = .1,
     clean_method = "na"
     )),
 tar_target(bp_cl_dist,
   target_bp_cl_dist(cl_obj = site_cl_rm)
   ),
 tar_target(bp_random_effect,
   gaussian_re_sd %>%
     ggplot(aes(y = estimate, x = group, fill = group)) %>%
     make_custom_boxplot(., aes_col = group)
   ),
 tar_target(country_to_plot, c("USA", "FRA","GRB", "SWE")),
 tar_target(p_cluster_country,
   tibble(
     country = country_to_plot,
     p = list(plot_loc_cluster(
         cluster_df = site_cl_na,
         world_site = world_site_sf,
         pays = country_to_plot 
         ))),
   pattern = map(country_to_plot)
   ),
 tar_target(pca_clust, compute_rotated_pca(site_no_drivers)),
 tar_target(p_pca_clust,
   plot_pca_clust(
     .data = pca_clust$rotated,
     site_cl = site_cl_rm,
     replace_var = get_var_replacement(),
     size_arrows_segment = 1,
     label_size = 2.5,
     alpha_point = .2,
     lim_x_y = c(-3.5, 3.5),
     force_pull = 1,
     force = 80
   )),


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
   here("doc/af-explain-high-turnover.Rmd")),
 tar_render(biodiversity_facets_support,
   here("doc/ag-biodiversity-facets-support.Rmd")),
 tar_render(ah_clust_tps,
   here("doc/ah-clust-tps.Rmd"))


                )
