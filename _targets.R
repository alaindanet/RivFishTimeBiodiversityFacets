source("./packages.R")

## Load your R files
lapply(list.files(here("R"), full.names = TRUE), source)
plan(multisession, workers = 3)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(
  # tar_target(target2, function_to_make2(arg)) ## targets style
  tar_target(
    raw_data_file,
    here("inst", "extdata", "GlobalTimeSeries_database_1232021.csv"),
    format = "file",
    error = "continue"),
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
      nb_sampling = 10,
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
           drop_first_year = TRUE
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
      min_nb_site_by_basin = 10,
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
      drop_first_year = TRUE
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
      drop_first_year = TRUE
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
      return_tibble = TRUE,
      drop_first_year = TRUE,
      similarity = TRUE
    )
  ),
  tar_target(hillebrand_avg3y,
    target_custom_temporal_turnover(
      dataset = com_mat_site_avg3y,
      fun = compute_hillebrand,
      var_name = "hillebrand",
      return_tibble = TRUE,
      drop_first_year = TRUE,
      similarity = TRUE
    )
  ),
  tar_target(turnover,
    target_custom_temporal_turnover(
      dataset = com_mat_site,
      fun = compute_codyn_turnover,
      var_name = turnover_types_chr,
      return_tibble = TRUE,
      drop_first_year = TRUE,
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
      drop_first_year = TRUE,
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
      drop_first_year = TRUE,
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
      baselga_c = baselga_c
    )
    ),
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
      x_var = "year", site_id = "siteid"),
    pattern = map(var_temporal_trends),
    iteration = "list"
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

  # Report
  tar_render(intro, here("vignettes/intro.Rmd")),
  tar_render(report, here("doc/aa-research-questions.Rmd")),
  tar_render(raw_data_watch, here("doc/ab-raw-data.Rmd")),
  tar_render(filtered_data_watch, here("doc/ac-data-filtering.Rmd")),
  tar_render(community_structure, "doc/aca-community-structure.Rmd"),
  tar_render(trends_report, here("doc/ad-temporal-trends.Rmd")),
  tar_render(meeting_slides, here("talk/meeting.Rmd")),

)
