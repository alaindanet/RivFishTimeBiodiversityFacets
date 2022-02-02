get_analysis_dataset <- function(
  filtered_dataset = NULL,
  chao_hillnb = NULL,
  hillebrand = NULL,
  turnover_c = NULL,
  vegdist_turnover_c = NULL,
  hillnb = NULL,
  baselga_c = NULL,
  river = NULL
  ) {

  analysis_dataset <- filtered_dataset$abun_rich_op %>%
    left_join(filtered_dataset$site_quali, by = "siteid") %>%
    left_join(filtered_dataset$location, by = "siteid") %>%
    left_join(chao_hillnb %>%
      select(op_id, chao_richness, chao_shannon, chao_simpson, chao_evenness),
      by = "op_id") %>%
    left_join(hillebrand, by = c("siteid", "year")) %>%
    left_join(turnover_c, by = c("siteid", "year")) %>%
    left_join(vegdist_turnover_c, by = c("siteid", "year")) %>%
    mutate(total_abundance_int = as.integer(total_abundance)) %>%
    left_join(select(hillnb, -species_nb, -op_id),
      by = c("siteid", "year"))

  if (!is.null(baselga_c)) {
    analysis_dataset <- analysis_dataset %>%
      left_join(rename(baselga_c, jaccard_dis = jaccard),
        by = c("siteid", "year"))
  }

  if (!is.null(river)) {
    analysis_dataset <- analysis_dataset %>%
      left_join(river, by = c("siteid"))
  }
  year_stat_site <- filtered_dataset$site_quanti %>%
    filter(variable == "year") %>%
    select(siteid, min, max) %>%
    mutate(span = max + 1 - min) %>%
    rename(first_year = min, last_year = max)

  analysis_dataset <- analysis_dataset %>%
    left_join(year_stat_site, by = "siteid")

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
      log_dist_up_km = log(dist_up_km)
    )
 
}
