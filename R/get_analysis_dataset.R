get_analysis_dataset <- function(
  filtered_dataset = NULL,
  chao_hillnb = NULL,
  hillebrand = NULL,
  turnover_c = NULL,
  vegdist_turnover_c = NULL
  ) {

  analysis_dataset <- filtered_dataset$abun_rich_op %>%
    left_join(filtered_dataset$site_quali, by = "siteid") %>%
    left_join(filtered_dataset$location, by = "siteid") %>%
    left_join(select(chao_hillnb, op_id, chao_richness, chao_shannon, chao_simpson), by = "op_id") %>%
    left_join(hillebrand, by = c("siteid", "year")) %>%
    left_join(turnover_c, by = c("siteid", "year")) %>%
    left_join(vegdist_turnover_c, by = c("siteid", "year")) %>%
    mutate(total_abundance_int = as.integer(total_abundance))

  year_stat_site <- filtered_dataset$site_quanti %>%
    filter(variable == "year") %>%
    select(siteid, min, max) %>%
    mutate(span = max + 1 - min) %>%
    rename(first_year = min, last_year = max)

  analysis_dataset <- analysis_dataset %>%
    left_join(year_stat_site, by = "siteid")

  return(analysis_dataset)

}
