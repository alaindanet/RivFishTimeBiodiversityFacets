#' Wrapper about untb
#'
#' @value list of two elements: sim (object return by untb) and mat_abun
#' (community matrix)
#'
get_untb_abun_mat <- function(
  start = sample(1:3, size = 64, replace = TRUE),
  prob = 0.001,
  D = 1,
  gens = 1300,
  sparse=TRUE
  ) {

  sim <- untb::untb(
    start = start,
    prob = prob,
    D = D,
    gens = gens,
    keep = TRUE
  )

  species <- unique(unlist(apply(sim, 1, unique)))

  mat_sim <- matrix(0,
    nrow = nrow(sim), ncol = length(species),
    dimnames = list(seq(gens), species)
  )

  for (i in seq_along(sim[, 1])) {
    tmp_abun <- table(sim[i, ])
    mat_sim[i, names(tmp_abun)] <- tmp_abun
  }

  if (sparse) {
   mat_sim <- Matrix::Matrix(mat_sim, sparse = TRUE)
  }

  return(
    list(
      sim = sim,
      abun_mat = mat_sim
    )
  )

}

target_untb <- function(filtered_dataset = NULL) {

  rich_quant <- summary_distribution(
    filtered_dataset$abun_rich$species_nb)
  count_site <- filtered_dataset$site_quali %>%
    filter(unitabundance == "Count") %>%
    .[["siteid"]]
  tot_abun_quant <- summary_distribution(
    filtered_dataset$abun_rich %>%
      filter(siteid %in% count_site) %>%
      .$total_abundance
  )

  neutral_com <- expand.grid(
    richness = rich_quant[c("1st_quart", "median", "2nd_quart")],
    abun = tot_abun_quant[c("1st_quart", "median", "2nd_quart")],
    death_rate = c(.01, .1),
    immigration = c(.01, .1, .7),
    n = seq(10)) %>%
  rowid_to_column()

neutral_com <- neutral_com  %>%
  rowwise(rowid) %>%
  mutate(sim = list(
      get_untb_abun_mat(
        start = sample(seq(richness), size = abun, replace = TRUE),
        D = death_rate * abun,
        prob = immigration,
        gens = 500,
        sparse = TRUE

        )$abun_mat
    )
  ) %>%
  ungroup()

  return(neutral_com)

}
