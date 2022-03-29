get_cluster_df <- function(
  tclust_obj = NULL,
  site_env = NULL,
  assign_threshold = .1,
  clean_method = "rm"
  ) {

  stopifnot(clean_method %in% c("na", "rm", "0", NULL))

  site_cl <- tclust_obj$par$x %>%
    as.data.frame() %>%
    rownames_to_column("siteid") %>%
    as_tibble() %>%
    mutate(cl = as.integer(tclust_obj$cluster)) %>%
    left_join(
      site_env %>%
        select(siteid, riv_str_rc1, hft_c9309_scaled_no_center),
      by = "siteid"
    )

    doubtful_assign_mask <-
      tclust::DiscrFact(tclust_obj)$assignfact >
    log(assign_threshold)

    if (is.null(clean_method)) {

    } else if (clean_method == "rm") {
      site_cl <- site_cl %>%
        filter(!doubtful_assign_mask) %>%
        filter(cl != 0)
    } else if (clean_method == "na") {
      site_cl[site_cl$cl == 0, ]$cl <- NA_integer_
      site_cl[doubtful_assign_mask, ]$cl <- NA_integer_
    } else if (clean_method == "0") {
      site_cl[doubtful_assign_mask, ]$cl <- 0
    }

    return(site_cl)

}

plot_cluster_proportion <- function(
  cluster_df = NULL,
  site_env = NULL,
  loc_var = ecoregion
){

  cl_region <- cluster_df %>%
    select(siteid, cl) %>%
    mutate(cl = as.factor(cl)) %>%
    left_join(select(site_env, siteid, {{loc_var}}), by = "siteid") %>%
    filter(! ecoregion %in% c("Afrotropics", "Neotropics"))


  p_nb_cl <-  cl_region %>%
    ggplot(aes(y = {{loc_var}}, fill = cl)) +
    geom_bar()

  cl_region_pc <- cl_region %>%
    group_by({{loc_var}}, cl) %>%
    summarise(freq = n()) %>%
    mutate(pc = prop.table(freq)) %>%
    ungroup()

  p_prop_cl <- cl_region_pc %>%
    ggplot(aes(x = {{loc_var}}, y = pc, fill = cl)) +
    geom_col() +
    geom_text(aes(label = scales::percent(pc)),
      position="stack",vjust=+2.1,col="firebrick",size=3)+
    scale_y_continuous(label = scales::percent)

  return(list(nb_cl = p_nb_cl, prop_cl = p_prop_cl))
}

plot_loc_cluster <- function(
  cluster_df = NULL,
  world_site = NULL,
  pays = NULL
) {

  site_cl_sf <- world_site$site %>%
    left_join(cluster_df[, c("siteid", "cl")], by = "siteid")

  if (!is.null(pays)) {
    site_cl_sf <- site_cl_sf %>%
      filter(country == pays) 
    stbbox <- site_cl_sf %>%
      st_bbox()
    world <- filter(world_site$world, iso_a3 == pays)
  } else {
    world <- world_site$world
  }

  p <- ggplot(data = world) +
    geom_sf() +
    geom_sf(
      data = site_cl_sf,
      aes(color = as.factor(cl)),
      shape = 16, size = 3) +
    theme(legend.position = "bottom")

  if (!is.null(pays)) {
    p <- p +  
      coord_sf(
        xlim = stbbox[c("xmin", "xmax")],
        ylim = stbbox[c("ymin", "ymax")],
        expand = FALSE
      )
  }

  return(p)

}
