
get_site_location_as_sf <- function(add_protocol = FALSE) {

  tar_load(site_desc_loc)
  tar_load(site_protocol_quali)

  if (add_protocol) {
    site_desc_loc  <- site_desc_loc %>%
    left_join(site_protocol_quali %>%
      select(siteid, protocol, protocol_detail, unitabundance), by = "siteid")
  }
  output <- st_as_sf(site_desc_loc, coords = c("longitude", "latitude"),
    crs = 4326)
  return(output)

}

map_world_trends <- function(
  df = NULL,
  y = NULL,
  world = ne_countries(returnclass = "sf"),
  range_per = .90
  ) {
  bb <- sf::st_bbox(df)

  if (!is.null(range_per)) {
    alpha2 <- (1 - range_per) / 2
    qu <- quantile(df[[y]], c(alpha2, 1 - alpha2))

    mask <- df[[y]] >= qu[1] & df[[y]] <= qu[2]

    df <- df[mask, ]
  }

      ggplot(data = world) +
      geom_sf() +
      geom_sf(
        data = df,
        aes_string(color = y), shape = 19
        ) +
      coord_sf(
        xlim = bb[c("xmin", "xmax")],
        ylim = bb[c("ymin", "ymax")],
        expand = FALSE
        ) +
      scale_color_distiller(palette = "Spectral", type = "div") +
      theme(legend.position = "bottom")
}
