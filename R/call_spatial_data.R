
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

