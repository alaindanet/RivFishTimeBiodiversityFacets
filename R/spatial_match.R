#' Span points on geometry
#'
#' @references https://github.com/r-spatial/sf/issues/792
#  https://stackoverflow.com/questions/51292952/snap-a-point-to-the-closest-point-on-a-line-segment-using-sf
st_snap_points <- function(
  x,
  y,
  max_dist = 1000,
  return_snapped_pt = FALSE,
  river_id = "HYRIV_ID"
    ) {

  if (inherits(x, "sf")) n <- nrow(x)
  if (inherits(x, "sfc")) n <- length(x)
  
  
  
  distance_tmp <- map(seq_len(n), ~st_nearest_points(x[.x,], y))
 
  near <- x %>%
    mutate(
      distance = distance_tmp,
      which_nearest = map_int(distance, get_nearest_pt, max_dist = max_dist),
      snapped_pt = pmap(
        list(distance, which_nearest, geometry),
        check_return_snap),
      riverid = map_int(which_nearest, ~ifelse(!is.na(.x), y[.x, ][[river_id]], NA))
    )

  if(return_snapped_pt) {
    
    out <- do.call(
      c,
      lapply(seq(n), function(i) {
        if (is.na(near$which_nearest[i])) {
          return(st_geometry(x)[i])
        }
        return(st_cast(near$distance[i][near$which_nearest[i], ], "POINT"))
    })
    )
    return(out)
    
  } else {
    return(near %>% select(siteid, riverid, which_nearest, snapped_pt))
  }

}

match_river_site <- function(
  river = NULL,
  site = NULL,
  max_dist = 1000,
  metric_crs = NULL,
  return_snapped_pt = FALSE,
  crop = TRUE,
  buffer_dist = NULL,
  col_id_river = "HYRIV_ID",
  site_id = "siteid"
) {

  if (!is.null(metric_crs)) {
    site <- sf::st_transform(site, crs = metric_crs)
    river <- sf::st_transform(river, crs = metric_crs)
  }


  if (is.null(buffer_dist)) {
    stopifnot(!is.null(max_dist))
    buffer_dist <- max_dist
  }


  # remove unnecessary rivers
  if (crop) {
    if (!col_id_river %in% colnames(river)) {
      river[[col_id_river]] <- seq_len(nrow(river))
    }
    pre_river_id <- river[[col_id_river]]
    river <- river %>%
      st_crop(st_bbox(st_buffer(site, dist = buffer_dist)))
  }

  nearest_snapped_pt <- st_snap_points(
    site,
    river,
    max_dist = max_dist,
    return_snapped_pt = return_snapped_pt
  )

  if (return_snapped_pt) {

    mask_na <- function(x, ...) {
      if(is.na(x)) {
        return(NA)
      } else {
        pre_river_id[pre_river_id == river[[col_id_river]][x]]
      }
    }

    out <- tibble(
      riverid = map_int(nearest_snapped_pt, mask_na),
      siteid = site[[site_id]]
    )
    return(out)

  } else {
    return(nearest_snapped_pt)
  }


}

get_nearest_pt <- function(snapped_pt = NULL, max_dist = NULL) {

  nrst_len <- st_length(snapped_pt)
  nrst_mn <- which.min(nrst_len)

  if (as.vector(nrst_len[nrst_mn]) > max_dist) {
    return(NA)
  }
  return(nrst_mn)
}

#' Snap sites on river atlas for every continent
#' 
#' Has parallel capacities with future::plan()
#' 
#' @param proj_crs integer default to 4087 because it keeps the distance well globally
#' https://epsg.org/crs_4087/WGS-84-World-Equidistant-Cylindrical.html
#' https://epsg.io/4087
target_snap_site_to_river <- function(
  river_shp_filepath = NULL,
  site_sf = NULL,
  proj_crs = 4087,
  length_chunk = 200,
  max_dist = 1000,
  river_id = "HYRIV_ID"
) {

  # load shapefile
  layer_name <- sf::st_layers(river_shp_filepath, do_count = TRUE)$name
  river <- sf::read_sf(river_shp_filepath,
    query = paste0("SELECT ", river_id," FROM ", layer_name)) %>%
    st_transform(crs = proj_crs)

  # crop shapefile
  crop_site <- site_sf %>%
    st_transform(crs = proj_crs) %>%
    st_crop(st_bbox(river)) %>%
    arrange(main_bas)

  # if no site in the river
  if(nrow(crop_site) == 0) {
    return(NA)
  }

  river_crop <- river %>%
    st_crop(st_bbox(crop_site))

  # Crop the river on buffered sites
  river_crop <- crop_on_buffer(
    to_crop = river_crop,
    x = crop_site,
    buffer_dist = max_dist
  )

  # cut the task by n times length_chunck
  ind <- seq_len(nrow(crop_site))
  nb_points_chuncks <- length_chunk
  chuncks <- split(ind, ceiling(seq_along(ind) / nb_points_chuncks))

  # Run
  correspondance <- furrr::future_map_dfr(
    chuncks,
    ~match_river_site(
      river = river_crop,
      site = crop_site[.x, ],
      max_dist = max_dist,
      metric_crs = NULL,
      return_snapped_pt = FALSE,
      crop = TRUE,
      buffer_dist = NULL,
      col_id_river = river_id,
      site_id = "siteid"
    )
  )
  return(correspondance)
}


check_return_snap <- function(d, nearest, origin){
  if (is.na(nearest)) {
    return(origin)
  }
  return(st_cast(d[[nearest]], "POINT"))
}

#' Crop an sf object on another with a buffer 
#'
#' @examples
#' ti <- st_sf(
#'   a = 3,
#'   geometry = st_sfc(st_point(1:2), st_point(3:4)))
#' tl <- st_sf(
#'   a = 3,
#'   geometry = st_sfc(st_linestring(pts), st_linestring(pts + 12))
#'   )
#' 
#' plot(g_buf)
#' plot(tl, add = T)
#' crop_on_buffer(to_crop = tl, x = ti, buffer_dist = 12)
crop_on_buffer <- function(
  to_crop = NULL,
  x = NULL,
  buffer_dist = NULL
  ) {

  x <- st_buffer(x, buffer_dist)
  to_crop <- to_crop[map_lgl(st_intersects(to_crop, x), any), ]

  return(to_crop)
}

extract_riveratlas_info <- function(
  shp_file = NULL,
  site = NULL,
  site_riverid = "riverid",
  river_id = "HYRIV_ID"
  ) {

  river <- sf::read_sf(shp_file)
  mask <- river[[river_id]] %in% site[[site_riverid]]
  river <- river[mask,]
  
  site[[river_id]] <- site[[site_riverid]]
  to_join <- site[, c("siteid", river_id)] %>%
    st_drop_geometry()
  
  
  river <- river %>%
    left_join(to_join, by = river_id)
  
  return(river)
}

target_extract_riveratlas_info  <- function(
  river_shp_files = NULL,
  snap_list = NULL,
  river_id = "riverid"
  ) {

  mask_na <- map_lgl(snap_list, ~all(is.na(.x)))
  river_shp_files <- river_shp_files[!mask_na]
  snap_c <- reduce(snap_list[!mask_na], rbind)

  out <- map_dfr(river_shp_files,
                 ~extract_riveratlas_info(shp_file = .x,
                                          site = snap_c)
  )
  
  stopifnot(sum(!is.na(snap_c[[river_id]])) == nrow(out))
  return(out)

}

extract_water_temperature_values <- function(
  raster_path= NULL,
  site = NULL
  ) {

  raster_path <- R.utils::filePath(raster_path, expandLinks = "any")

  r <- terra::rast(raster_path)
  terra::extract(r, st_coordinates(site))
}

target_extract_chelsa_data <- function(
  chelsa_shp_files = NULL,
  site = NULL
  ) {

  out <- map(chelsa_shp_files, 
    ~tibble(
    siteid = site$siteid,
    extract_water_temperature_values(raster_path = .x, site = site))
    ) %>%
    reduce(., left_join, by = "siteid")
  
  return(out)

}

get_full_riveratlas <- function(
  river_shp_files = NULL,
  river_id = "HYRIV_ID",
  var_to_collect = get_river_atlas_significant_var() 
  ) {

  out <- map_dfr(river_shp_files,
    function(shp, col_names) {

      river <- sf::read_sf(shp) %>%
        st_drop_geometry()

      river <- janitor::clean_names(river)
      

      river <- river[, tolower(col_names)]
      return(river)

    }, col_names = c(river_id, var_to_collect))

  return(out)
}


match_tedesco_basin_site <- function(
  site = NULL,
  basin = NULL,
  country_missing_fishbase = c(
  FIN = "Finland", SWE = "Sweden", USA = "United States",
  FRA = "France", GBR = "United Kingdom", ESP = "Spain",
  AUS = "Australia", BEL = "Belgium", BWA = "Botswana",
  NOR = "Norway", CAN = "Canada", JPN = "Japan"
  )

  ) {

  sf_use_s2(use_s2 = FALSE)
  within <- st_within(site, basin)

  exo_basin_site <- tibble(
    siteid = site$siteid,
    basin_name = map(within, ~basin[.x, ]$basin_name)
    ) %>%
    unnest(basin_name)

  # Missing sites, nearest basin
  m_sites <- site %>%
    filter(!siteid %in% exo_basin_site$siteid)

  mask_country <- !unique(m_sites$country) %in%
    names(country_missing_fishbase)

  if (any(mask_country)) {
    stop(paste0(
        "missing country not in country_missing_fishbase",
        paste0(
          unique(m_sites$country)[mask_country],
          collapse = ", ")
        )
    )
  }

  nested_basin_tedesco <- basin %>%
    filter(country %in% country_missing_fishbase) %>%
    group_by(country) %>%
    nest() %>%
    rename(basin = data)

  site_missing_tedesco <- m_sites %>%
    select(siteid, country) %>%
    mutate(country = country_missing_fishbase[country]) %>%
    group_by(country) %>%
    nest() %>%
    rename(site = data)

  site_basin <- site_missing_tedesco %>%
    left_join(nested_basin_tedesco, by = "country")

  dist_match <- function(x, y) {
    dist_matrix <- st_distance(x, y)
    tibble(
      siteid = y$siteid,
      basin_name = x$basin_name[apply(dist_matrix, 2, which.min)]
    )
  }
  exo_basin_m_site <- map2_dfr(
    site_basin$basin,
    site_basin$site,
    dist_match)

  site_basin <-
    rbind(exo_basin_site, exo_basin_m_site)
  stopifnot(nrow(site_basin) == length(unique(site_basin$siteid)))

  return(site_basin)
}

get_site_us_states <- function(site_desc_loc = NULL) {

  data(us_states, package = "spData")
  # Get abbreviations
  states_abb <- c(setNames(state.abb, state.name), "District of Columbia" = "DC")

  us_rft_site <- site_desc_loc %>%
    filter(country == "USA") %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(crs = st_crs(us_states)) %>%
    st_buffer(dist = .1)

  states_sites_mat <- st_intersects(us_rft_site, us_states)

  states_sites <- tibble(
    siteid = us_rft_site$siteid,
    region = purrr::map_chr(states_sites_mat, ~ifelse(length(.x) == 0, NA,
        us_states$NAME[.x])),
    region_abb = states_abb[region]
  )

  return(states_sites)

}

get_usgs_species_status <- function(
  meas_exo = measurement_exo %>%
    filter(native_exotic_origin != "tedesco", country == "USA"),
  usgs_data = occ_exotic_us,
  us_states_site = us_states_site
  ) {
  us_non_tedesco_states <- meas_exo %>%
  ## Add us states
  left_join(us_states_site %>% select(siteid, region_abb),
    by = "siteid") %>%
  distinct(fishbase_name, region_abb, native_exotic_status)
  stopifnot(any(!is.na(us_non_tedesco_states$region_abb)))

species_list_exo_us <- rfishbase::synonyms(
  species_list = unique(usgs_data$scientific_name)) %>%
  select(provided_name = synonym, fishbase_name = Species, comment = Status) %>%
  as_tibble() %>%
  filter(comment == "accepted name")

# Add fishbase name to USGS data
occ_exotic_us_fishbase <- usgs_data %>%
  rename(provided_name = scientific_name) %>%
  # Add species list
  left_join(
    species_list_exo_us %>%
      select(-comment),
    by = "provided_name"
    ) %>%
  select(provided_name, fishbase_name,
    region_abb = state_province,
    native_exotic_status_usgs)

    return(occ_exotic_us_fishbase)

}

add_usgs_data_to_measurement_exo <- function(
  meas_exo = NULL,
  us_states_site = NULL,
  occ_exotic_us_meas_exo = NULL
  ) {

  meas_exo %>%
    left_join(
      us_states_site %>% select(siteid, region_abb),
      by = "siteid") %>%
  left_join(occ_exotic_us_meas_exo %>%
    select(fishbase_name, region_abb, native_exotic_status_usgs),
  by = c("fishbase_name", "region_abb")) %>%
  mutate(
    native_exotic_origin = ifelse(
      country == "USA" & native_exotic_origin != "tedesco" &
        !is.na(native_exotic_status_usgs), "usgs",
      native_exotic_origin
      ),
    native_exotic_status = ifelse(
      country == "USA" & native_exotic_origin != "tedesco" &
        !is.na(native_exotic_status_usgs), native_exotic_status_usgs,
      native_exotic_status
      )) %>%
  select(-native_exotic_status_usgs, - region_abb)

}

test_spatial_autocorrelation_moran <- function(
  models = gaussian_int_env,
  model_data = modelling_data,
  loc = filtered_dataset$location) {

  sp_modelling_data  <- model_data %>%
    left_join(
      loc %>%
        select(siteid, longitude, latitude),
      by = "siteid"
    )

  sim_resid_mod <- expand.grid(
    list(response = models$response, year_nb = unique(model_data$year_nb))
    ) %>%
  as_tibble() %>%
  group_by(response) %>%
  nest() %>%
  mutate(
    resid_sim = list(
      DHARMa::simulateResiduals(
        models[models$response == response, ]$mod[[1]]
      )
    )
    )
    sim_resid_mod %>%
      unnest(cols = c(data)) %>%
      mutate(
        resid_year = map2(year_nb, resid_sim, function(d, r) {
          DHARMa::recalculateResiduals(r, sel = sp_modelling_data$year_nb == d)
      }),
    test_sp_cor = map(year_nb, resid_year, 
      ~DHARMa::testSpatialAutocorrelation(
        .y,
        x = sp_modelling_data[sp_modelling_data$year_nb == .x, ]$longitude,
        y = sp_modelling_data[sp_modelling_data$year_nb == .x, ]$latitude,
        plot = FALSE)
    )
      )

}

get_liming_site_swe <- function(loc = NULL, stream_limmed = NULL) {

  loc100 <- stream_limmed %>%
    st_buffer(dist = 100)

  within <- st_intersects(x = loc10, y = stream_limmed)

  lime_loc <- tibble(siteid = loc10$siteid,
    streamid = map(within, ~stream_limmed[.x, ]$objectid)) %>%
  unnest(streamid) %>%
  mutate(status_english = stream_limmed$status_english[streamid])

  return(lime_loc)
}
