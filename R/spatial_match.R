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

  out <- map_dfr(chelsa_shp_files,
    ~extract_water_temperature_values(shp_file = .x,
      site = site)

}
