#' Span points on geometry
#'
#' @references https://github.com/r-spatial/sf/issues/792
#  https://stackoverflow.com/questions/51292952/snap-a-point-to-the-closest-point-on-a-line-segment-using-sf
st_snap_points <- function(
  x,
  y,
  max_dist = 1000,
  return_snapped_pt = FALSE) {

  if (inherits(x, "sf")) n <- nrow(x)
  if (inherits(x, "sfc")) n <- length(x)
  
  
  
  distance_tmp <- map(seq_len(n), ~st_nearest_points(x[.x,], y))
  
  near <- x %>%
    mutate(
      distance = distance_tmp,
      which_nearest = map(distance, get_nearest_pt),
      snapped_pt = pmap(
        list(distance, which_nearest, geometry), 
        function(d, nearest, origin){
          if (is.na(nearest)) {
            return(st_geometry(origin))
          }
          return(st_cast(distance[nearest, ], "POINT")[2])
        })
    )

  if(return_snapped_pt) {
    
    out <- do.call(
      c,
      lapply(seq(n), function(i) {
        if (is.na(near$which_nearest[i])) {
          return(st_geometry(x)[i])
        }
        return(st_cast(near$distance[i][near$which_nearest[i], ], "POINT")[2])
    })
    )

    return(out)
  } else {
    return(near %>% select(which, snapped_pt))
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
  col_id_river = "FID",
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

  if (!return_snapped_pt) {
    
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
    return(return_snapped_pt)
  }


}

get_nearest_pt <- function(snapped_pt = NULL) {
  
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
#' @param proj_crs integer default to 54032 because it keeps the distance well globally
#' https://learn.arcgis.com/en/projects/choose-the-right-projection/
#' https://epsg.io/54032
target_snap_site_to_river <- function(
  river_shp_filepath = NULL,
  site_sf = NULL,
  proj_crs = 54032,
  length_chunk = 200
) {
  
  # load shapefile
  layer_name <- sf::st_layers(river_shp_filepath, do_count = TRUE)$name
  river <- sf::read_sf(river_shp_filepath, query = paste0("SELECT FID FROM ", layer_name)) %>%
    st_transform(crs = proj_crs) 
  # crop shapefile
  crop_site <- site_sf %>% 
    st_transform(crs = proj_crs) %>%
    st_crop(st_bbox(river)) %>%
    arrange(main_bas)
  river_crop <- river %>%
    st_crop(st_bbox(crop_site))
  
  # cut the task by n times length_chunck
  ind <- seq_len(nrow(crop_site))
  nb_points_chuncks <- length_chunk
  chuncks <- split(ind, ceiling(seq_along(ind)/nb_points_chuncks))
  
  # Run
  correspondance <- furrr::future_map_dfr(
    chuncks,
    ~match_river_site(
      river = river_crop,
      site = crop_site[.x,],
      max_dist = 2000,
      metric_crs = NULL,
      return_snapped_pt = FALSE,
      crop = TRUE,
      buffer_dist = NULL,
      col_id_river = "FID",
      site_id = "siteid"
    )
  )
  return(correspondance)
}

#'
#' Works with windows shortcuts
get_shp_files <- function(dir = here("inst", "extdata", "RiverATLAS_v10_shp")) {
  dir %>%
    R.utils::filePath(., expandLinks = "any") %>%
    list.files(., full.names = TRUE) %>%
    .[stringr::str_detect(., "\\.shp")]
  
}