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

  snapped_pt <- do.call(
    c,
    lapply(seq(n), function(i) {
      st_nearest_points(st_geometry(x)[i], y)
    })
  )

  which_y_nearest_x <- do.call(
    c,
    lapply(seq(n), function(i) {
      nrst_len <- st_length(snapped_pt[i])
      nrst_mn <- which.min(nrst_len)
      if (as.vector(nrst_len[nrst_mn]) > max_dist) {
        return(NA)
      }
      return(nrst_mn)
    })
  )

  if(return_snapped_pt) {
    out <- do.call(
      c,
      lapply(seq(n), function(i) {
        if (is.na(which_y_nearest_x[i])) {
          return(st_geometry(x)[i])
        }
        return(st_cast(snapped_pt[which_y_nearest_x[i], ], "POINT")[2])
    })
    )

    return(out)
  } else {
    return(which_y_nearest_x)
  }

}

match_river_site <- function(
  river = NULL,
  site = NULL,
  max_dist = 1000,
  metric_crs = 54008,
  return_snapped_pt = FALSE,
  crop = TRUE,
  buffer_dist = NULL,
  col_id_river = "FID",
  site_id = "siteid"
) {

  site <- sf::st_transform(site, crs = metric_crs)
  river <- sf::st_transform(river, crs = metric_crs)

  if (is.null(buffer_dist)) {
    buffer_dist <- max_dist
  }


  # remove uncessary rivers
  if (crop) {
    if (!col_id_river %in% colnames(river)) {
      river[[col_id_river]] <- seq_len(nrow(river))
    }
    pre_river <- river
    river <- river %>%
      st_crop(st_bbox(st_buffer(site, dist = buffer_dist)))
  }

  nearest_snapped_pt <- st_snap_points2(
    site,
    river,
    max_dist = max_dist,
    return_snapped_pt = return_snapped_pt
  )

  if (!return_snapped_pt) {

    mask <- pre_river[["col_id_river"]] %in% nearest_snapped_pt
    out <- pre_river[mask, ]
    out$siteid <- site[[site_id]]
    return(out)
  } else {
    return(return_snapped_pt)
  }


}
