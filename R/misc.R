source_dir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = ".R")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

obj_mb <- function(x = NULL) {
  x <- object.size(x)
  class(x) <- "numeric"
  round(x * 10^-6)
}

get_tol_palettes <- function() {

  Tol_bright <- c("#EE6677", "#228833", "#4477AA", "#CCBB44", "#66CCEE",
    "#AA3377", "#BBBBBB"
  )

  Tol_muted <- c(
    "#88CCEE", "#44AA99", "#117733", "#332288", "#DDCC77",
    "#999933", "#CC6677", "#882255", "#AA4499", "#DDDDDD"
  )

  Tol_light <- c("#BBCC33", "#AAAA00", "#77AADD", "#EE8866", "#EEDD88", "#FFAABB",
    "#99DDFF", "#44BB99", "#DDDDDD"
  )

  #From Color Universal Design (CUD): https://jfly.uni-koeln.de/color/
  Okabe_Ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
    "#CC79A7", "#000000")

  l <- list(Okabe_Ito, Tol_bright, Tol_bright, Tol_light)
  names(l) <- c("okabe_ito", "tol_bright", "tol_bright", "tol_light")

  return(l)
}
