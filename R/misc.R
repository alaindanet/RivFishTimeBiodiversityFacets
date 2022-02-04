source_dir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "*.R")) {
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
