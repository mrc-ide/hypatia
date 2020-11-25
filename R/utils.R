`%||%` <- function(a, b) { # nolint
  if (is.null(a)) b else a
}

#' @noRd
remove_non_numerics <- function(l) {
  clean <- list()
  for (key in names(l)) {
    if (class(l[[key]]) %in% c("numeric", "integer", "array")) {
      clean[[key]] <- l[[key]]
    }
  }
  clean
}
