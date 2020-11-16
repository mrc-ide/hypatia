`%||%` <- function(a, b) { # nolint
  if (is.null(a)) b else a
}

#' @importFrom stats rbinom 
bernoulli <- function(size, p) sample.int(size, rbinom(1, size, min(p, 1)))

#' @importFrom stats runif
bernoulli_multi_p <- function(size, p) runif(size, 0, 1) < p

#' @importFrom stats rgamma
r_erlang <- function(size, mu) { rgamma(size, shape = 2, rate = 2/mu) }

#' @importFrom stats rgamma
r_exp <- function(size, mu) { rexp(size, rate = 1/mu) }

#' @noRd
remove_non_numerics <- function(l) {
  classes <- unlist(lapply(l, class))
  return(l[classes %in% c("numeric", "integer", "array")])
}

#' @noRd
is_integer_like <- function(x) {
  abs(x - round(x)) < sqrt(.Machine$double.eps)
}

#' @noRd
isEmpty <- function(x) {
  return(length(x) == 0)
}