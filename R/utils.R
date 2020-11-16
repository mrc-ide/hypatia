`%||%` <- function(a, b) { # nolint
  if (is.null(a)) b else a
}

#' @importFrom stats rbinom 
bernoulli <- function(size, p) sample.int(size, rbinom(1, size, min(p, 1)))

#' @importFrom stats runif
bernoulli_multi_p <- function(size, p) runif(size, 0, 1) < p

#' Erlang waiting time distribution
#' 
#' @details Random draws from erlang distribution
#' @param size Number of draws
#' @param mu Mean duration
#' @importFrom stats rgamma
r_erlang <- function(size, mu) { rgamma(size, shape = 2, rate = 2/mu) }

#' Exponential waiting time distribution
#' 
#' @details Random draws from exponential distribution
#' @param size Number of draws
#' @param mu Mean duration
#' @importFrom stats rexp
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