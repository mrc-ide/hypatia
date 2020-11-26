#' @title Get parameters from squire model
#'
#' @inheritParams squire::parameters_explicit_SEEIR
#' @param ... Other parameters for [squire::parameters_explicit_SEEIR]
#'
#' @return squire model parameters
#' @export
get_parameters <- function(country = NULL,
                           population = NULL,
                           contact_matrix_set = NULL,
                           time_period = 365,
                           ...) {

  # dt should always be 1 as individual is always discrete time
  dt <- 1

  c(
    squire::parameters_explicit_SEEIR(
      population = population,
      country = country,
      contact_matrix_set = contact_matrix_set,
      dt = dt,
      time_period = time_period,
      ...
    ),
    time_period = time_period
  )
}

#' @title Get population from squire model
#' @description rounds population sizes to discrete numbers
#'
#' @param country name of country
#' @return population
#' @export
get_population <- function(country) {
  squire::get_population(country = country, simple_SEIR = FALSE)
}
