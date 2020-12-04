#' #' @title Get parameters from squire model
#' #'
#' #' @param iso3c three letter code for your country of interest
#' #' @param time_period for the simulation
#' #' @param ... Other parameters for [squire::parameters_explicit_SEEIR]
#' #'
#' #' @return squire model parameters
#' #' @export
#' get_parameters <- function(iso3c, time_period = 365, ...) {
#'   c(
#'     squire::parameters_explicit_SEEIR(
#'       country = get_country(iso3c),
#'       # population = get_population(iso3c),
#'       # contact_matrix_set = squire::contact_matrices[[1]],
#'       dt = 1,
#'       time_period = time_period,
#'
#'       ...
#'     ),
#'     time_period = time_period
#'   )
#' }

#' @title Get parameters from squire model
#'
#' @inheritParams squire::parameters_explicit_SEEIR
#' @param ... Other parameters for [squire::parameters_explicit_SEEIR]
#'
#' @return squire model parameters
#' @export
get_parameters <- function(iso3c,
                           population = NULL,
                           contact_matrix_set = NULL,
                           time_period = 365,
                           ...) {

  # dt should always be 1 as individual is always discrete time
  dt <- 1

  c(
    squire::parameters_explicit_SEEIR(
      population = population,
      country = get_country(iso3c),
      contact_matrix_set = contact_matrix_set,
      dt = dt,
      time_period = time_period,
      ...
    ),
    time_period = time_period
  )
}

#' @title Get population from SQUIRE model
#' @description rounds population sizes to discrete numbers
#'
#' @param iso3c three letter code for your country of interest
get_population <- function(iso3c) {
  squire::get_population(iso3c = iso3c, simple_SEIR = FALSE)
}

#' @noRd
get_country <- function(iso3c) {
  squire::population[squire::population$iso3c == iso3c, "country"][[1]]
}
