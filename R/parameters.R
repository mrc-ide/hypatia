#' @title Get parameters from squire model
#'
#' @param iso3c three letter code for your country of interest
#' @param time_period for the simulation
#' @param ... Other parameters for [squire::parameters_explicit_SEEIR]
#'
#' @return squire model parameters
#' @export
get_parameters <- function(iso3c, time_period = 365, ...) {
  c(
    squire::parameters_explicit_SEEIR(
      country = get_country(iso3c),
      dt = 1, # should always be 1
      time_period = time_period,
      ...
    ),
    time_period = time_period
  )
}

#' @title Get population from SQUIRE model
#' @description rounds population sizes to discrete numbers
#'
#' @param countryname name of country
get_population <- function(iso3c) {
  squire::get_population(iso3c = iso3c, simple_SEIR = FALSE)
}

#' @noRd
get_country <- function(iso3c) {
  squire::population[squire::population$iso3c == 'BHS', 'country'][[1]]
}
