#' @title Get parameters from SQUIRE model
#'
#' @param pop population. See [squire::get_population]
#' @param dt the timestep (days) for the simulation
#' @param time_period the number timesteps to run the simulation for
#' @param ... Other parameters for [squire::parameters_explicit_SEEIR]
#'
#' @return SQUIRE parameters
#' @export
get_parameters <- function(pop, dt = 1, time_period = 365, ...) {
  c(
    squire::parameters_explicit_SEEIR(
      population = pop$n,
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
#' @param countryname name of country
#' @return population
#' @export
get_population <- function(countryname) {
  squire::get_population(countryname, simple_SEIR = FALSE)
}
