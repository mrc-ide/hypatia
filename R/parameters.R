#' @title Get parameters from SQUIRE model
#'
#' @param pop population. See [squire::get_population]
#' @param R0 R0
#' @param time_period the number timesteps to run the simulation for
#' @param tt_contact_matrix tt_contact_matrix
#' @param contact_matrix_set contact_matrix_set
#' @param dt the timestep (days) for the simulation
#' @param ... Other parameters for [squire::parameters_explicit_SEEIR]
#'
#' @return SQUIRE parameters
#' @export
get_parameters <- function(pop, R0 = 2, time_period = 365,
                           tt_contact_matrix = 0,
                           contact_matrix_set = squire::contact_matrices[[1]],
                           dt = 1,  ...) {
  c(
    squire::parameters_explicit_SEEIR(
      population = pop$n,
      R0 = R0,
      time_period = time_period,
      tt_contact_matrix = tt_contact_matrix,
      contact_matrix_set = contact_matrix_set,
      dt = dt,
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
