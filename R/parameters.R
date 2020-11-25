#' @title Get parameters from SQUIRE model
#'
#' @param countryname name of country
#' @param R0 R0
#' @param dt dt
#' @param time_period time period
#' @param tt_contact_matrix contact matrix time
#' @param contact_matrix_set contact_matrix_set
#'
#' @return SQUIRE parameters
#' @export
get_parameters <- function(countryname, R0, dt, time_period,
                           tt_contact_matrix, contact_matrix_set) {

  pop <- squire::get_population(countryname, simple_SEIR = FALSE)

  psq <- squire::parameters_explicit_SEEIR(
    population = pop$n,
    dt = dt,
    R0 = R0,
    tt_contact_matrix = tt_contact_matrix,
    time_period = time_period,
    contact_matrix_set = contact_matrix_set)

  psq

}