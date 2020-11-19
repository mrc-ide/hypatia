#' @title Get parameters from SQUIRE model
#'
#' @param countryname name of country
#' @param R0 R0
#' @param timestep tme step
#' @param dt dt
#' @param time_period time period
#' @param tt_contact_matrix contact matrix time
#' @param newpopulation population to use instead of actual one
#' @param numberof_days number of days for the run
#'
#' @return SQUIRE parameters
#' @export
#'
#' @examples
get_parameters <- function(countryname, R0, timestep, dt, time_period,
                           tt_contact_matrix, newpopulation = NULL, numberof_days) {

  pop <- squire::get_population(countryname, simple_SEIR = FALSE)

  psq <- squire::parameters_explicit_SEEIR(
    population = pop$n,
    dt = dt,
    R0 = R0,
    tt_contact_matrix = tt_contact_matrix,
    time_period = time_period,
    contact_matrix_set = squire::contact_matrices[[1]])

  psq

}
