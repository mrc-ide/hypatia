#' @title Get parameters from squire model
#'
#' @inheritParams squire::parameters_explicit_SEEIR
#' @param iso3c Character for country iso3c
#' @param max_age the maximum age for humans
#' @param ... Other parameters for [squire::parameters_explicit_SEEIR]
#'
#' @return squire model parameters
#' @export
get_parameters <- function(iso3c = NULL,
                           population = NULL,
                           contact_matrix_set = NULL,
                           time_period = 365,
                           max_age = 100,
                           ...) {

  # if missing a contact matrix but have iso3c then use that
  if (!is.null(iso3c) && is.null(contact_matrix_set)) {
    contact_matrix_set <- squire::get_mixing_matrix(iso3c = iso3c)
  }

  # Get squire parameters
  squire_parameters <- squire::parameters_explicit_SEEIR(
        population = population,
        country = get_country(iso3c),
        contact_matrix_set = contact_matrix_set,
        dt = 1, # dt should always be 1 as individual is always a discrete time
        time_period = time_period,
        ...)

  squire_parameters$contact_matrix_set <- NULL # remove contact_matrix_set

  asymp_list <- get_asymptomatic() # create list of asymptomatic parameters

  # create list with hypatia parameters
  newlist <- list(time_period = time_period, max_age = max_age)

  # Then add all 3 lists together into pars, the list to be returned
  pars <- squire_parameters

  pars <- append(pars, asymp_list)

  pars <- append(pars, newlist)

  pars <- remove_non_numerics(pars) # remove non numerics

  pars

}

#' @title Get population from SQUIRE model
#' @description rounds population sizes to discrete numbers
#'
#' @param iso3c three letter code for your country of interest
#' @export
get_population <- function(iso3c) {
  squire::get_population(iso3c = iso3c, simple_SEIR = FALSE)
}

#' @noRd
get_country <- function(iso3c) {
  squire::population[squire::population$iso3c == iso3c, "country"][[1]]
}

#' Function to add asymptomatic information
#'
#' @return list of asymptomatic parameters
get_asymptomatic <- function()
{

  # Temporary values just for testing purposes. Literature derived values
  # will be supplied later
  prob_asymp <- c(0.3, 0.3, rep(0.2, 15))
  IAsymp_0 <- c(rep(0L, 17))
  dur_IAsymp <- 2.1

  list(
    prob_asymp = prob_asymp,
    IAsymp_0 = IAsymp_0,
    dur_IAsymp = dur_IAsymp
  )

}
