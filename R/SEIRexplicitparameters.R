#' Calculating the FOI
#'
#' @param human, human
#' @param IMild rest of the infections, which we consider to be mild and not
#'  require hospitalisation
#' @param ICase1 First of the compartments for infections that will require
#'  hospitalisation
#' @param ICase2 Second of the compartments for infections that will require
#'  hospitalisation
#' @param cum_hosp_inc cumulative hosptal incidences
#' @param i index
#' @param N_age number of points for a particular age
#' @param lambda FOI
#' @param problambda probability of
#' @param beta beta
#' @param m m
#' @param dt dt
#'
#' @export
#'
#' @examples
#' SEIRexplicitparameters(human, IMild, ICase1, ICase2, cum_hosp_inc, i, N_age,
#' lambda, problambda, beta, m, dt)
SEIRexplicitparameters <- function(human, IMild, ICase1, ICase2, cum_hosp_inc,
                                   i, N_age, lambda, problambda, beta, m, dt) {
  function(api) {

    # Generating Force of Infection
    newImild <- api$get_state(human, IMild)
    newICase1 <- api$get_state(human, ICase1)
    newICase2 <- api$get_state(human, ICase2)

    temp <- c(newImild, newICase1, newICase2)

    if (length(temp) == 0) {
      problambda <- 0
    }
    else {

      lambda <- beta * length(temp) * mean(m)
      if (!isEmpty(lambda) && lambda != numeric(0) && !is.na(lambda) == FALSE){
        problambda <- 1 - exp(-lambda * dt)
      }
      else{
        problambda <- 0
      }

    }
  }
}

isEmpty <- function(x) {
  return(length(x)==0)
}

parse_country_population_mixing_matrix <- function(country = NULL,
                                                   population = NULL,
                                                   contact_matrix_set = NULL) {

  # Handle country population args
  if (is.null(country) &&
      (is.null(population) || is.null(contact_matrix_set))) {
    stop("User must provide either the country being simulated or
         both the population size and contact_matrix_set")
  }

  # If a country was provided then grab the population and matrices if needed
  if (is.null(population)) {
    population <- squire::get_population(country)

    if (is.null(contact_matrix_set)) {
      contact_matrix_set <- squire::get_mixing_matrix(country)
    }
    population <- population$n
  }

  ret <- list(population = population,
              country = country,
              contact_matrix_set = contact_matrix_set)

  return(ret)

}
