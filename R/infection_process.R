#' @title Calculating the FOI
#'
#' @description calculating the FOI and infection process
#'
#' @param human, human
#' @param IMild rest of the infections, which we consider to be mild and not
#'  require hospitalisation
#' @param ICase1 First of the compartments for infections that will require
#'  hospitalisation
#' @param ICase2 Second of the compartments for infections that will require
#'  hospitalisation
#' @param S Susceptible
#' @param E1 First of the latent infection compartments
#' @param population_size population_size
#' @param beta_set beta_set
#' @param m m
#' @param dt dt
#' @importFrom stats runif
infection_process <- function(human, IMild, ICase1, ICase2,
                     S, E1, population_size, beta, m, dt) {
  function(api) {

    # Generating Force of Infection
    newImild <- api$get_state(human, IMild)
    newICase1 <- api$get_state(human, ICase1)
    newICase2 <- api$get_state(human, ICase2)

    get_3_states <- c(newImild, newICase1, newICase2)

    probability_of_infection <- 0

    bernoulli_multi_p <- function(size, p) runif(size, 0, 1) < p

    # If IMild = ICase1 = ICase2 = 0, FOI = 0, i.e. no infected individuals
    if (length(get_3_states) == 0) {
      probability_of_infection  <- 0.0
    }
    else {
      # Calculate FoI and use to create probability
      lambda <- beta * length(get_3_states) * mean(m)

      if (!isEmpty(lambda)) {
        probability_of_infection  <- 1 - exp(-lambda * dt)

        #Transition from S to E1
        susceptible <- api$get_state(human, S)

        temp <- bernoulli_multi_p(length(susceptible),
                                  probability_of_infection)

        validated_state_update(api, human, E1, temp, sum(population_size))

      }
      else {
        probability_of_infection  <- 0.0
      }
    }
  }
}

isEmpty <- function(x) {
  return(length(x) == 0)
}
